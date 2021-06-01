function report!(T::Type{<:InferenceErrorReport}, interp::JETInterpreter,  @nospecialize(spec_args...))
    push!(interp.reports, T(interp, spec_args...))
end
function report!(T::Type{UncaughtExceptionReport}, interp::JETInterpreter,  @nospecialize(spec_args...))
    push!(interp.uncaught_exceptions, T(interp, spec_args...))
end

@inline report_pass!(T::Type{<:InferenceErrorReport}, interp::JETInterpreter, linfo::Union{InferenceState,MethodInstance}, @nospecialize(spec_args...)) =
    (JETAnalysisParams(interp).report_pass)(T, interp, linfo, spec_args...)

"""
    ReportPass

An interface type for report passes of JET's analysis.
"""
abstract type ReportPass end

"""
    SoundPass <: ReportPass

`ReportPass` for the sound JET analysis.
"""
struct SoundPass <: ReportPass end

"""
    BasicPass <: ReportPass

`ReportPass` for the basic (default) JET analysis.
"""
struct BasicPass <: ReportPass end

# `SoundPass` is still WIP, we will use it to implement both passes at once for the meantime
const SoundBasicPass = Union{SoundPass,BasicPass}

# NOTE we would like to omit type declarations in `(::ReportPass)(::Type{<:InferenceErrorReport}, ...)`
# definitions below, so that an user-defined report pass can easily exclude some of them by
# overloading `(::CustomPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return`

# the fallback implementation does nothing, so that future additional report pass "declaration"
# doesn't require the existing report pass traits to implement corresponding a report pass
(::ReportPass)(T::Type{<:InferenceErrorReport}, interp, linfo, @nospecialize(spec_args...)) = return

# NoMethodErrorReport

function (::SoundBasicPass)(::Type{NoMethodErrorReport}, interp::JETInterpreter, sv::InferenceState, info, @nospecialize(atype), argtypes::Vector{Any})
    if isa(info, MethodMatchInfo)
        if is_empty_match(info)
            report!(NoMethodErrorReport, interp, sv, atype)
        end
    elseif isa(info, UnionSplitInfo)
        # check each match for union-split signature
        split_argtypes = nothing
        ts = nothing

        for (i, matchinfo) in enumerate(info.matches)
            if is_empty_match(matchinfo)
                isnothing(split_argtypes) && (split_argtypes = switchtupleunion(argtypes))
                isnothing(ts) && (ts = Type[])
                sig_n = argtypes_to_type(split_argtypes[i])
                push!(ts, sig_n)
            end
        end

        if !isnothing(ts)
            report!(NoMethodErrorReport, interp, sv, ts)
        end
    end
end
function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

# InvalidReturnTypeCall

function (::SoundBasicPass)(::Type{InvalidReturnTypeCall}, interp::JETInterpreter, sv::InferenceState, argtypes::Vector{Any})
    # here we just check if the call of `return_type` is valid or not by very simple analysis
    # we don't take (possible, but very unexpected) overloads into account here, just as
    # `NativeInterpreter`'s `return_type_tfunc` hard-codes its return type to `Type`
    if length(argtypes) ≠ 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        report!(InvalidReturnTypeCall, interp, sv)
    end
end

# GlobalUndefVarErrorReport

function (::SoundPass)(::Type{GlobalUndefVarErrorReport}, interp::JETInterpreter, sv::InferenceState, mod::Module, name::Symbol)
    report!(GlobalUndefVarErrorReport, interp, sv, mod, name)
end
function (::BasicPass)(::Type{GlobalUndefVarErrorReport}, interp::JETInterpreter, sv::InferenceState, mod::Module, name::Symbol)
    is_corecompiler_undefglobal(mod, name) && return
    report!(GlobalUndefVarErrorReport, interp, sv, mod, name)
end

"""
    is_corecompiler_undefglobal

Returns `true` if this global reference is undefined inside `Core.Compiler`, but the
  corresponding name exists in the `Base` module.
`Core.Compiler` reuses the minimum amount of `Base` code and there're some of missing
definitions, and `BasicPass` will exclude reports on those undefined names since they
usually don't matter and `Core.Compiler`'s basic functionality is battle-tested and validated
exhausively by its test suite and real-world usages
"""
is_corecompiler_undefglobal(mod::Module, name::Symbol) =
    return mod === CC ? isdefined(Base, name) :
           mod === CC.Sort ? isdefined(Base.Sort, name) :
           false

# LocalUndefVarErrorReport

# these report passes use `:throw_undef_if_not` and `:(unreachable)` introduced by the native
# optimization pass, and thus supposed to only work on post-optimization code
(::SoundPass)(::Type{LocalUndefVarErrorReport}, interp::JETInterpreter, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(interp, frame, stmts, false)
(::BasicPass)(::Type{LocalUndefVarErrorReport}, interp::JETInterpreter, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(interp, frame, stmts, true)

function report_undefined_local_slots!(interp::JETInterpreter, frame::InferenceState, stmts::Vector{Any}, unsound::Bool)
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym::Symbol, _ = stmt.args

            # slots in toplevel frame may be a abstract global slot
            istoplevel(interp, frame) && is_global_slot(interp, sym) && continue

            if unsound
                next_idx = idx + 1
                if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                    # the optimization so far has found this statement is never "reachable";
                    # JET reports it since it will invoke undef var error at runtime, or will just
                    # be dead code otherwise
                    report!(LocalUndefVarErrorReport, interp, frame, sym, idx)
                else
                    # by excluding this pass, this analysis accepts some false negatives and
                    # some undefined variable error may happen in actual execution (thus unsound)
                end
            else
                report!(LocalUndefVarErrorReport, interp, frame, sym, idx)
            end
        end
    end
end

# NonBooleanCondErrorReport

function (::SoundPass)(::Type{NonBooleanCondErrorReport}, interp::JETInterpreter, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if !(t ⊑ Bool)
                push!(ts, t)
            end
        end
        if !isempty(ts)
            report!(NonBooleanCondErrorReport, interp, sv, ts)
        end
    else
        if !(t ⊑ Bool)
            report!(NonBooleanCondErrorReport, interp, sv, t)
        end
    end
end
function (::BasicPass)(::Type{NonBooleanCondErrorReport}, interp::JETInterpreter, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if typeintersect(Bool, t) !== Bool
                # TODO move this to abstractinterpretation.jl
                if JETAnalysisParams(interp).strict_condition_check ||
                   !(t <: Function || # !(::Function)
                     t === Missing || # ==(::Missing, ::Any), ==(::Any, ::Missing), ...
                     false)
                    push!(ts, t)
                end
            end
        end
        if !isempty(ts)
            report!(NonBooleanCondErrorReport, interp, sv, ts)
        end
    else
        if typeintersect(Bool, t) !== Bool
            report!(NonBooleanCondErrorReport, interp, sv, t)
        end
    end
end

# InvalidConstantRedefinition

function (::SoundBasicPass)(::Type{InvalidConstantRedefinition}, interp::JETInterpreter, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    report!(InvalidConstantRedefinition, interp, sv, mod, name, prev_t, t)
end

# InvalidConstantDeclaration

function (::SoundBasicPass)(::Type{InvalidConstantDeclaration}, interp::JETInterpreter, sv::InferenceState, mod::Module, name::Symbol)
    report!(InvalidConstantDeclaration, interp, sv, mod, name)
end

# GeneratorErrorReport

# XXX what's the "soundness" of a `@generated` function ?
# adapated from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::SoundBasicPass)(::Type{GeneratorErrorReport}, interp::JETInterpreter, mi::MethodInstance)
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(interp, linfo) XXX doesn't work
        may_invoke_generator(mi) || return
        try
            ccall(:jl_code_for_staged, Any, (Any,), mi)
        catch err
            # if user code throws error, wrap and report it
            report!(GeneratorErrorReport, interp, mi, err)
        end
    end
end

# SeriousExceptionReport

function (::SoundBasicPass)(::Type{SeriousExceptionReport}, interp::JETInterpreter, sv::InferenceState, argtypes::Vector{Any})
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            v = a.val
            if isa(v, UndefKeywordError)
                report!(UndefKeywordErrorReport, interp, sv, v, get_lin(sv))
            end
        end
    end
end

# UncaughtExceptionReport

# this error report is really special, and might not be eligible for possible overloads since:
# - this report does not only report error points but also "clean up" caught error points
# - this pass is tightly bound to that of `SeriousExceptionReport`
function (::SoundBasicPass)(::Type{UncaughtExceptionReport}, interp::JETInterpreter, frame::InferenceState, stmts::Vector{Any})
    # report `throw` calls "appropriately"
    if get_result(frame) === Bottom
        # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
        # `throw` calls
        # XXX: well, it's possible that the `throw` calls within them are all caught but the
        # other critical errors make the return type `Bottom`
        # NOTE: to reduce the false positive `UncaughtExceptionReport`s described above, we count
        # `throw` calls here after optimization, since it may have eliminated "unreachable"
        # `throw` calls
        codelocs    = frame.src.codelocs
        linetable   = frame.src.linetable::Vector
        throw_locs  = LineInfoNode[]
        throw_calls = Expr[]
        for r in interp.reports
            if isa(r, SeriousExceptionReport) && last(r.vst).linfo === frame.linfo
                push!(throw_locs, r.lin)
            end
        end
        for (i, stmt) in enumerate(stmts)
            is_throw_call_expr(interp, frame, stmt) || continue
            # if this `throw` is already reported, don't duplciate
            linetable[codelocs[i]]::LineInfoNode in throw_locs && continue
            push!(throw_calls, stmt)
        end
        if !isempty(throw_calls)
            report!(UncaughtExceptionReport, interp, frame, throw_calls)
        end
    else
        # the non-`Bottom` result here may mean `throw` calls from the children frames
        # (if exists) are caught and not propagated here;
        # we don't want to cache `UncaughtExceptionReport`s for those calls for this frame
        # and its parents, so just filter them away
        empty!(interp.uncaught_exceptions)
    end
end

# InvalidBuiltinCallErrorReport

# TODO we do need sound versions of these functions

# XXX: for general case, JET just relies on the (maybe too persmissive) return type
# from native tfuncs to report invalid builtin calls and probably there're lots of
# false negatives
function handle_unimplemented_builtins!(interp::JETInterpreter, sv::InferenceState, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(ret))
    if ret === Bottom
        @assert !(f === throw) "`throw` calls shuold be handled by report passes of `SeriousExceptionReport` or `UncaughtExceptionReport`"

        report!(UnimplementedBuiltinCallErrorReport, interp, sv, argtypes)
    end
end

# fallback for unimplemented tfuncs, eliminate me in the future
function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, interp::JETInterpreter, sv::InferenceState, @nospecialize(f::Builtin), argtypes::Vector{Any}, @nospecialize(ret))
    handle_unimplemented_builtins!(interp, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, interp::JETInterpreter, sv::InferenceState, f::typeof(getfield), argtypes::Vector{Any}, @nospecialize(ret))
    if 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const) && (mod = obj.val; isa(mod, Module))
                    if !isdefined(mod, name)
                        # bypass for report pass for undefined global reference
                        report_pass!(GlobalUndefVarErrorReport, interp, sv, mod, name)
                        return
                    end
                elseif ret === Bottom
                    # general case when an error is detected by the native `getfield_tfunc`
                    typ = widenconst(obj)
                    report!(NoFieldErrorReport, interp, sv, typ, name)
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(interp, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, interp::JETInterpreter, sv::InferenceState, f::IntrinsicFunction, argtypes::Vector{Any}, @nospecialize(ret))
    # TODO this check might be better in its own report pass, say `NumericalPass`
    if length(argtypes) == 2
        if f === Intrinsics.checked_sdiv_int ||
           f === Intrinsics.checked_srem_int ||
           f === Intrinsics.checked_udiv_int ||
           f === Intrinsics.checked_urem_int ||
           f === Intrinsics.sdiv_int ||
           f === Intrinsics.srem_int ||
           f === Intrinsics.udiv_int ||
           f === Intrinsics.urem_int
            a = argtypes[2]
            t = widenconst(a)
            if isprimitivetype(t) && t <: Number
                if isa(a, Const) && a.val === zero(t)
                    report!(DivideErrorReport, interp, sv)
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(interp, sv, f, argtypes, ret)
end

# NativeRemark

# ignores `NativeRemark`s by default
(::SoundBasicPass)(::Type{NativeRemark}, interp::JETInterpreter, sv::InferenceState, s) = return

# InvalidInvokeErrorReport

function (::SoundBasicPass)(::Type{InvalidInvokeErrorReport}, interp::JETInterpreter, sv::InferenceState, ret::CallMeta, argtypes::Vector{Any})
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            report!(InvalidInvokeErrorReport, interp, sv, argtypes)
        end
    end
end
