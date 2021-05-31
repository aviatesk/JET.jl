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
This filter also provides the default implementation of `ReportPass`.
"""
struct SoundPass <: ReportPass end

"""
    BasicPass <: ReportPass

`ReportPass` for the sound JET analysis.
This filter also provides the default implementation of `ReportPass`.
"""
struct BasicPass <: ReportPass end

# NOTE we would like to omit type declarations in `(::ReportPass)(::Type{<:InferenceErrorReport}, ...)`
# definitions below, so that an user-defined report pass can easily exclude some of them by
# overloading `(::CustomPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return`

# the fallback implementation does nothing, so that future additional report pass "declaration"
# doesn't require the existing report pass traits to implement corresponding a report pass
(::ReportPass)(T::Type{<:InferenceErrorReport}, interp, linfo, @nospecialize(spec_args...)) = return

# NoMethodErrorReport

function (::ReportPass)(::Type{NoMethodErrorReport}, interp, sv, info, @nospecialize(atype), argtypes)
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

function (::ReportPass)(::Type{InvalidReturnTypeCall}, interp, sv, argtypes)
    # here we just check if the call of `return_type` is valid or not by very simple analysis
    # we don't take (possible, but very unexpected) overloads into account here, just as
    # `NativeInterpreter`'s `return_type_tfunc` hard-codes its return type to `Type`
    if length(argtypes) ≠ 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        report!(InvalidReturnTypeCall, interp, sv)
    end
end

# GlobalUndefVarErrorReport

function (::ReportPass)(::Type{GlobalUndefVarErrorReport}, interp, sv, mod, name)
    report!(GlobalUndefVarErrorReport, interp, sv, mod, name)
end
function (::BasicPass)(::Type{GlobalUndefVarErrorReport}, interp, sv, mod, name)
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
(::ReportPass)(::Type{LocalUndefVarErrorReport}, interp, sv, stmts) =
    report_undefined_local_slots!(interp, sv, stmts, false)
(::BasicPass)(::Type{LocalUndefVarErrorReport}, interp, sv, stmts) =
    report_undefined_local_slots!(interp, sv, stmts, true)

function report_undefined_local_slots!(interp, sv, stmts::Vector{Any}, unsound::Bool)
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

function (::ReportPass)(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t))
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
function (::BasicPass)(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t))
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

function (::ReportPass)(::Type{InvalidConstantRedefinition}, interp, sv, mod, name, @nospecialize(prev_t), @nospecialize(t))
    report!(InvalidConstantRedefinition, interp, sv, mod, name, prev_t, t)
end

# InvalidConstantDeclaration

function (::ReportPass)(::Type{InvalidConstantDeclaration}, interp, sv, mod, name)
    report!(InvalidConstantDeclaration, interp, sv, mod, name, prev_t, t)
end

# GeneratorErrorReport

# XXX what's the "soundness" of a `@generated` function ?
# adapated from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::ReportPass)(::Type{GeneratorErrorReport}, interp, mi)
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

function (::ReportPass)(::Type{SeriousExceptionReport}, interp, sv, argtypes)
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

# NativeRemark

# ignores `NativeRemark`s by default
(::ReportPass)(::Type{NativeRemark}, interp, sv, s) = return

# InvalidInvokeErrorReport

function (::ReportPass)(::Type{InvalidInvokeErrorReport}, interp, sv, rt, argtypes)
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            report!(InvalidInvokeErrorReport, interp, sv, argtypes)
        end
    end
end
