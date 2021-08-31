"""
    struct JETAnalyzer <: AbstractAnalyzer

JET.jl's default error analyzer.
"""
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API
# ====================

# NOTE `@aggressive_constprop` here makes sure `mode` to be propagated as constant
@aggressive_constprop @jetconfigurable function JETAnalyzer(;
    report_pass::Union{Nothing,T} = nothing,
    mode::Symbol                  = :basic,
    # default `InferenceParams` tuning
    aggressive_constant_propagation::Bool = true,
    unoptimize_throw_blocks::Bool         = false,
    # default `OptimizationParams` tuning
    inlining::Bool = false,
    jetconfigs...) where {T<:ReportPass}
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        report_pass = mode === :basic ? BasicPass() :
                      mode === :sound ? SoundPass() :
                      throw(ArgumentError("`mode` configuration should be either of `:basic` or `:sound`"))
    end
    # NOTE we always disable inlining, because:
    # - our current strategy to find undefined local variables and uncaught `throw` calls assumes un-inlined frames
    # - the cost for inlining isn't necessary for JETAnalyzer
    @assert !inlining "inlining should be disabled for JETAnalyzer"
    state = AnalyzerState(; aggressive_constant_propagation,
                            unoptimize_throw_blocks,
                            inlining,
                            jetconfigs...)
    return JETAnalyzer(report_pass, state)
end
AnalyzerState(analyzer::JETAnalyzer) =
    return analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) =
    return JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) =
    return analyzer.report_pass

# TODO document the definitions of errors, elaborate the difference of these two passes

"""
    SoundPass <: ReportPass

`ReportPass` for the sound `JETAnalyzer`'s error analysis.
"""
struct SoundPass <: ReportPass end

"""
    BasicPass <: ReportPass

`ReportPass` for the basic (default) `JETAnalyzer`'s error analysis.
"""
struct BasicPass <: ReportPass end

# `SoundPass` is still WIP, we will use it to implement both passes at once for the meantime
const SoundBasicPass = Union{SoundPass,BasicPass}

# analysis
# ========

function InferenceState(result::InferenceResult, cached::Bool, analyzer::JETAnalyzer)
    frame = @invoke InferenceState(result::InferenceResult, cached::Bool, analyzer::AbstractAnalyzer)
    if isnothing(frame) # indicates something bad happened within `retrieve_code_info`
        ReportPass(analyzer)(GeneratorErrorReport, analyzer, result)
    end
    return frame
end

@reportdef struct GeneratorErrorReport <: InferenceErrorReport
    @nospecialize(err) # actual error wrapped
end
get_msg(::Type{GeneratorErrorReport}, analyzer::JETAnalyzer, linfo::MethodInstance, @nospecialize(err)) =
    return sprint(showerror, err)

# XXX what's the "soundness" of a `@generated` function ?
# adapated from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::SoundBasicPass)(::Type{GeneratorErrorReport}, analyzer::JETAnalyzer, result::InferenceResult)
    mi = result.linfo
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(analyzer, linfo) XXX doesn't work
        may_invoke_generator(mi) || return
        try
            ccall(:jl_code_for_staged, Any, (Any,), mi) # invoke the "errorneous" generator again
        catch err
            # if user code throws error, wrap and report it
            report = add_new_report!(result, GeneratorErrorReport(analyzer, mi, err))
            # we will return back to the caller immediately
            add_caller_cache!(analyzer, report)
        end
    end
end

# TODO disable optimization for better performance, only do necessary analysis work by ourselves

function CC.finish!(analyzer::JETAnalyzer, frame::InferenceState)
    src = @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)

    if isnothing(src)
        # caught in cycle, similar error should have been reported where the source is available
        return src
    else
        code = (src::CodeInfo).code
        # report pass for (local) undef var error
        ReportPass(analyzer)(LocalUndefVarErrorReport, analyzer, frame, code)

        if frame.bestguess === Bottom
            # report pass for uncaught `throw` calls
            ReportPass(analyzer)(UncaughtExceptionReport, frame, code)
        else
            # the non-`Bottom` result may mean `throw` calls from the children frames
            # (if exists) are caught and not propagated here;
            # we don't want to cache `UncaughtExceptionReport`s for those calls for this frame
            # and its parents, and just filter them away now
            filter!(report->!isa(report, UncaughtExceptionReport), get_reports(frame.result))
        end

        return src
    end
end

@reportdef struct LocalUndefVarErrorReport <: InferenceErrorReport
    name::Symbol
end
get_msg(T::Type{LocalUndefVarErrorReport}, analyzer::JETAnalyzer, state, name::Symbol) =
    return "local variable $(name) is not defined"
print_error_report(io, report::LocalUndefVarErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

# these report passes use `:throw_undef_if_not` and `:(unreachable)` introduced by the native
# optimization pass, and thus supposed to only work on post-optimization code
(::SoundPass)(::Type{LocalUndefVarErrorReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, false)
(::BasicPass)(::Type{LocalUndefVarErrorReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, true)

function report_undefined_local_slots!(analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}, unsound::Bool)
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym = stmt.args[1]::Symbol

            # slots in toplevel frame may be a abstract global slot
            istoplevel(frame) && is_global_slot(analyzer, sym) && continue

            if unsound
                next_idx = idx + 1
                if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                    # the optimization so far has found this statement is never "reachable";
                    # JET reports it since it will invoke undef var error at runtime, or will just
                    # be dead code otherwise
                    add_new_report!(frame.result, LocalUndefVarErrorReport(analyzer, (frame, idx), sym))
                else
                    # by excluding this pass, this analysis accepts some false negatives and
                    # some undefined variable error may happen in actual execution (thus unsound)
                end
            else
                add_new_report!(frame.result, LocalUndefVarErrorReport(analyzer, (frame, idx), sym))
            end
        end
    end
end

is_unreachable(@nospecialize(x)) = isa(x, ReturnNode) && !isdefined(x, :val)

"""
    UncaughtExceptionReport <: InferenceErrorReport

Represents general `throw` calls traced during inference.
This is reported only when it's not caught by control flow.
"""
@reportdef struct UncaughtExceptionReport <: InferenceErrorReport
    throw_calls::Vector{Tuple{Int,Expr}} # (pc, call)
end
function UncaughtExceptionReport(sv::InferenceState, throw_calls::Vector{Tuple{Int,Expr}})
    vf = get_virtual_frame(sv.linfo)
    msg = length(throw_calls) == 1 ? "may throw" : "may throw either of"
    sig = Any[]
    ncalls = length(throw_calls)
    for (i, (pc, call)) in enumerate(throw_calls)
        call_sig = _get_sig((sv, pc), call)
        append!(sig, call_sig)
        i ≠ ncalls && push!(sig, ", ")
    end
    return UncaughtExceptionReport([vf], msg, sig, throw_calls)
end

# report `throw` calls "appropriately"
# this error report pass is really special, and requires special care when overloaded
# since it is tightly bound to that of `SeriousExceptionReport`
function (::SoundBasicPass)(::Type{UncaughtExceptionReport}, frame::InferenceState, stmts::Vector{Any})
    @assert frame.bestguess === Bottom

    # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
    # `throw` calls
    # XXX it's possible that the `throw` calls within them are all caught but the other
    # critical errors still make the return type `Bottom`
    # NOTE to reduce the false positive cases described above, we count `throw` calls
    # after optimization, since it may have eliminated "unreachable" `throw` calls
    codelocs = frame.src.codelocs
    linetable = frame.src.linetable::Vector
    reported_locs = nothing
    for report in get_reports(frame.result)
        if isa(report, SeriousExceptionReport)
            if isnothing(reported_locs)
                reported_locs = LineInfoNode[]
            end
            push!(reported_locs, report.loc)
        end
    end
    throw_calls = nothing
    for (pc, stmt) in enumerate(stmts)
        isa(stmt, Expr) || continue
        is_throw_call(stmt) || continue
        # if this `throw` is already reported, don't duplciate
        if !isnothing(reported_locs) && linetable[codelocs[pc]]::LineInfoNode in reported_locs
            continue
        end
        if isnothing(throw_calls)
            throw_calls = Tuple{Int,Expr}[]
        end
        push!(throw_calls, (pc, stmt))
    end
    if !isnothing(throw_calls) && !isempty(throw_calls)
        add_new_report!(frame.result, UncaughtExceptionReport(frame, throw_calls))
    end
end

@reportdef struct NoMethodErrorReport <: InferenceErrorReport
    @nospecialize(t::Union{Type,Vector{Type}})
end
get_msg(::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t::Type)) =
    "no matching method found for call signature ($t)"
get_msg(::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, ts::Vector{Type}) =
    "for $(length(ts)) of union split cases, no matching method found for call signatures ($(join(ts, ", "))))"

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

# TODO report "too many method matched"
function CC.abstract_call_gf_by_type(analyzer::JETAnalyzer, @nospecialize(f),
                                     fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                     sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer, @nospecialize(f),
                                              fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                              sv::InferenceState, max_methods::Int)

    info = ret.info
    if isa(info, ConstCallInfo)
        info = info.call # unwrap to `MethodMatchInfo` or `UnionSplitInfo`
    end
    # report passes for no matching methods error
    if isa(info, UnionSplitInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, argtypes)
    elseif isa(info, MethodMatchInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, atype)
    end

    return ret
end

function (::SoundBasicPass)(::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, info::UnionSplitInfo, argtypes::Vector{Any})
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
        add_new_report!(sv.result, NoMethodErrorReport(analyzer, sv, ts))
    end
end
function (::SoundBasicPass)(::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, info::MethodMatchInfo, @nospecialize(atype))
    if is_empty_match(info)
        add_new_report!(sv.result, NoMethodErrorReport(analyzer, sv, atype))
    end
end

@doc """
    bail_out_call(analyzer::JETAnalyzer, ...)

With this overload, `abstract_call_gf_by_type(analyzer::JETAnalyzer, ...)` doesn't bail
out inference even after the current return type grows up to `Any` and collects as much
error points as possible.
Of course this slows down inference performance, but hoopefully it stays to be "practical"
speed since the number of matching methods are limited beforehand.
"""
CC.bail_out_call(analyzer::JETAnalyzer, @nospecialize(t), sv) = false

@doc """
    add_call_backedges!(analyzer::JETAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::JETAnalyzer, ...)`, which always add
backedges (even if a new method can't refine the return type grew up to `Any`).
This is because a new method definition always has a potential to change `JETAnalyzer`'s analysis result.
"""
:(add_call_backedges!)
# branch on https://github.com/JuliaLang/julia/pull/41633
@static if isdefined(CC, :find_matching_methods)
import .CC:
    MethodMatches,
    UnionSplitMethodMatches
function CC.add_call_backedges!(analyzer::JETAnalyzer, @nospecialize(rettype), edges::Vector{MethodInstance},
                                matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
                                sv::InferenceState)
    # NOTE a new method may refine analysis, so we always add backedges
    # # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine (widen) this type
    # rettype === Any && return
    # for edge in edges
    #     add_backedge!(edge, sv)
    # end
    # also need an edge to the method table in case something gets
    # added that did not intersect with any existing method
    if isa(matches, MethodMatches)
        matches.fullmatch || add_mt_backedge!(matches.mt, atype, sv)
    else
        for (thisfullmatch, mt) in zip(matches.fullmatches, matches.mts)
            thisfullmatch || add_mt_backedge!(mt, atype, sv)
        end
    end
end
else # @static if isdefined(CC, :find_matching_methods)
function CC.add_call_backedges!(analyzer::JETAnalyzer, @nospecialize(rettype), edges::Vector{MethodInstance},
                                fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                                sv::InferenceState)
    # NOTE a new method may refine analysis, so we always add backedges
    # if rettype === Any
    #     # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine
    #     # (widen) this type
    #     return
    # end
    for edge in edges
        add_backedge!(edge, sv)
    end
    for (thisfullmatch, mt) in zip(fullmatch, mts)
        if !thisfullmatch
            # also need an edge to the method table in case something gets
            # added that did not intersect with any existing method
            add_mt_backedge!(mt, atype, sv)
        end
    end
end
end # @static if isdefined(CC, :find_matching_methods)

# this overload isn't necessary after https://github.com/JuliaLang/julia/pull/41882

@doc """
    const_prop_entry_heuristic(analyzer::JETAnalyzer, result::MethodCallResult, sv::InferenceState)

This overload for `abstract_call_method_with_const_args(analyzer::JETAnalyzer, ...)` forces
constant prop' even if an inference result can't be improved anymore _with respect to the
return type_, e.g. when `result.rt` is already `Const`.
Especially, this overload implements an heuristic to force constant prop' when any error points
have been reported while the previous abstract method call without constant arguments.
The reason we want much more aggressive constant propagation by that heuristic is that it's
highly possible constant prop' can produce more accurate analysis result, by throwing away
false positive error reports by cutting off the unreachable control flow or detecting
must-reachable `throw` calls.
"""
CC.const_prop_entry_heuristic(::JETAnalyzer, result::MethodCallResult, sv::InferenceState) = true

function CC.return_type_tfunc(analyzer::JETAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    # report pass for invalid `Core.Compiler.return_type` call
    ReportPass(analyzer)(InvalidReturnTypeCall, analyzer, sv, argtypes)

    return @invoke CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
end

@reportdef struct InvalidReturnTypeCall <: InferenceErrorReport end
get_msg(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState) = "invalid `Core.Compiler.return_type` call"

function (::SoundBasicPass)(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    # here we just check if the call of `return_type` is valid or not by very simple analysis
    # we don't take (possible, but very unexpected) overloads into account here, just as
    # `NativeInterpreter`'s `return_type_tfunc` hard-codes its return type to `Type`
    if length(argtypes) ≠ 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        add_new_report!(sv.result, InvalidReturnTypeCall(analyzer, sv))
    end
end

function CC.abstract_invoke(analyzer::JETAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    ret = @invoke CC.abstract_invoke(analyzer::AbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)

    ReportPass(analyzer)(InvalidInvokeErrorReport, analyzer, sv, ret, argtypes)

    return ret
end

@reportdef struct InvalidInvokeErrorReport <: InferenceErrorReport
    argtypes::Vector{Any}
end
function get_msg(::Type{InvalidInvokeErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    fallback_msg = "invalid invoke" # mostly because of runtime unreachable

    ft = widenconst(argtype_by_index(argtypes, 2))
    ft === Bottom && return fallback_msg
    t = argtype_by_index(argtypes, 3)
    (types, isexact, isconcrete, istype) = instanceof_tfunc(t)
    if types === Bottom
        if isa(t, Const)
            type = typeof(t.val)
            return "argument type should be `Type`-object (given `$type`)"
        end
        return fallback_msg
    end

    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    @assert nargtype === Bottom
    return "actual argument type (`$argtype`) doesn't intersect with specified argument type (`$types`)"
end

function (::SoundBasicPass)(::Type{InvalidInvokeErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Vector{Any})
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            add_new_report!(sv.result, InvalidInvokeErrorReport(analyzer, sv, argtypes))
        end
    end
end

function CC.abstract_eval_special_value(analyzer::JETAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, e, vtypes::VarTable, sv::InferenceState)

    if isa(e, GlobalRef)
        mod, name = e.mod, e.name
        if !isdefined(mod, name)
            # report pass for undefined global reference
            ReportPass(analyzer)(GlobalUndefVarErrorReport, analyzer, sv, mod, name)

            # # NOTE `ret` at this point should be annotated as `Any` by `NativeInterpreter`,
            # # and we just pass it as is to collect as much error points as possible
            # # we can change it to `Bottom` to suppress any further abstract interpretation
            # # with this variable, but then we also need to make sure to invalidate the cache
            # # for the analysis on the future re-definition of this (currently) undefined binding
            # return Bottom
        end
    end

    return ret
end

@reportdef struct GlobalUndefVarErrorReport <: InferenceErrorReport
    mod::Module
    name::Symbol
end
get_msg(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    "variable $(mod).$(name) is not defined"

function (::SoundPass)(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(sv.result, GlobalUndefVarErrorReport(analyzer, sv, mod, name))
end

function (::BasicPass)(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    is_corecompiler_undefglobal(mod, name) && return
    add_new_report!(sv.result, GlobalUndefVarErrorReport(analyzer, sv, mod, name))
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

function CC.abstract_eval_value(analyzer::JETAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractAnalyzer, e, vtypes::VarTable, sv::InferenceState)

    # report non-boolean condition error
    stmt = get_stmt((sv, get_currpc(sv)))
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom
            ReportPass(analyzer)(NonBooleanCondErrorReport, analyzer, sv, t)
            # if this condition leads to an "non-boolean (t) used in boolean context" error,
            # we can turn it into Bottom and bail out early
            # TODO upstream this
            if typeintersect(Bool, t) !== Bool
                ret = Bottom
            end
        end
    end

    return ret
end

@reportdef struct NonBooleanCondErrorReport <: InferenceErrorReport
    @nospecialize(t::Union{Type,Vector{Type}})
end
get_msg(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t::Type)) =
    "non-boolean ($t) used in boolean context"
get_msg(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, ts::Vector{Type}) =
    "for $(length(ts)) of union split cases, non-boolean ($(join(ts, ", "))) used in boolean context"

function (::SoundPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if !(t ⊑ Bool)
                push!(ts, t)
            end
        end
        if !isempty(ts)
            add_new_report!(sv.result, NonBooleanCondErrorReport(analyzer, sv, ts))
        end
    else
        if !(t ⊑ Bool)
            add_new_report!(sv.result, NonBooleanCondErrorReport(analyzer, sv, t))
        end
    end
end

function (::BasicPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if typeintersect(Bool, t) !== Bool
                # TODO move this to abstractinterpretation.jl
                if JETAnalysisParams(analyzer).strict_condition_check ||
                   !(t <: Function || # !(::Function)
                     t === Missing || # ==(::Missing, ::Any), ==(::Any, ::Missing), ...
                     false)
                    push!(ts, t)
                end
            end
        end
        if !isempty(ts)
            add_new_report!(sv.result, NonBooleanCondErrorReport(analyzer, sv, ts))
        end
    else
        if typeintersect(Bool, t) !== Bool
            add_new_report!(sv.result, NonBooleanCondErrorReport(analyzer, sv, t))
        end
    end
end

function (::SoundBasicPass)(::Type{InvalidConstantRedefinition}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    add_new_report!(sv.result, InvalidConstantRedefinition(analyzer, sv, mod, name, prev_t, t))
end
function (::SoundBasicPass)(::Type{InvalidConstantDeclaration}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(sv.result, InvalidConstantDeclaration(analyzer, sv, mod, name))
end

# XXX tfunc implementations in Core.Compiler are really not enough to catch invalid calls
# TODO set up our own checks and enable sound analysis

function CC.builtin_tfunction(analyzer::JETAnalyzer, @nospecialize(f), argtypes::Array{Any,1},
                              sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::AbstractAnalyzer, f, argtypes::Array{Any,1},
                                       sv::InferenceState)

    if f === throw
        # here we only report a selection of "serious" exceptions, i.e. those that should be
        # reported even if they may be caught in actual execution;
        ReportPass(analyzer)(SeriousExceptionReport, analyzer, sv, argtypes)

        # other general `throw` calls will be reported within `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
        # only when they are not caught by control flow, which is judged by whether if the
        # final return type of `sv` is annotated as `Bottom` or not, thus early return now
        return ret
    end

    ReportPass(analyzer)(InvalidBuiltinCallErrorReport, analyzer, sv, f, argtypes, ret)

    return ret
end

"""
    SeriousExceptionReport <: InferenceErrorReport

Represents a "serious" error that is manually thrown by a `throw` call.
This is reported regardless of whether it's caught by control flow or not, as opposed to
[`UncaughtExceptionReport`](@ref).
"""
@reportdef struct SeriousExceptionReport <: InferenceErrorReport
    @nospecialize(err)
    # keeps the location where this exception is raised
    # this information will be used later when collecting `UncaughtExceptionReport`s
    # in order to avoid duplicated reports from the same `throw` call
    loc::LineInfoNode
end
get_msg(T::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, state, @nospecialize(err), loc::LineInfoNode) =
    string(first(split(sprint(showerror, err), '\n')))
print_error_report(io, report::SeriousExceptionReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

function (::SoundBasicPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            err = a.val
            if isa(err, UndefKeywordError)
                add_new_report!(sv.result, SeriousExceptionReport(analyzer, sv, err, get_lin((sv, get_currpc(sv)))))
            elseif isa(err, MethodError)
                # ignore https://github.com/JuliaLang/julia/blob/7409a1c007b7773544223f0e0a2d8aaee4a45172/base/boot.jl#L261
                if err.f !== Bottom
                    add_new_report!(sv.result, SeriousExceptionReport(analyzer, sv, err, get_lin((sv, get_currpc(sv)))))
                end
            end
        end
    end
end

"""
    InvalidBuiltinCallErrorReport

Represents errors caused by invalid builtin-function calls.
Technically they're defined as those error points that should be caught within
`Core.Compiler.builtin_tfunction`.
"""
abstract type InvalidBuiltinCallErrorReport <: InferenceErrorReport end

@reportdef struct NoFieldErrorReport <: InvalidBuiltinCallErrorReport
    @nospecialize(typ::Type)
    name::Symbol
end
get_msg(::Type{NoFieldErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(typ::Type), name::Symbol) =
    "type $(typ) has no field $(name)"
print_error_report(io, report::NoFieldErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

@reportdef struct DivideErrorReport <: InferenceErrorReport end
let s = sprint(showerror, DivideError())
    global get_msg(::Type{DivideErrorReport}, analyzer::JETAnalyzer, sv::InferenceState) = s
end
print_error_report(io, report::DivideErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

@reportdef struct UnimplementedBuiltinCallErrorReport <: InvalidBuiltinCallErrorReport
    argtypes::Vector{Any}
end
get_msg(::Type{UnimplementedBuiltinCallErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(args...)) =
    "invalid builtin function call"

# TODO we do need sound versions of these functions

# XXX: for general case, JET just relies on the (maybe too persmissive) return type
# from native tfuncs to report invalid builtin calls and probably there're lots of
# false negatives
function handle_unimplemented_builtins!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(ret))
    if ret === Bottom
        @assert !(f === throw) "`throw` calls shuold be handled by report passes of `SeriousExceptionReport` or `UncaughtExceptionReport`"

        add_new_report!(sv.result, UnimplementedBuiltinCallErrorReport(analyzer, sv, argtypes))
    end
end

# fallback for unimplemented tfuncs, eliminate me in the future
function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f::Builtin), argtypes::Vector{Any}, @nospecialize(ret))
    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, f::typeof(getfield), argtypes::Vector{Any}, @nospecialize(ret))
    if 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const) && (mod = obj.val; isa(mod, Module))
                    if !isdefined(mod, name)
                        # bypass for report pass for undefined global reference
                        ReportPass(analyzer)(GlobalUndefVarErrorReport, analyzer, sv, mod, name)
                        return
                    end
                elseif ret === Bottom
                    # general case when an error is detected by the native `getfield_tfunc`
                    typ = widenconst(obj)
                    add_new_report!(sv.result, NoFieldErrorReport(analyzer, sv, typ, name))
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, f::IntrinsicFunction, argtypes::Vector{Any}, @nospecialize(ret))
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
                    add_new_report!(sv.result, DivideErrorReport(analyzer, sv))
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end
