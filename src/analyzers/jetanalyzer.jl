"""
Every [entry point of error analysis](@ref jetanalysis-entry) can accept
any of [general JET configurations](@ref JET-configurations) as well as
the following additional configurations that are specific to the error analysis.

---
- `mode::Symbol = :basic`:\\
  Switches the error analysis pass. Each analysis pass reports errors according to their
  own "error" definition.
  JET by default offers the following modes:
  - `mode = :basic`: the default error analysis pass.
    This analysis pass is tuned to be useful for general Julia development by reporting common
    problems, but also note that it is not enough strict to guarantee that your program never
    throws runtime errors.\\
    See [`BasicPass`](@ref) for more details.
  - `mode = :sound`: the sound error analysis pass.
    If this pass doesn't report any errors, then your program is assured to run without
    any runtime errors (unless JET's error definition is not accurate and/or there is an
    implementation flaw).\\
    See [`SoundPass`](@ref) for more details.
  - `mode = :typo`: a typo detection pass
    A simple analysis pass to detect "typo"s in your program.
    This analysis pass is essentially a subset of the default basic pass ([`BasicPass`](@ref)),
    and it only reports undefined global reference and undefined field access.
    This might be useful especially for a very complex code base, because even the basic pass
    tends to be too noisy (spammed with too many errors) for such a case.\\
    See [`TypoPass`](@ref) for more details.

  !!! note
      You can also set up your own analysis using JET's [`AbstractAnalyzer`-Framework](@ref).
---
"""
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
    __cache_key::UInt
end

# AbstractAnalyzer API
# ====================

# NOTE `@constprop :aggressive` here makes sure `mode` to be propagated as constant
@constprop :aggressive @jetconfigurable function JETAnalyzer(;
    report_pass::Union{Nothing,ReportPass} = nothing,
    mode::Symbol                           = :basic,
    # default `InferenceParams` tuning
    aggressive_constant_propagation::Bool = true,
    unoptimize_throw_blocks::Bool         = false,
    # default `OptimizationParams` tuning
    inlining::Bool = false,
    jetconfigs...)
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        report_pass = mode === :basic ? BasicPass(; jetconfigs...) :
                      mode === :sound ? SoundPass() :
                      mode === :typo  ? TypoPass() :
                      throw(ArgumentError("`mode` configuration should be either of `:basic`, `:sound` or `:typo`"))
    end
    # NOTE we always disable inlining, because:
    # - our current strategy to find undefined local variables and uncaught `throw` calls assumes un-inlined frames
    # - the cost for inlining isn't necessary for JETAnalyzer
    inlining && throw(ArgumentError("inlining should be disabled for `JETAnalyzer`"))
    state = AnalyzerState(; aggressive_constant_propagation,
                            unoptimize_throw_blocks,
                            inlining,
                            jetconfigs...)
    cache_key = state.param_key
    cache_key = hash(report_pass, cache_key)
    return JETAnalyzer(report_pass, state, cache_key)
end
JETInterface.AnalyzerState(analyzer::JETAnalyzer) =
    return analyzer.state
JETInterface.AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) =
    return JETAnalyzer(ReportPass(analyzer), state, analyzer.__cache_key)
JETInterface.ReportPass(analyzer::JETAnalyzer) =
    return analyzer.report_pass
JETInterface.get_cache_key(analyzer::JETAnalyzer) =
    return analyzer.__cache_key

"""
The basic (default) error analysis pass.

_**TODO**_: elaborate the definitions of "error"s.
"""
struct BasicPass{FF} <: ReportPass
    function_filter::FF
end
function BasicPass(; function_filter = basic_function_filter,
                     __jetconfigs...)
    return BasicPass(function_filter)
end

function basic_function_filter(@nospecialize ft)
    ft === typeof(Base.mapreduce_empty) && return false
    ft === typeof(Base.reduce_empty) && return false
    return true
end

"""
The sound error analysis pass.

_**TODO**_: elaborate the definitions of "error"s.
"""
struct SoundPass <: ReportPass end

basic_filter(analyzer::JETAnalyzer, sv) =
    is_compileable_frame(sv) || get_entry(analyzer) === get_linfo(sv) # `report_call` may start analysis with abstract signature

# `SoundPass` is still WIP, we may use it to implement both passes at once for the meantime
const SoundBasicPass = Union{SoundPass,BasicPass}

"""
A typo detection pass.

_**TODO**_: elaborate the definitions of "error"s.
"""
struct TypoPass <: ReportPass end
(::TypoPass)(@nospecialize _...) = return false # ignore everything except GlobalUndefVarErrorReport and NoFieldErrorReport

# analysis
# ========

function CC.InferenceState(result::InferenceResult, cache::CACHE_ARG_TYPE, analyzer::JETAnalyzer)
    frame = @invoke CC.InferenceState(result::InferenceResult, cache::CACHE_ARG_TYPE, analyzer::AbstractAnalyzer)
    if isnothing(frame) # indicates something bad happened within `retrieve_code_info`
        ReportPass(analyzer)(GeneratorErrorReport, analyzer, result)
    end
    return frame
end

@reportdef struct GeneratorErrorReport <: InferenceErrorReport
    @nospecialize err # actual error wrapped
end
get_msg(::Type{GeneratorErrorReport}, linfo::MethodInstance, @nospecialize(err)) =
    return sprint(showerror, err)

# XXX what's the "soundness" of a `@generated` function ?
# adapated from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::SoundBasicPass)(::Type{GeneratorErrorReport}, analyzer::JETAnalyzer, result::InferenceResult)
    mi = result.linfo
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(analyzer, linfo) XXX doesn't work
        may_invoke_generator(mi) || return false
        try
            ccall(:jl_code_for_staged, Any, (Any,), mi) # invoke the "errorneous" generator again
        catch err
            # if user code throws error, wrap and report it
            report = add_new_report!(result, GeneratorErrorReport(mi, err))
            # we will return back to the caller immediately
            add_caller_cache!(analyzer, report)
            return true
        end
    end
    return false
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

        # report pass for uncaught `throw` calls
        ReportPass(analyzer)(UncaughtExceptionReport, frame, code)

        return src
    end
end

@reportdef struct LocalUndefVarErrorReport <: InferenceErrorReport
    name::Symbol
end
get_msg(T::Type{LocalUndefVarErrorReport}, state, name::Symbol) =
    return "local variable $(name) is not defined"
print_error_report(io, report::LocalUndefVarErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

# these report passes use `:throw_undef_if_not` and `:(unreachable)` introduced by the native
# optimization pass, and thus supposed to only work on post-optimization code
(::SoundPass)(::Type{LocalUndefVarErrorReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, false)
(::BasicPass)(::Type{LocalUndefVarErrorReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, true)

function report_undefined_local_slots!(analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}, unsound::Bool)
    local reported = false
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym = stmt.args[1]::Symbol
            # slots in toplevel frame may be a abstract global slot
            istoplevel(frame) && is_global_slot(analyzer, sym) && continue
            if unsound
                next_idx = idx + 1
                if checkbounds(Bool, stmts, next_idx) && is_unreachable(stmts[next_idx])
                    # the optimization so far has found this statement is never "reachable";
                    # JET reports it since it will invoke undef var error at runtime, or will just
                    # be dead code otherwise
                    add_new_report!(frame.result, LocalUndefVarErrorReport((frame, idx), sym))
                    reported |= true
                else
                    # by excluding this pass, this analysis accepts some false negatives and
                    # some undefined variable error may happen in actual execution (thus unsound)
                end
            else
                add_new_report!(frame.result, LocalUndefVarErrorReport((frame, idx), sym))
                reported |= true
            end
        end
    end
    return reported
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
    return UncaughtExceptionReport([vf], msg, Signature(sig), throw_calls)
end

# report `throw` calls "appropriately"
# this error report pass is very special, since 1.) it's tightly bound to the report pass of
# `SeriousExceptionReport` and 2.) it involves "report filtering" on its own
function (::BasicPass)(::Type{UncaughtExceptionReport}, frame::InferenceState, stmts::Vector{Any})
    if frame.bestguess === Bottom
        report_uncaught_exceptions!(frame, stmts)
        return true
    else
        # the non-`Bottom` result may mean `throw` calls from the children frames
        # (if exists) are caught and not propagated here
        # we don't want to cache the caught `UncaughtExceptionReport`s for this frame and
        # its parents, and just filter them away now
        filter!(report->!isa(report, UncaughtExceptionReport), get_reports(frame.result))
    end
    return false
end
(::SoundPass)(::Type{UncaughtExceptionReport}, frame::InferenceState, stmts::Vector{Any}) =
    report_uncaught_exceptions!(frame, stmts) # yes, you want tons of false positives !
function report_uncaught_exceptions!(frame::InferenceState, stmts::Vector{Any})
    # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
    # `throw` calls
    # XXX it's possible that the `throw` calls within them are all caught but the other
    # critical errors still make the return type `Bottom`
    # NOTE to reduce the false positive cases described above, we count `throw` calls
    # after optimization, since it may have eliminated "unreachable" `throw` calls
    codelocs = frame.src.codelocs
    linetable = frame.src.linetable::LineTable
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
        return true
    end
    return false
end

@reportdef struct NoMethodErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
end
function get_msg(::Type{NoMethodErrorReport}, sv::InferenceState, @nospecialize(t), union_split::Int)
    if union_split == 0
        "no matching method found for call signature ($t)"
    else
        ts = t::Vector{Any}
        nts = length(ts)
        tss = join(ts, ", ")
        "for $nts of $union_split union split cases, no matching method found for call signatures ($tss))"
    end
end

@static if IS_AFTER_42529
function CC.abstract_call_gf_by_type(analyzer::JETAnalyzer, @nospecialize(f),
                                     arginfo::ArgInfo, @nospecialize(atype),
                                     sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer, @nospecialize(f),
                                              arginfo::ArgInfo, @nospecialize(atype),
                                              sv::InferenceState, max_methods::Int)
    info = ret.info
    if isa(info, ConstCallInfo)
        info = info.call # unwrap to `MethodMatchInfo` or `UnionSplitInfo`
    end
    # report passes for no matching methods error
    if isa(info, UnionSplitInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, arginfo.argtypes, atype)
    elseif isa(info, MethodMatchInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, arginfo.argtypes, atype)
    end
    return ret
end
else # @static if IS_AFTER_42529
function CC.abstract_call_gf_by_type(analyzer::JETAnalyzer, @nospecialize(f),
                                     fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes, @nospecialize(atype),
                                     sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer, @nospecialize(f),
                                              fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes, @nospecialize(atype),
                                              sv::InferenceState, max_methods::Int)
    info = ret.info
    if isa(info, ConstCallInfo)
        info = info.call # unwrap to `MethodMatchInfo` or `UnionSplitInfo`
    end
    # report passes for no matching methods error
    if isa(info, UnionSplitInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, argtypes, atype)
    elseif isa(info, MethodMatchInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, argtypes, atype)
    end
    return ret
end
end # @static if IS_AFTER_42529

function (rp::BasicPass)(
    ::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    info::UnionSplitInfo, argtypes::Argtypes, @nospecialize(_))
    basic_filter(analyzer, sv) || return false
    ft = widenconst(first(argtypes))
    rp.function_filter(ft) || return false
    return report_no_method_error_for_union_split!(analyzer, sv, info, argtypes)
end
function (::SoundPass)(
    ::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    info::UnionSplitInfo, argtypes::Argtypes, @nospecialize(_))
    return report_no_method_error_for_union_split!(analyzer, sv, info, argtypes)
end
function report_no_method_error_for_union_split!(
    analyzer::JETAnalyzer, sv::InferenceState, info::UnionSplitInfo, argtypes::Argtypes)
    # check each match for union-split signature
    split_argtypes = nothing
    rinfo = nothing
    for (i, matchinfo) in enumerate(info.matches)
        if is_empty_match(matchinfo)
            isnothing(split_argtypes) && (split_argtypes = switchtupleunion(argtypes))
            if rinfo === nothing
                rinfo = Any[], length(info.matches)
            end
            sig_n = argtypes_to_type(split_argtypes[i]::Vector{Any})
            push!(rinfo[1], sig_n)
        end
    end
    if rinfo !== nothing
        add_new_report!(sv.result, NoMethodErrorReport(sv, rinfo...))
        return true
    end
    return false
end

function (rp::BasicPass)(
    ::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    info::MethodMatchInfo, argtypes::Argtypes, @nospecialize(atype))
    basic_filter(analyzer, sv) || return false
    ft = widenconst(first(argtypes))
    rp.function_filter(ft) || return false
    return report_no_method_error!(analyzer, sv, info, atype)
end
function (::SoundPass)(
    ::Type{NoMethodErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    info::MethodMatchInfo, argtypes::Argtypes, @nospecialize(atype))
    return report_no_method_error!(analyzer, sv, info, atype)
end
function report_no_method_error!(
    analyzer::JETAnalyzer, sv::InferenceState, info::MethodMatchInfo, @nospecialize(atype))
    if is_empty_match(info)
        add_new_report!(sv.result, NoMethodErrorReport(sv, atype, 0))
        return true
    end
    return false
end

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
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
function CC.add_call_backedges!(analyzer::JETAnalyzer, @nospecialize(rettype), edges::Vector{MethodInstance},
                                matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
                                sv::InferenceState)
    # NOTE why we want to comment out the following lines:
    # a new method may refine analysis, so we always add backedges
    # # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine (widen) this type
    # rettype === Any && return
    for edge in edges
        add_backedge!(edge, sv)
    end
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

function CC.return_type_tfunc(analyzer::JETAnalyzer, argtypes::Argtypes, sv::InferenceState)
    # report pass for invalid `Core.Compiler.return_type` call
    ReportPass(analyzer)(InvalidReturnTypeCall, analyzer, sv, argtypes)

    return @invoke CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
end

@reportdef struct InvalidReturnTypeCall <: InferenceErrorReport end
get_msg(::Type{InvalidReturnTypeCall}, sv::InferenceState) = "invalid `Core.Compiler.return_type` call"

function (::SoundBasicPass)(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Argtypes)
    # here we make a very simple analysis to check if the call of `return_type` is clearly
    # invalid or not by just checking the # of call arguments
    # we don't take a (very unexpected) possibility of its overload into account here,
    # `NativeInterpreter` doens't also (it hard-codes the return type as `Type`)
    if length(argtypes) ≠ 3
        # invalid argument #, let's report and return error result (i.e. `Bottom`)
        add_new_report!(sv.result, InvalidReturnTypeCall(sv))
        return true
    end
    return false
end

@static if IS_AFTER_42529
function CC.abstract_invoke(analyzer::JETAnalyzer, arginfo::ArgInfo, sv::InferenceState)
    ret = @invoke CC.abstract_invoke(analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState)
    ReportPass(analyzer)(InvalidInvokeErrorReport, analyzer, sv, ret, arginfo.argtypes)
    return ret
end
else # @static if IS_AFTER_42529
function CC.abstract_invoke(analyzer::JETAnalyzer, argtypes::Argtypes, sv::InferenceState)
    ret = @invoke CC.abstract_invoke(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
    ReportPass(analyzer)(InvalidInvokeErrorReport, analyzer, sv, ret, argtypes)
    return ret
end
end # @static if IS_AFTER_42529

@reportdef struct InvalidInvokeErrorReport <: InferenceErrorReport
    argtypes::Argtypes
end
function get_msg(::Type{InvalidInvokeErrorReport}, sv::InferenceState, argtypes::Argtypes)
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

function (::SoundBasicPass)(::Type{InvalidInvokeErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes)
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            add_new_report!(sv.result, InvalidInvokeErrorReport(sv, argtypes))
            return true
        end
    end
    return false
end

function CC.abstract_eval_special_value(analyzer::JETAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, e, vtypes::VarTable, sv::InferenceState)

    if isa(e, GlobalRef)
        mod, name = e.mod, e.name
        # report pass for undefined global reference
        ReportPass(analyzer)(GlobalUndefVarErrorReport, analyzer, sv, mod, name)

        # NOTE `NativeInterpreter` should return `ret = Any` `ret` even if `mod.name`
        # isn't defined and we just pass it as is to collect as much error points as possible
        # we can change it to `Bottom` to suppress any further inference with this variable,
        # but then we also need to make sure to invalidate the cache for the analysis on
        # the future re-definition of this (currently) undefined binding
        # return Bottom
    end

    return ret
end

@reportdef struct GlobalUndefVarErrorReport <: InferenceErrorReport
    mod::Module
    name::Symbol
end
get_msg(::Type{GlobalUndefVarErrorReport}, sv::InferenceState, mod::Module, name::Symbol) =
    "variable $(mod).$(name) is not defined"

(::SoundPass)(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    report_undef_var!(sv, mod, name, true)
(::BasicPass)(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    report_undef_var!(sv, mod, name, false)
(::TypoPass)(::Type{GlobalUndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    report_undef_var!(sv, mod, name, false)
function report_undef_var!(sv::InferenceState, mod::Module, name::Symbol, sound::Bool)
    if !isdefined(mod, name)
        report = false
        if sound
            report |= true
        else
            if !is_corecompiler_undefglobal(mod, name)
                report |= true
            end
        end
        if report
            add_new_report!(sv.result, GlobalUndefVarErrorReport(sv, mod, name))
            return true
        end
    end
    return false
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
            # TODO upstream this ?
            if typeintersect(Bool, t) !== Bool
                ret = Bottom
            end
        end
    end

    return ret
end

@reportdef struct NonBooleanCondErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
end
function get_msg(::Type{NonBooleanCondErrorReport}, sv::InferenceState, @nospecialize(t), union_split::Int)
    if union_split == 0
        return "non-boolean ($t) used in boolean context"
    else
        ts = t::Vector{Any}
        nt = length(ts)
        tss = join(ts, ", ")
        return "for $nt of $union_split union split cases, non-boolean ($tss) used in boolean context"
    end
end

function (::SoundPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        rinfo = nothing
        uts = Base.uniontypes(t)
        for t in uts
            if !(t ⊑ Bool)
                if rinfo === nothing
                    rinfo = Any[], length(uts)
                end
                push!(rinfo[1], t)
            end
        end
        if rinfo !== nothing
            add_new_report!(sv.result, NonBooleanCondErrorReport(sv, rinfo...))
            return true
        end
    else
        if !(t ⊑ Bool)
            add_new_report!(sv.result, NonBooleanCondErrorReport(sv, t, 0))
            return true
        end
    end
    return false
end

function (::BasicPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    if basic_filter(analyzer, sv)
        if isa(t, Union)
            info = nothing
            uts = Base.uniontypes(t)
            for t in uts
                if typeintersect(Bool, t) !== Bool
                    if info === nothing
                        info = Any[], length(uts)
                    end
                    push!(info[1], t)
                end
            end
            if info !== nothing
                add_new_report!(sv.result, NonBooleanCondErrorReport(sv, info...))
                return true
            end
        else
            if typeintersect(Bool, t) !== Bool
                add_new_report!(sv.result, NonBooleanCondErrorReport(sv, t, 0))
                return true
            end
        end
    end
    return false
end

function (::SoundBasicPass)(::Type{InvalidConstantRedefinition}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    add_new_report!(sv.result, InvalidConstantRedefinition(sv, mod, name, prev_t, t))
    return true
end
function (::SoundBasicPass)(::Type{InvalidConstantDeclaration}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(sv.result, InvalidConstantDeclaration(sv, mod, name))
    return true
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

        # other general `throw` calls will be handled within `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
    else
        ReportPass(analyzer)(BuiltinErrorReport, analyzer, sv, f, argtypes, ret)
    end

    return ret
end

"""
    SeriousExceptionReport <: InferenceErrorReport

Represents a "serious" error that is manually thrown by a `throw` call.
This is reported regardless of whether it's caught by control flow or not, as opposed to
[`UncaughtExceptionReport`](@ref).
"""
@reportdef struct SeriousExceptionReport <: InferenceErrorReport
    @nospecialize err
    # keeps the location where this exception is raised
    # this information will be used later when collecting `UncaughtExceptionReport`s
    # in order to avoid duplicated reports from the same `throw` call
    loc::LineInfoNode
end
get_msg(T::Type{SeriousExceptionReport}, state, @nospecialize(err), loc::LineInfoNode) =
    string(first(split(sprint(showerror, err), '\n')))
print_error_report(io, report::SeriousExceptionReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

(::BasicPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    basic_filter(analyzer, sv) && report_serious_exception!(analyzer, sv, argtypes)
(::SoundPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    report_serious_exception!(analyzer, sv, argtypes) # any (non-serious) `throw` calls will be caught by the report pass for `UncaughtExceptionReport`
function report_serious_exception!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes)
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            err = a.val
            if isa(err, UndefKeywordError)
                add_new_report!(sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
                return true
            elseif isa(err, MethodError)
                # ignore https://github.com/JuliaLang/julia/blob/7409a1c007b7773544223f0e0a2d8aaee4a45172/base/boot.jl#L261
                if err.f !== Bottom
                    add_new_report!(sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
                    return true
                end
            end
        end
    end
    return false
end

"""
    BuiltinErrorReport

Represents errors caused by builtin-function calls.
Technically they're defined as those error points that can be caught within `Core.Compiler.builtin_tfunction`.
"""
abstract type BuiltinErrorReport <: InferenceErrorReport end

@reportdef struct NoFieldErrorReport <: BuiltinErrorReport
    @nospecialize typ # ::Type
    name::Symbol
end
get_msg(::Type{NoFieldErrorReport}, sv::InferenceState, @nospecialize(typ::Type), name::Symbol) =
    "type $(typ) has no field $(name)"
print_error_report(io, report::NoFieldErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

@reportdef struct DivideErrorReport <: BuiltinErrorReport end
let s = sprint(showerror, DivideError())
    global get_msg(::Type{DivideErrorReport}, sv::InferenceState) = s
end
print_error_report(io, report::DivideErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)

@reportdef struct InvalidBuiltinCallErrorReport <: BuiltinErrorReport
    argtypes::Argtypes
end
get_msg(::Type{InvalidBuiltinCallErrorReport}, sv::InferenceState, @nospecialize(args...)) =
    "invalid builtin function call"

# TODO we do need sound versions of these functions
# XXX for general case JET just relies on the (maybe too permissive) return type from native
# tfuncs to report invalid builtin calls and probably there're lots of false negatives

function (::BasicPass)(::Type{BuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    @assert !(f === throw) "`throw` calls shuold be handled either by the report pass of `SeriousExceptionReport` or `UncaughtExceptionReport`"
    if f === getfield
        maybe_report_getfield!(analyzer, sv, argtypes, ret) && return true
    elseif length(argtypes) == 2 &&
           f === Intrinsics.checked_sdiv_int ||
           f === Intrinsics.checked_srem_int ||
           f === Intrinsics.checked_udiv_int ||
           f === Intrinsics.checked_urem_int ||
           f === Intrinsics.sdiv_int ||
           f === Intrinsics.srem_int ||
           f === Intrinsics.udiv_int ||
           f === Intrinsics.urem_int
        maybe_report_devide_error!(analyzer, sv, argtypes, ret) && return true
    end
    return handle_invalid_builtins!(analyzer, sv, argtypes, ret)
end

function (::TypoPass)(::Type{BuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    if f === getfield
        maybe_report_getfield!(analyzer, sv, argtypes, ret) && return true
    end
    return false
end

function maybe_report_getfield!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    if 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const) && (mod = obj.val; isa(mod, Module))
                    # forward to the report pass for undefined global reference
                    if ReportPass(analyzer)(GlobalUndefVarErrorReport, analyzer, sv, mod, name)
                        return true
                    end
                elseif ret === Bottom
                    # report invalid field access detected by the native `getfield_tfunc`
                    typ = widenconst(obj)
                    add_new_report!(sv.result, NoFieldErrorReport(sv, typ, name))
                    return true
                end
            end
        end
    end
    return false
end

# TODO this check might be better in its own report pass, say `NumericalPass`
function maybe_report_devide_error!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    a = argtypes[2]
    t = widenconst(a)
    if isprimitivetype(t) && t <: Number
        if isa(a, Const) && a.val === zero(t)
            add_new_report!(sv.result, DivideErrorReport(sv))
            return true
        end
    end
    return false
end

function handle_invalid_builtins!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    # we don't bail out using `basic_filter` here because the native tfuncs are already very permissive
    if ret === Bottom
        add_new_report!(sv.result, InvalidBuiltinCallErrorReport(sv, argtypes))
        return true
    end
    return false
end

function (::SoundPass)(::Type{BuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    return BasicPass()(BuiltinErrorReport, analyzer, sv, f, argtypes, ret)

    # TODO enable this sound pass:
    # - make `stmt_effect_free` work on `InfernceState`
    # - sort out `argextype` interface to make it accept `InfernceState`
    @assert !(f === throw) "`throw` calls shuold be handled either by the report pass of `SeriousExceptionReport` or `UncaughtExceptionReport`"
    stmt = get_stmt((sv, get_currpc(sv)))
    if !CC.stmt_effect_free(stmt, ret, sv, sv.sptypes)
        add_new_report!(sv.result, UnsoundBuiltinCallErrorReport(sv, argtypes))
    end
end
