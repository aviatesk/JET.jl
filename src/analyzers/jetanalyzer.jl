struct JETAnalyzerConfig
    ignore_missing_comparison::Bool
    function JETAnalyzerConfig(;
        ignore_missing_comparison::Bool=false,
        __jetconfigs...)
        return new(
            ignore_missing_comparison)
    end
end

"""
Every [entry point of error analysis](@ref jetanalysis-entry) can accept
any of the [general configurations](@ref) as well as the following additional configurations
that are specific to the error analysis.

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
- `ignore_missing_comparison::Bool = false`:\\
  If `true`, JET will ignores the possibility of a poorly-inferred comparison operator call
  (e.g. `==`) returning `missing` in order to hide the error reports from branching on the
  potential `missing` return value of such a comparison operator call.
  This is turned off by default, because a comparison call results in a
  `Union{Bool,Missing}` possibility, it likely signifies an inferrability issue or the
  `missing` possibility should be handled someway. But this is useful to reduce the noisy
  error reports in the situations where specific input arguments type is not available at
  the beginning of the analysis like [`report_package`](@ref).
---
"""
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    state::AnalyzerState
    analysis_cache::AnalysisCache
    report_pass::RP
    method_table::CachedMethodTable{OverlayMethodTable}
    config::JETAnalyzerConfig

    function JETAnalyzer(state::AnalyzerState, analysis_cache::AnalysisCache, report_pass::RP,
                         config::JETAnalyzerConfig) where RP<:ReportPass
        method_table = CachedMethodTable(OverlayMethodTable(state.world, JET_METHOD_TABLE))
        return new{RP}(state, analysis_cache, report_pass, method_table, config)
    end
    function JETAnalyzer(state::AnalyzerState, report_pass::ReportPass,
                         config::JETAnalyzerConfig)
        if ((@static VERSION < v"1.11.0-DEV.1255" && true) && generating_output())
            # XXX Avoid storing analysis results into a cache that persists across the
            #     precompilation, as pkgimage currently doesn't support serializing
            #     externally created `CodeInstance`. Otherwise, `CodeInstance`s created by
            #     JET, containing JET-specific data structures, will leak into the native
            #     code cache, likely causing segfaults or undefined behavior.
            #     (see https://github.com/JuliaLang/julia/issues/48453).
            analysis_cache = AnalysisCache()
        else
            cache_key = compute_hash(state.inf_params, report_pass, config)
            analysis_cache = get!(AnalysisCache, JET_ANALYZER_CACHE, cache_key)
        end
        return JETAnalyzer(state, analysis_cache, report_pass, config)
    end
end

# JETAnalyzer does not need any sources, so discard them always
CC.maybe_compress_codeinfo(::JETAnalyzer, ::MethodInstance, ::CodeInfo) = nothing

# JETAnalyzer hooks on abstract interpretation only,
# and so the cost of running the optimization passes is just unnecessary
CC.may_optimize(::JETAnalyzer) = false

CC.method_table(analyzer::JETAnalyzer) = analyzer.method_table

"""
    IntrinsicErrorCheckLattice <: AbstractLattice

This lattice is used to check if an intrinsic function call is erroneous.
It is not adjointing any lattice elements and is merely used to overload `tfunc`s for
intrinsic calls that return `IntrinsicError` when the call is erroneous.
"""
struct IntrinsicErrorCheckLattice{𝕃<:AbstractLattice} <: AbstractLattice
    inner::𝕃
end
CC.widenlattice(𝕃::IntrinsicErrorCheckLattice) = 𝕃.inner
CC.is_valid_lattice_norec(::IntrinsicErrorCheckLattice, @nospecialize(elem)) = false
@nospecs CC.:⊑(𝕃::IntrinsicErrorCheckLattice, x, y) = ⊑(widenlattice(𝕃), x, y)
@nospecs CC.tmerge(𝕃::IntrinsicErrorCheckLattice, x, y) = tmerge(widenlattice(𝕃), x, y)
@nospecs CC.tmeet(𝕃::IntrinsicErrorCheckLattice, x, t::Type) = tmeet(widenlattice(𝕃), x, t)
@nospecs CC._getfield_tfunc(𝕃::IntrinsicErrorCheckLattice, xs...) = CC._getfield_tfunc(widenlattice(𝕃), xs...)
CC.typeinf_lattice(::JETAnalyzer) = CC.InferenceLattice(IntrinsicErrorCheckLattice(CC.MustAliasesLattice(CC.BaseInferenceLattice.instance)))
CC.ipo_lattice(::JETAnalyzer) = CC.InferenceLattice(IntrinsicErrorCheckLattice(CC.InterMustAliasesLattice(CC.IPOResultLattice.instance)))

# AbstractAnalyzer API
# ====================

JETInterface.AnalyzerState(analyzer::JETAnalyzer) = analyzer.state
function JETInterface.AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState)
    return JETAnalyzer(state, ReportPass(analyzer), JETAnalyzerConfig(analyzer))
end
JETInterface.ReportPass(analyzer::JETAnalyzer) = analyzer.report_pass
JETInterface.AnalysisCache(analyzer::JETAnalyzer) = analyzer.analysis_cache

const JET_ANALYZER_CACHE = IdDict{UInt, AnalysisCache}()

JETAnalyzerConfig(analyzer::JETAnalyzer) = analyzer.config

# report passes
# =============

# TODO elaborate the documentations of passes

"""
The basic error analysis pass. This is used by default.
"""
struct BasicPass <: ReportPass end

function basic_filter(analyzer::JETAnalyzer, sv::InferenceState)
    mi = sv.linfo
    is_compileable_mi(mi) && return true
    return is_entry(analyzer, mi) # `report_call` may start analysis with abstract signature
end

"""
The sound error analysis pass.
"""
struct SoundPass <: ReportPass end

# `SoundPass` is still WIP, we may use it to implement both passes at once for the meantime
const SoundBasicPass = Union{SoundPass,BasicPass}

"""
A typo detection pass.
"""
struct TypoPass <: ReportPass end
(::TypoPass)(@nospecialize _...) = return false # ignore everything except UndefVarErrorReport and field error report

# A report pass that is used for `analyze_from_definitions!`.
# Especially, this report pass ignores `UncaughtExceptionReport` to avoid false positives
# from methods that are intentionally written to throw errors.
struct DefinitionAnalysisPass <: ReportPass end
(::DefinitionAnalysisPass)(@nospecialize args...) = BasicPass()(args...)

# overlay method table
# ====================

"""
    JET_METHOD_TABLE

This JET-specific method table keeps method definition overrides, that allow us to cut off
false positive errors, while simulating the original semantics reasonably.
This works as a temporal patch, and ideally we want to port it back to the Julia base or
a package, or improve the accuracy of base abstract interpretation analysis.
"""
@MethodTable JET_METHOD_TABLE

# https://github.com/aviatesk/JET.jl/issues/404
# this definition makes it impossible to dispatch to `Base.iterate(()::Tuple, i::Int)`,
# getting rid of the false positive error from `getindex((), i)`.
@overlay JET_METHOD_TABLE Base.iterate(::Tuple{}, ::Int) = nothing

# overloads
# =========

@static if VERSION ≥ v"1.11.0-DEV.843"
function CC.InferenceState(result::InferenceResult, cache_mode::UInt8, analyzer::JETAnalyzer)
    frame = @invoke CC.InferenceState(result::InferenceResult, cache_mode::UInt8, analyzer::AbstractAnalyzer)
    if isnothing(frame) # indicates something bad happened within `retrieve_code_info`
        ReportPass(analyzer)(GeneratorErrorReport, analyzer, result)
    end
    return frame
end
else
function CC.InferenceState(result::InferenceResult, cache_mode::Symbol, analyzer::JETAnalyzer)
    frame = @invoke CC.InferenceState(result::InferenceResult, cache_mode::Symbol, analyzer::AbstractAnalyzer)
    if isnothing(frame) # indicates something bad happened within `retrieve_code_info`
        ReportPass(analyzer)(GeneratorErrorReport, analyzer, result)
    end
    return frame
end
end

function CC.finish!(analyzer::JETAnalyzer, caller::InferenceState)
    src = caller.result.src

    if isnothing(src)
        # caught in cycle, similar error should have been reported where the source is available
    elseif src isa CodeInfo
        # report pass for uncaught `throw` calls
        ReportPass(analyzer)(UncaughtExceptionReport, analyzer, caller, src.code)
    else
        # NOTE `src` never be `OptpimizationState` since `CC.may_optimize(::JETAnalyzer) === false`
        Core.eval(@__MODULE__, :(src = $src))
        throw("unexpected state happened, inspect `$(@__MODULE__).src`")
    end

    return @invoke CC.finish!(analyzer::AbstractAnalyzer, caller::InferenceState)
end

function CC.abstract_call_gf_by_type(analyzer::JETAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype), sv::InferenceState,
    max_methods::Int)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer,
        f::Any, arginfo::ArgInfo, si::StmtInfo, atype::Any, sv::InferenceState, max_methods::Int)
    ReportPass(analyzer)(MethodErrorReport, analyzer, sv, ret, arginfo.argtypes, atype)
    ReportPass(analyzer)(UnanalyzedCallReport, analyzer, sv, ret, atype)
    return ret
end

function CC.from_interprocedural!(analyzer::JETAnalyzer,
    @nospecialize(rt), sv::InferenceState, arginfo::ArgInfo, @nospecialize(maybecondinfo))
    ret = @invoke CC.from_interprocedural!(analyzer::AbstractAnalyzer,
        rt::Any, sv::InferenceState, arginfo::ArgInfo, maybecondinfo::Any)
    if JETAnalyzerConfig(analyzer).ignore_missing_comparison
        # Widen the return type of comparison operator calls to ignore the possibility of
        # they returning `missing` when analyzing from top-level.
        # Otherwise we will see frustrating false positive errors from branching on the
        # return value (aviatesk/JET.jl#542), since the analysis often uses loose
        # top-level argument types as input.
        if ret === Union{Bool,Missing}
            ret = Any
        end
    end
    return ret
end

"""
    Core.Compiler.bail_out_call(analyzer::JETAnalyzer, ...)

This overload makes call inference performed by `JETAnalyzer` not bail out even when
inferred return type grows up to `Any` to collect as much error reports as possible.
That potentially slows down inference performance, but it would stay to be practical
given that the number of matching methods are limited beforehand.
"""
CC.bail_out_call(::JETAnalyzer, ::CC.InferenceLoopState, ::InferenceState) = false

struct __DummyRettype__ end

"""
    Core.Compiler.add_call_backedges!(analyzer::JETAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::JETAnalyzer, ...)`, which always add
backedges (even if a new method can't refine the return type grew up to `Any`).
This is because a new method definition always has a potential to change `JETAnalyzer`'s analysis result.
"""
function CC.add_call_backedges!(
    analyzer::JETAnalyzer, @nospecialize(rettype), effects::CC.Effects,
    edges::Vector{MethodInstance}, matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
    sv::InferenceState)
    return @invoke CC.add_call_backedges!(
        # NOTE this `__DummyRettype__()` hack forces `add_call_backedges!(::AbstractInterpreter,...)` to add backedges
        analyzer::AbstractInterpreter, __DummyRettype__()::Any, effects::CC.Effects,
        edges::Vector{MethodInstance}, matches::Union{MethodMatches,UnionSplitMethodMatches}, atype::Any,
        sv::InferenceState)
end

# TODO Reasons about error found by [semi-]concrete evaluation:
# For now JETAnalyzer allows the regular constant-prop' only,
# unless the analyzed effects are proven to be `:nothrow`.
function CC.concrete_eval_eligible(analyzer::JETAnalyzer,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
    if CC.is_nothrow(result.effects)
        neweffects = CC.Effects(result.effects;
            nonoverlayed=@static VERSION ≥ v"1.11.0-beta2.49" ? CC.ALWAYS_TRUE : true)
        @static if VERSION ≥ v"1.11.0-DEV.945"
        newresult = MethodCallResult(result.rt, result.exct, result.edgecycle, result.edgelimited,
                                     result.edge, neweffects)
        else
        newresult = MethodCallResult(result.rt, result.edgecycle, result.edgelimited,
                                     result.edge, neweffects)
        end
        res = @invoke CC.concrete_eval_eligible(analyzer::AbstractAnalyzer,
            f::Any, newresult::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
        if res === :concrete_eval
            return :concrete_eval
        end
    elseif (istopfunction(f, :fieldindex) || istopfunction(f, :typejoin) ||
            istopfunction(f, :typejoin_union_tuple))
        if concrete_eval_eligible_ignoring_overlay(result, arginfo)
            return :concrete_eval
        end
    end
    # disables both concrete evaluation and semi-concrete interpretation
    return :none
end

function concrete_eval_eligible_ignoring_overlay(result::MethodCallResult, arginfo::ArgInfo)
    result.edge !== nothing || return false
    return CC.is_foldable(result.effects) && CC.is_all_const_arg(arginfo, #=start=#2)
end

function CC.return_type_tfunc(analyzer::JETAnalyzer, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
    # report pass for invalid `Core.Compiler.return_type` call
    ReportPass(analyzer)(InvalidReturnTypeCall, analyzer, sv, argtypes)
    return @invoke CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
end

function CC.abstract_invoke(analyzer::JETAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState)
    ret = @invoke CC.abstract_invoke(analyzer::AbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState)
    ReportPass(analyzer)(InvalidInvokeErrorReport, analyzer, sv, ret, arginfo.argtypes)
    return ret
end

# report pass for undefined static parameter
@static if VERSION ≥ v"1.11.0-DEV.888"
function CC.abstract_eval_statement_expr(analyzer::JETAnalyzer, e::Expr, vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_statement_expr(analyzer::AbstractAnalyzer, e::Expr, vtypes::VarTable, sv::InferenceState)
    if e.head === :static_parameter
        ReportPass(analyzer)(UndefVarErrorReport, analyzer, sv, e.args[1]::Int)
    end
    return ret
end
else
function CC.abstract_eval_value_expr(analyzer::JETAnalyzer, e::Expr, vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value_expr(analyzer::AbstractAnalyzer, e::Expr, vtypes::VarTable, sv::InferenceState)
    if e.head === :static_parameter
        ReportPass(analyzer)(UndefVarErrorReport, analyzer, sv, e.args[1]::Int)
    end
    return ret
end
end

function CC.abstract_eval_special_value(analyzer::JETAnalyzer,
    @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractAnalyzer,
        e::Any, vtypes::VarTable, sv::InferenceState)

    if isa(e, GlobalRef)
        # report pass for undefined global reference
        ReportPass(analyzer)(UndefVarErrorReport, analyzer, sv, e)
    # elseif isa(e, SlotNumber)
    #     # TODO enable this (aviatesk/JET.jl#596)
    #     # report pass for (local) undef var error
    #     ReportPass(analyzer)(UndefVarErrorReport, analyzer, sv, e, vtypes, ret)
    end

    return ret
end

# N.B. this report pass won't be necessary as the frontend will generate code
# that `typeassert`s the value type as the binding type beforehand
@inline function CC.abstract_eval_basic_statement(analyzer::JETAnalyzer,
    @nospecialize(stmt), pc_vartable::VarTable, frame::InferenceState)
    ret = @invoke CC.abstract_eval_basic_statement(analyzer::AbstractAnalyzer,
        stmt::Any, pc_vartable::VarTable, frame::InferenceState)
    if isexpr(stmt, :(=)) && (lhs = stmt.args[1]; isa(lhs, GlobalRef))
        rt = @static VERSION ≥ v"1.11.0-DEV.945" ? ret.rt : ret.type
        ReportPass(analyzer)(InvalidGlobalAssignmentError, analyzer,
            frame, lhs.mod, lhs.name, rt)
    end
    return ret
end

function CC.abstract_eval_value(analyzer::JETAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractAnalyzer, e::Any, vtypes::VarTable, sv::InferenceState)

    # report non-boolean condition error
    stmt = get_stmt((sv, get_currpc(sv)))
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom
            ReportPass(analyzer)(NonBooleanCondErrorReport, analyzer, sv, t)
        end
    end

    return ret
end

@static if VERSION ≥ v"1.11.0-DEV.1080"
function CC.abstract_throw(analyzer::JETAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    ft = popfirst!(argtypes)
    ReportPass(analyzer)(SeriousExceptionReport, analyzer, sv, argtypes)
    pushfirst!(argtypes, ft)
    return @invoke CC.abstract_throw(analyzer::AbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
end
end

function CC.builtin_tfunction(analyzer::JETAnalyzer,
    @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::AbstractAnalyzer,
        f::Any, argtypes::Vector{Any}, sv::InferenceState)

    if f === fieldtype
        # the valid widest possible return type of `fieldtype_tfunc` is `Union{Type,TypeVar}`
        # because fields of unwrapped `DataType`s can legally be `TypeVar`s,
        # but this will lead to lots of false positive `MethodErrorReport`s for inference
        # with accessing to abstract fields since most methods don't expect `TypeVar`
        # (e.g. `@report_call readuntil(stdin, 'c')`)
        # JET.jl further widens this case to `Any` and give up further analysis rather than
        # trying hard to do sound and noisy analysis
        # xref: https://github.com/JuliaLang/julia/pull/38148
        if ret === Union{Type, TypeVar}
            ret = Any
        end
    end

    if f === throw
        # here we only report a selection of "serious" exceptions, i.e. those that should be
        # reported even if they may be caught in actual execution;
        ReportPass(analyzer)(SeriousExceptionReport, analyzer, sv, argtypes)

        # other general `throw` calls will be handled within `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
    else
        ReportPass(analyzer)(AbstractBuiltinErrorReport, analyzer, sv, f, argtypes, ret)
    end

    # `IntrinsicError` is a special marker object that JET uses to indicate an erroneous
    # intrinsic function call, so fix it up here to `Bottom`
    if ret isa IntrinsicError
        ret = Bottom
    end

    return ret
end

# analysis
# ========

@jetreport struct GeneratorErrorReport <: InferenceErrorReport
    @nospecialize err # actual error wrapped
end
JETInterface.print_report_message(io::IO, rep::GeneratorErrorReport) = showerror(io, rep.err)

# XXX what's the "soundness" of a `@generated` function ?
# adapted from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::SoundBasicPass)(::Type{GeneratorErrorReport}, analyzer::JETAnalyzer, result::InferenceResult)
    mi = result.linfo
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(analyzer, linfo) XXX doesn't work
        CC.may_invoke_generator(mi) || return false
        world = get_inference_world(analyzer)
        try
            @static if VERSION ≥ v"1.11-"
            @ccall jl_code_for_staged(mi::Any, world::UInt)::Any
            else
            @ccall jl_code_for_staged(mi::Any)::Any
            end
        catch err
            # if user code throws error, wrap and report it
            report = add_new_report!(analyzer, result, GeneratorErrorReport(mi, err))
            # we will return back to the caller immediately
            stash_report!(analyzer, report)
            return true
        end
    end
    return false
end

"""
    UncaughtExceptionReport <: InferenceErrorReport

Represents general `throw` calls traced during inference.
This is reported only when it's not caught by control flow.
"""
@jetreport struct UncaughtExceptionReport <: InferenceErrorReport
    single_error::Bool
end
function UncaughtExceptionReport(sv::InferenceState, throw_calls::Vector{Tuple{Int,Expr}})
    vf = get_virtual_frame(sv.linfo)
    vst = VirtualFrame[vf]
    sigs = Any[]
    tt = Union{}
    ncalls = length(throw_calls)
    for (i, (pc, call)) in enumerate(throw_calls)
        call_sig, call_tt = get_sig_nowrap((sv, pc), call)
        append!(sigs, call_sig)
        tt = Union{tt, call_tt}
        i ≠ ncalls && push!(sigs, ", ")
    end
    sig = Signature(sigs, tt)
    single_error = ncalls == 1
    return UncaughtExceptionReport(vst, sig, single_error)
end
function JETInterface.print_report_message(io::IO, r::UncaughtExceptionReport)
    msg = r.single_error ? "may throw" : "may throw either of"
    print(io, msg)
end

# report `throw` calls "appropriately"
# this error report pass is very special, since 1.) it's tightly bound to the report pass of
# `SeriousExceptionReport` and 2.) it involves "report filtering" on its own
function (::BasicPass)(::Type{UncaughtExceptionReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any})
    if frame.bestguess === Bottom
        report_uncaught_exceptions!(analyzer, frame, stmts)
        return true
    else
        # the non-`Bottom` result may mean `throw` calls from the children frames
        # (if exists) are caught and not propagated here
        # we don't want to cache the caught `UncaughtExceptionReport`s for this frame and
        # its parents, and just filter them away now
        filter!(get_reports(analyzer, frame.result)) do @nospecialize(report::InferenceErrorReport)
            return !isa(report, UncaughtExceptionReport)
        end
    end
    return false
end
function (::DefinitionAnalysisPass)(::Type{UncaughtExceptionReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any})
    # the non-`Bottom` result may mean `throw` calls from the children frames
    # (if exists) are caught and not propagated here
    # we don't want to cache the caught `UncaughtExceptionReport`s for this frame and
    # its parents, and just filter them away now
    filter!(get_reports(analyzer, frame.result)) do @nospecialize(report::InferenceErrorReport)
        return !isa(report, UncaughtExceptionReport)
    end
    return false
end
(::SoundPass)(::Type{UncaughtExceptionReport}, analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_uncaught_exceptions!(analyzer, frame, stmts) # yes, you want tons of false positives !
function report_uncaught_exceptions!(analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any})
    # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
    # `throw` calls
    # XXX it's possible that the `throw` calls within them are all caught but the other
    # critical errors still make the return type `Bottom`
    # NOTE to reduce the false positive cases described above, we count `throw` calls
    # after optimization, since it may have eliminated "unreachable" `throw` calls
    codelocs = frame.src.codelocs
    linetable = frame.src.linetable::LineTable
    reported_locs = nothing
    for report in get_reports(analyzer, frame.result)
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
        @static if VERSION ≥ v"1.11.0-DEV.888"
            CC.is_throw_call(stmt, stmts) || continue
        else
            CC.is_throw_call(stmt) || continue
        end
        # if this `throw` is already reported, don't duplicate
        if !isnothing(reported_locs) && linetable[codelocs[pc]]::LineInfoNode in reported_locs
            continue
        end
        if isnothing(throw_calls)
            throw_calls = Tuple{Int,Expr}[]
        end
        push!(throw_calls, (pc, stmt))
    end
    if !isnothing(throw_calls) && !isempty(throw_calls)
        add_new_report!(analyzer, frame.result, UncaughtExceptionReport(frame, throw_calls))
        return true
    end
    return false
end

@jetreport struct MethodErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
    uncovered::Bool
end
function JETInterface.print_report_message(io::IO, report::MethodErrorReport)
    (; t, union_split, uncovered) = report
    if uncovered
        print(io, "uncovered method match found ")
    else
        print(io, "no matching method found ")
    end
    if union_split == 0
        print_callsig(io, t)
    else
        ts = t::Vector{Any}
        nts = length(ts)
        for i = 1:nts
            print_callsig(io, ts[i])
            i == nts || print(io, ", ")
        end
        print(io, " (", nts, '/', union_split, " union split)")
    end
end
function print_callsig(io, @nospecialize(t))
    print(io, '`')
    Base.show_tuple_as_call(io, Symbol(""), t)
    print(io, '`')
end

function (rp::BasicPass)(::Type{MethodErrorReport}, analyzer::JETAnalyzer,
    sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype))
    info = call.info
    if isa(info, ConstCallInfo)
        info = info.call
    end
    if isa(info, MethodMatchInfo) || isa(info, UnionSplitInfo)
        basic_filter(analyzer, sv) || return false
    end
    if isa(info, MethodMatchInfo)
        return report_method_error!(analyzer, sv, info, atype, argtypes, call.rt, #=sound=#false)
    elseif isa(info, UnionSplitInfo)
        return report_method_error_for_union_split!(analyzer, sv, info, argtypes, call.rt, #=sound=#false)
    end
    return false
end

function (::SoundPass)(::Type{MethodErrorReport}, analyzer::JETAnalyzer,
    sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype))
    (; rt, info) = call
    if isa(info, ConstCallInfo)
        info = info.call
    end
    if isa(info, MethodMatchInfo)
        return report_method_error!(analyzer, sv, info, atype, argtypes, rt, #=sound=#true)
    elseif isa(info, UnionSplitInfo)
        return report_method_error_for_union_split!(analyzer, sv, info, argtypes, rt, #=sound=#true)
    end
    return false
end

const REDUCE_EMPTY_REPORT_SIG = let
    sig = Any["MethodError: reducing over an empty collection is not allowed; consider supplying `init` to the reducer"]
    Signature(sig, nothing)
end

# special case `reduce_empty` and `mapreduce_empty`:
# In the `:basic` mode, downgrade `MethodErrorReport` on call of `reduce_empty` or `mapreduce_empty`
# to `UncaughtExceptionReport` so that it can be filtered out in common cases.
# xref: https://github.com/JuliaLang/julia/pull/41885/
function report_reduce_empty_error!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    f = singleton_type(argtypes[1])
    if f !== nothing
        if f === Base.reduce_empty || f === Base.mapreduce_empty
            vf = get_virtual_frame(sv.linfo)
            report = UncaughtExceptionReport([vf], REDUCE_EMPTY_REPORT_SIG, #=single_error=#true)
            add_new_report!(analyzer, sv.result, report)
            return true
        end
    end
    return false
end

function report_method_error!(analyzer::JETAnalyzer,
    sv::InferenceState, info::MethodMatchInfo, @nospecialize(atype), argtypes::Vector{Any},
    @nospecialize(rt), sound::Bool)
    if is_empty_match(info)
        if !sound && report_reduce_empty_error!(analyzer, sv, argtypes)
            return true
        end
        report = MethodErrorReport(sv, atype, 0, #=uncovered=#false)
        add_new_report!(analyzer, sv.result, report)
        return true
    elseif sound && !is_fully_covered(info)
        report = MethodErrorReport(sv, atype, 0, #=uncovered=#true)
        report.sig[end] = widenconst(ignorelimited(rt))
        add_new_report!(analyzer, sv.result, report)
        return true
    end
    return false
end

function report_method_error_for_union_split!(analyzer::JETAnalyzer,
    sv::InferenceState, info::UnionSplitInfo, argtypes::Argtypes, @nospecialize(rt), sound::Bool)
    # check each match for union-split signature
    split_argtypes = empty_matches = uncovered_matches = nothing
    reported = false
    for (i, matchinfo) in enumerate(info.matches)
        if is_empty_match(matchinfo)
            if isnothing(split_argtypes)
                split_argtypes = CC.switchtupleunion(typeinf_lattice(analyzer), argtypes)
            end
            argtypes′ = split_argtypes[i]::Vector{Any}
            if !sound && report_reduce_empty_error!(analyzer, sv, argtypes′)
                reported |= true
                continue
            end
            if empty_matches === nothing
                empty_matches = (Any[], length(info.matches))
            end
            sig_n = argtypes_to_type(argtypes′)
            push!(empty_matches[1], sig_n)
        elseif sound && !is_fully_covered(matchinfo)
            if isnothing(split_argtypes)
                split_argtypes = CC.switchtupleunion(typeinf_lattice(analyzer), argtypes)
            end
            argtypes′ = split_argtypes[i]::Vector{Any}
            if uncovered_matches === nothing
                uncovered_matches = (Any[], length(info.matches))
            end
            sig_n = argtypes_to_type(argtypes′)
            push!(uncovered_matches[1], sig_n)
        end
    end
    if empty_matches !== nothing
        add_new_report!(analyzer, sv.result, MethodErrorReport(sv, empty_matches..., #=reason=#false))
        reported |= true
    end
    if uncovered_matches !== nothing
        report = MethodErrorReport(sv, uncovered_matches..., #=uncovered=#true)
        add_new_report!(analyzer, sv.result, report)
        report.sig[end] = widenconst(ignorelimited(rt))
        reported |= true
    end
    return reported
end

is_empty_match(info::MethodMatchInfo) = CC.isempty(info.results)
is_fully_covered(info::MethodMatchInfo) = CC._all(m->m.fully_covers, info.results)

@jetreport struct UnanalyzedCallReport <: InferenceErrorReport
    @nospecialize type
end
function JETInterface.print_report_message(io::IO, report::UnanalyzedCallReport)
    print(io, "unanalyzed method call ")
    print_callsig(io, report.type)
end

(::BasicPass)(::Type{UnanalyzedCallReport}, ::JETAnalyzer, ::InferenceState, ::CallMeta, @nospecialize(_)) = false
(::TypoPass)(::Type{UnanalyzedCallReport}, ::JETAnalyzer, ::InferenceState, ::CallMeta, @nospecialize(_)) = false
function (::SoundPass)(::Type{UnanalyzedCallReport}, analyzer::JETAnalyzer,
    sv::InferenceState, call::CallMeta, @nospecialize(atype))
    if call.info === CC.NoCallInfo()
        @assert call.rt === Any "unexpected call info"
        report = UnanalyzedCallReport(sv, atype)
        add_new_report!(analyzer, sv.result, report)
        report.sig[end] = Any
        return true
    end
    return false
end

@jetreport struct InvalidReturnTypeCall <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::InvalidReturnTypeCall)
    print(io, "invalid `Core.Compiler.return_type` call")
end

function (::SoundBasicPass)(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Argtypes)
    # here we make a very simple analysis to check if the call of `return_type` is clearly
    # invalid or not by just checking the # of call arguments
    # we don't take a (very unexpected) possibility of its overload into account here,
    # `Core.Compiler.NativeInterpreter` doesn't also (it hard-codes the return type as `Type`)
    if length(argtypes) ≠ 3
        # invalid argument #, let's report and return error result (i.e. `Bottom`)
        add_new_report!(analyzer, sv.result, InvalidReturnTypeCall(sv))
        return true
    end
    return false
end

@jetreport struct InvalidInvokeErrorReport <: InferenceErrorReport
    argtypes::Argtypes
end
function JETInterface.print_report_message(io::IO, (; argtypes)::InvalidInvokeErrorReport)
    fallback_msg = "invalid invoke" # mostly because of runtime unreachable

    ft = widenconst(argtype_by_index(argtypes, 2))
    if ft === Bottom
        print(io, "invalid invoke") # mostly because of runtime unreachable
        return
    end

    t = argtype_by_index(argtypes, 3)
    (types, isexact, isconcrete, istype) = instanceof_tfunc(t)
    if types === Bottom
        if isa(t, Const)
            type = typeof(t.val)
            print(io, "argument type should be `Type`-object (given `", type, "`)")
        else
            print(io, "invalid invoke") # mostly because of runtime unreachable
        end
        return
    end

    argtype = argtypes_to_type(CC.argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    @assert nargtype === Bottom
    print(io, "actual argument type `", argtype, "` doesn't intersect with specified argument type `", types, '`')
    return
end

function (::SoundBasicPass)(::Type{InvalidInvokeErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes)
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            add_new_report!(analyzer, sv.result, InvalidInvokeErrorReport(sv, argtypes))
            return true
        end
    end
    return false
end

@jetreport struct UndefVarErrorReport <: InferenceErrorReport
    var::Union{GlobalRef,TypeVar,Symbol}
end
function JETInterface.print_report_message(io::IO, r::UndefVarErrorReport)
    var = r.var
    if isa(var, TypeVar)
        print(io, "`", var.name, "` not defined in static parameter matching")
    else
        if isa(var, GlobalRef)
            print(io, "`", var.mod, '.', var.name, "`")
        else
            print(io, "local variable `", var, "`")
        end
        print(io, " is not defined")
    end
end

# undefined global variable report passes

(::SoundPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, gr::GlobalRef) =
    report_undef_global_var!(analyzer, sv, gr, #=sound=#true)
(::BasicPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, gr::GlobalRef) =
    report_undef_global_var!(analyzer, sv, gr, #=sound=#false)
(::TypoPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, gr::GlobalRef) =
    report_undef_global_var!(analyzer, sv, gr, #=sound=#false)
function report_undef_global_var!(analyzer::JETAnalyzer, sv::InferenceState, gr::GlobalRef, sound::Bool)
    isdefined(gr.mod, gr.name) && return false
    sound && @goto report
    is_corecompiler_undefglobal(gr) && return false
    # if this global var is explicitly type-declared, it will be likely get assigned somewhere
    # TODO give this permission only to top-level analysis
    ccall(:jl_get_binding_type, Any, (Any, Any), gr.mod, gr.name) !== nothing && return false
    begin @label report
        add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, gr))
        return true
    end
end

# Returns `true` if this global reference is undefined inside `Core.Compiler`, but the
# corresponding name exists in the `Base` module.
# `Core.Compiler` reuses the minimum amount of `Base` code and there're some of missing
# definitions, and `BasicPass` will exclude reports on those undefined names since they
# usually don't matter and `Core.Compiler`'s basic functionality is battle-tested and
# validated exhausively by its test suite and real-world usages.
function is_corecompiler_undefglobal(gr::GlobalRef)
    gr.mod === CC && return isdefined(Base, gr.name)
    return false
end

# undefined static parameter report passes

(::SoundPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, n::Int) =
    report_undef_static_parameter!(analyzer, sv, n, true)
(::BasicPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, n::Int) =
    report_undef_static_parameter!(analyzer, sv, n, false)
(::TypoPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, n::Int) =
    report_undef_static_parameter!(analyzer, sv, n, false)
function report_undef_static_parameter!(analyzer::JETAnalyzer, sv::InferenceState, n::Int, sound::Bool)
    if 1 ≤ n ≤ length(sv.sptypes)
        mi = sv.linfo
        if sv.sptypes[n].undef && (sound || is_compileable_mi(mi))
            tv = mi.sparam_vals[n]::TypeVar
            add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, tv))
            return true
        end
    else
        add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, TypeVar(:unknown)))
        return true
    end
    return false
end

# undefined local variable report passes

# these report passes use `:throw_undef_if_not` and `:(unreachable)` introduced by the native
# optimization pass, and thus supposed to only work on post-optimization code
# (::SoundPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, e::SlotNumber, vtypes::VarTable, @nospecialize(ret)) =
#     report_undefined_local_slots!(analyzer, sv, e, vtypes, ret, #=unsound=#false)
# (::BasicPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, e::SlotNumber, vtypes::VarTable, @nospecialize(ret)) =
#     report_undefined_local_slots!(analyzer, sv, e, vtypes, ret, #=unsound=#true)

function (::SoundPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    var::SlotNumber, vtypes::VarTable, @nospecialize(ret))
    vtyp = vtypes[slot_id(var)]
    if vtyp.undef
        add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, get_slotname(sv, var)))
        return true
    end
    return false
end
function (::BasicPass)(::Type{UndefVarErrorReport}, analyzer::JETAnalyzer, sv::InferenceState,
    var::SlotNumber, vtypes::VarTable, @nospecialize(ret))
    ret === Bottom || return false
    add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, get_slotname(sv, var)))
    return true
end

@jetreport struct InvalidGlobalAssignmentError <: InferenceErrorReport
    @nospecialize vtyp
    @nospecialize btyp
    mod::Module
    name::Symbol
end
function JETInterface.print_report_message(io::IO, report::InvalidGlobalAssignmentError)
    print(io, "found invalid assignment of an incompatible value")
    print(io, " (`", report.vtyp, "`)")
    print(io, " to the value global")
    print(io, " `", GlobalRef(report.mod, report.name), "`")
    print(io, " (`", report.btyp, "`)")
end

(::SoundPass)(::Type{InvalidGlobalAssignmentError}, analyzer::JETAnalyzer,
    sv::InferenceState, mod::Module, name::Symbol, @nospecialize(vtyp)) =
    report_global_assignment!(analyzer, sv, mod, name, vtyp,#=sound=#true)
(::BasicPass)(::Type{InvalidGlobalAssignmentError}, analyzer::JETAnalyzer,
    sv::InferenceState, mod::Module, name::Symbol, @nospecialize(vtyp)) =
    report_global_assignment!(analyzer, sv, mod, name, vtyp,#=sound=#false)
(::TypoPass)(::Type{InvalidGlobalAssignmentError}, analyzer::JETAnalyzer,
    sv::InferenceState, mod::Module, name::Symbol, @nospecialize(vtyp)) =
    report_global_assignment!(analyzer, sv, mod, name, vtyp, #=sound=#false)
function report_global_assignment!(analyzer::JETAnalyzer,
    sv::InferenceState, mod::Module, name::Symbol, @nospecialize(vtyp), sound::Bool)
    btyp = ccall(:jl_get_binding_type, Any, (Any, Any), mod, name)
    if btyp !== nothing
        vtyp = widenconst(ignorelimited(vtyp))
        if !(sound ? vtyp ⊑ btyp : hasintersect(vtyp, btyp))
            add_new_report!(analyzer, sv.result, InvalidGlobalAssignmentError(sv, vtyp, btyp, mod, name))
            return true
        end
        return false
    else # the binding type hasn't been declared yet
        return false
    end
end

@jetreport struct NonBooleanCondErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
    uncovered::Bool
end
function JETInterface.print_report_message(io::IO, report::NonBooleanCondErrorReport)
    (; t, union_split, uncovered) = report
    if union_split == 0
        print(io, "non-boolean `", t, "`")
        if uncovered
            print(io, " may be used in boolean context")
        else
            print(io, " found in boolean context")
        end
    else
        ts = t::Vector{Any}
        nts = length(ts)
        print(io, "non-boolean ")
        for i = 1:nts
            print(io, '`', ts[i], '`')
            i == nts || print(io, ", ")
        end
        if uncovered
            print(io, " may be used in boolean context")
        else
            print(io, " found in boolean context")
        end
        print(io, " (", nts, '/', union_split, " union split)")
    end
end

function (::SoundPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    return report_non_boolean_cond!(analyzer, sv, t, #=sound=#true)
end

function (::BasicPass)(::Type{NonBooleanCondErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t))
    return basic_filter(analyzer, sv) && report_non_boolean_cond!(analyzer, sv, t, #=sound=#false)
end

function report_non_boolean_cond!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t),
    check_uncovered::Bool)
    if isa(t, Union)
        info = nothing
        uts = Base.uniontypes(t)
        for t in uts
            if !(check_uncovered ? t ⊑ Bool : hasintersect(t, Bool))
                if info === nothing
                    info = Any[], length(uts)
                end
                push!(info[1], t)
            end
        end
        if info !== nothing
            add_new_report!(analyzer, sv.result, NonBooleanCondErrorReport(sv, info..., #=uncovered=#check_uncovered))
            return true
        end
    else
        if !(check_uncovered ? t ⊑ Bool : hasintersect(t, Bool))
            add_new_report!(analyzer, sv.result, NonBooleanCondErrorReport(sv, t, 0, #=uncovered=#check_uncovered))
            return true
        end
    end
    return false
end

function (::SoundBasicPass)(::Type{InvalidConstantRedefinition}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    add_new_report!(analyzer, sv.result, InvalidConstantRedefinition(sv, mod, name, prev_t, t))
    return true
end
function (::SoundBasicPass)(::Type{InvalidConstantDeclaration}, analyzer::JETAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(analyzer, sv.result, InvalidConstantDeclaration(sv, mod, name))
    return true
end

"""
    SeriousExceptionReport <: InferenceErrorReport

Represents a "serious" error that is manually thrown by a `throw` call.
This is reported regardless of whether it's caught by control flow or not, as opposed to
[`UncaughtExceptionReport`](@ref).
"""
@jetreport struct SeriousExceptionReport <: InferenceErrorReport
    @nospecialize err
    # keeps the location where this exception is raised
    # this information will be used later when collecting `UncaughtExceptionReport`s
    # in order to avoid duplicated reports from the same `throw` call
    loc::LineInfoNode
end
function JETInterface.print_report_message(io::IO, (; err)::SeriousExceptionReport)
    s = with_bufferring(io->showerror(io, err))
    print(io, first(split(s, '\n')))
end

(::BasicPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    basic_filter(analyzer, sv) && report_serious_exception!(analyzer, sv, argtypes)
(::SoundPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    report_serious_exception!(analyzer, sv, argtypes) # any (non-serious) `throw` calls will be caught by the report pass for `UncaughtExceptionReport`
(::DefinitionAnalysisPass)(::Type{SeriousExceptionReport}, analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    false
function report_serious_exception!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes)
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            err = a.val
            if isa(err, UndefKeywordError)
                add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
                return true
            elseif isa(err, MethodError)
                # ignore https://github.com/JuliaLang/julia/blob/7409a1c007b7773544223f0e0a2d8aaee4a45172/base/boot.jl#L261
                if err.f !== Bottom
                    add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
                    return true
                end
            end
        elseif widenconst(a) <: ArgumentError
            # promote `ArgumentError` thrown by `to_index` method
            # so that we get reports from dangerous indexing (aviatesk/JET.jl#581)
            def = sv.linfo.def
            if isa(def, Method) && def.name === :to_index
                add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, ArgumentError("invalid index"), get_lin((sv, get_currpc(sv)))))
                return true
            end
        end
    end
    return false
end

"""
    AbstractBuiltinErrorReport

Represents errors caused by builtin-function calls.
Technically they're defined as those error points that can be caught within `Core.Compiler.builtin_tfunction`.
"""
abstract type AbstractBuiltinErrorReport <: InferenceErrorReport end

# TODO: docs
@jetreport struct BuiltinErrorReport <: AbstractBuiltinErrorReport
    @nospecialize f
    msg::AbstractString
end
JETInterface.print_report_message(io::IO, r::BuiltinErrorReport) = print(io, r.msg)
const GENERAL_BUILTIN_ERROR_MSG = "invalid builtin function call"

# report erroneous intrinsic function calls

"""
    IntrinsicError(reason::String)

A special lattice element that represents an erroneous intrinsic function call.
`IntrinsicError` is essentially equivalent to `Bottom` but isn't fully integrated with the
`AbstractLattice` system. It is a special marker object that is merely returned by `tfunc`s
for intrinsic calls that are overloaded with `IntrinsicErrorCheckLattice` to generate
`BuiltinErrorReport` with an appropriate error message, and it should be replaced with
`Bottom` before it is propagated to other abstract interpretation routines.
"""
struct IntrinsicError
    reason::String
end

@nospecs function with_conversion_errorcheck(t, x, bitshift::Bool=false)
    ty, isexact, isconcrete = instanceof_tfunc(t)
    if isconcrete
        if !isprimitivetype(ty)
            if bitshift
                return IntrinsicError("target type not a leaf primitive type")
            else
                return IntrinsicError("type is not a primitive type")
            end
        end
    end
    xty = widenconst(x)
    if isconcretetype(xty)
        if !isprimitivetype(xty)
            return IntrinsicError("value is not a primitive type")
        end
        if bitshift && isconcrete
            if Core.sizeof(ty) !== Core.sizeof(xty)
                return IntrinsicError("argument size does not match size of target type")
            end
        end
    end
    return ty
end
@nospecs function with_intrinsic_errorcheck(ok, a)
    aty = widenconst(a)
    if isconcretetype(aty)
        isprimitivetype(aty) || return IntrinsicError("value is not a primitive type")
    end
    return ok
end
@nospecs function with_intrinsic_errorcheck(ok, a, b, shift::Bool=false)
    aty = widenconst(a)
    if isconcretetype(aty)
        isprimitivetype(aty) || return IntrinsicError("a is not a primitive type")
    end
    bty = widenconst(b)
    if isconcretetype(bty)
        isprimitivetype(bty) || return IntrinsicError("b is not a primitive type")
    end
    shift || hasintersect(aty, bty) || return IntrinsicError("types of a and b must match")
    return ok
end
@nospecs function with_intrinsic_errorcheck(ok, a, b, c)
    aty = widenconst(a)
    if isconcretetype(aty)
        isprimitivetype(aty) || return IntrinsicError("a is not a primitive type")
    end
    bty = widenconst(b)
    if isconcretetype(bty)
        isprimitivetype(bty) || return IntrinsicError("b is not a primitive type")
    end
    cty = widenconst(c)
    if isconcretetype(cty)
        isprimitivetype(cty) || return IntrinsicError("c is not a primitive type")
    end
    hasintersect(aty, bty) || return IntrinsicError("types of a and b must match")
    hasintersect(bty, cty) || return IntrinsicError("types of b and c must match")
    return ok
end

@nospecs CC.bitcast_tfunc(𝕃::IntrinsicErrorCheckLattice, t, x) = with_conversion_errorcheck(t, x, #=bitshift=#true)
@nospecs CC.conversion_tfunc(𝕃::IntrinsicErrorCheckLattice, t, x) = with_conversion_errorcheck(t, x)
@nospecs CC.math_tfunc(𝕃::IntrinsicErrorCheckLattice, a, bs...) = with_intrinsic_errorcheck(widenconst(a), a, bs...)
@nospecs CC.shift_tfunc(𝕃::IntrinsicErrorCheckLattice, a, b) = with_intrinsic_errorcheck(widenconst(a), a, b, #=shift=#true)
@nospecs CC.cmp_tfunc(𝕃::IntrinsicErrorCheckLattice, a, b) = with_intrinsic_errorcheck(Bool, a, b, #=shift=#true)
@nospecs CC.chk_tfunc(𝕃::IntrinsicErrorCheckLattice, a, b) = with_intrinsic_errorcheck(Tuple{widenconst(a),Bool}, a, b, #=shift=#true)

function (::BasicPass)(::Type{AbstractBuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    @assert !(f === throw) "`throw` calls should be handled either by the report pass of `SeriousExceptionReport` or `UncaughtExceptionReport`"
    if f === getfield
        report_getfield!(analyzer, sv, argtypes, ret) && return true
    elseif f === setfield!
        report_setfield!!(analyzer, sv, argtypes, ret) && return true
    elseif f === fieldtype
        report_fieldtype!(analyzer, sv, argtypes, ret) && return true
    elseif f === getglobal
        report_getglobal!(analyzer, sv, argtypes, ret) && return true
    elseif f === setglobal!
        report_setglobal!!(analyzer, sv, argtypes) && return true
    elseif length(argtypes) == 2 && is_division_func(f)
        report_divide_error!(analyzer, sv, f, argtypes) && return true
    end
    if ret isa IntrinsicError
        msg = LazyString(f, ": ", ret.reason)
        report = BuiltinErrorReport(sv, f, msg)
        add_new_report!(analyzer, sv.result, report)
        return true
    end
    return handle_invalid_builtins!(analyzer, sv, f, argtypes, ret)
end

function (::TypoPass)(::Type{AbstractBuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    if f === getfield
        report_getfield!(analyzer, sv, argtypes, ret) && return true
    elseif f === getglobal
        report_getglobal!(analyzer, sv, argtypes, ret) && return true
    elseif f === setglobal!
        report_setglobal!!(analyzer, sv, argtypes) && return true
    end
    return false
end

function report_getfield!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    if ret === Any
        report_getglobal!(analyzer, sv, argtypes) && return true
    elseif ret === Bottom
        report_fieldaccess!(analyzer, sv, getfield, argtypes) && return true
    end
    return false
end

report_getglobal!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret)) =
    ret === Any && report_getglobal!(analyzer, sv, argtypes)
function report_getglobal!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes)
    2 ≤ length(argtypes) ≤ 3 || return false
    gr = constant_globalref(argtypes)
    gr === nothing && return false
    # forward to the report pass for undefined global reference
    return ReportPass(analyzer)(UndefVarErrorReport, analyzer, sv, gr)
end

function report_setfield!!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    if ret === Bottom
        report_fieldaccess!(analyzer, sv, setfield!, argtypes) && return true
    end
    return false
end

function report_fieldtype!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, @nospecialize(ret))
    if ret === Bottom
        report_fieldaccess!(analyzer, sv, fieldtype, argtypes) && return true
    end
    return false
end

function report_setglobal!!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes)
    3 ≤ length(argtypes) ≤ 4 || return false
    gr = constant_globalref(argtypes)
    gr === nothing && return false
    # forward to the report pass for invalid global assignment
    return ReportPass(analyzer)(InvalidGlobalAssignmentError, analyzer, sv, gr.mod, gr.name, argtypes[3])
end

using Core.Compiler: _getfield_fieldindex, _mutability_errorcheck

const MODULE_SETFIELD_MSG = "cannot assign variables in other modules"
const DIVIDE_ERROR_MSG = sprint(showerror, DivideError())
@nospecs type_error_msg(f, expected, actual) =
    lazy"TypeError: in $f, expected $expected, got a value of type $actual"
function field_error_msg(@nospecialize(typ), name::Symbol)
    if typ <: Tuple
        typ = Tuple # reproduce base error message
    end
    tname = nameof(typ::Union{DataType,UnionAll})
    return lazy"type $tname has no field $name"
end
function bounds_error_msg(@nospecialize(typ), name::Int)
    return lazy"BoundsError: attempt to access $typ at index [$name]"
end

function report_fieldaccess!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes)
    2 ≤ length(argtypes) ≤ 3 || return false

    issetfield! = f === setfield!
    obj, name = argtypes[1], argtypes[2]
    s00 = widenconst(obj)

    if issetfield!
        if !_mutability_errorcheck(s00)
            msg = lazy"setfield!: immutable struct of type $s00 cannot be changed"
            report = BuiltinErrorReport(sv, setfield!, msg)
            add_new_report!(analyzer, sv.result, report)
            return true
        end
    end

    isa(name, Const) || return false
    s = Base.unwrap_unionall(s00)
    if CC.isType(s)
        if f === fieldtype
            # XXX this is a hack to share more code between `getfield`/`setfield!`/`fieldtype`
            s00 = s = s.parameters[1]
        elseif CC.isconstType(s)
            s = (s00::DataType).parameters[1]
        else
            return false
        end
    end
    isa(s, DataType) || return false
    isabstracttype(s) && return false
    if s <: Module
        if issetfield!
            report = BuiltinErrorReport(sv, setfield!, MODULE_SETFIELD_MSG)
            add_new_report!(analyzer, sv.result, report)
            return true
        end
        nametyp = widenconst(name)
        if !hasintersect(nametyp, Symbol)
            msg = type_error_msg(getglobal, Symbol, nametyp)
            report = BuiltinErrorReport(sv, getglobal, msg)
            add_new_report!(analyzer, sv.result, report)
            return true
        end
    end
    fidx = _getfield_fieldindex(s, name)
    if fidx !== nothing
        nf = length(Base.datatype_fieldtypes(s))
        1 ≤ fidx ≤ nf && return false
    end

    namev = (name::Const).val
    objtyp = s00
    if namev isa Symbol
        msg = field_error_msg(objtyp, namev)
    elseif namev isa Int
        msg = bounds_error_msg(objtyp, namev)
    else
        @assert false "invalid field analysis"
    end
    add_new_report!(analyzer, sv.result, BuiltinErrorReport(sv, f, msg))
    return true
end

function is_division_func(@nospecialize f)
    return (f === Intrinsics.checked_sdiv_int ||
            f === Intrinsics.checked_srem_int ||
            f === Intrinsics.checked_udiv_int ||
            f === Intrinsics.checked_urem_int ||
            f === Intrinsics.sdiv_int ||
            f === Intrinsics.srem_int ||
            f === Intrinsics.udiv_int ||
            f === Intrinsics.urem_int)
end

# TODO this check might be better in its own report pass, say `NumericalPass`
function report_divide_error!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes)
    a = argtypes[2]
    t = widenconst(a)
    if isprimitivetype(t) && t <: Number
        if isa(a, Const) && a.val === zero(t)
            report = BuiltinErrorReport(sv, f, DIVIDE_ERROR_MSG)
            add_new_report!(analyzer, sv.result, report)
            return true
        end
    end
    return false
end

function handle_invalid_builtins!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    # we don't bail out using `basic_filter` here because the native tfuncs are already very permissive
    if ret === Bottom
        msg = GENERAL_BUILTIN_ERROR_MSG
        report = BuiltinErrorReport(sv, f, msg)
        add_new_report!(analyzer, sv.result, report)
        return true
    end
    return false
end

@jetreport struct UnsoundBuiltinErrorReport <: AbstractBuiltinErrorReport
    @nospecialize(f)
    argtypes::Argtypes
    msg::String = "this builtin function call may throw"
end
JETInterface.print_report_message(io::IO, r::UnsoundBuiltinErrorReport) = print(io, r.msg)

function (::SoundPass)(::Type{AbstractBuiltinErrorReport}, analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(rt))
    @assert !(f === throw) "`throw` calls should be handled either by the report pass of `SeriousExceptionReport` or `UncaughtExceptionReport`"
    if isa(f, IntrinsicFunction)
        nothrow = Core.Compiler.intrinsic_nothrow(f, argtypes)
    else
        nothrow = Core.Compiler.builtin_nothrow(CC.typeinf_lattice(analyzer), f, argtypes, rt)
    end
    nothrow && return false
    add_new_report!(analyzer, sv.result, UnsoundBuiltinErrorReport(sv, f, argtypes))
    return true
end

# entries
# =======

# the entry constructor
function JETAnalyzer(world::UInt = Base.get_world_counter();
    report_pass::Union{Nothing,ReportPass} = nothing,
    mode::Symbol = :basic,
    jetconfigs...)
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        if mode === :basic
            report_pass = BasicPass()
        elseif mode === :sound
            report_pass = SoundPass()
        elseif mode === :typo
            report_pass = TypoPass()
        else
            throw(JETConfigError("`mode` configuration should be either of `:basic`, `:sound` or `:typo`", :mode, mode))
        end
    elseif mode !== :basic
        throw(JETConfigError("Either of `report_pass` and `mode` configurations can be specified", :report_pass, report_pass))
    end
    jetconfigs = kwargs_dict(jetconfigs)
    set_if_missing!(jetconfigs, :aggressive_constant_propagation, true)
    set_if_missing!(jetconfigs, :unoptimize_throw_blocks, false)
    # Enable the `assume_bindings_static` option to terminate analysis a bit earlier when
    # there are undefined bindings detected. Note that this option will cause inference
    # cache inconsistency until JuliaLang/julia#40399 is merged. But the analysis cache of
    # JETAnalyzer has the same problem already anyway, so enabling this option does not
    # make the situation worse.
    set_if_missing!(jetconfigs, :assume_bindings_static, true)
    state = AnalyzerState(world; jetconfigs...)
    config = JETAnalyzerConfig(; jetconfigs...)
    return JETAnalyzer(state, report_pass, config)
end

const JET_ANALYZER_CONFIGURATIONS = Set{Symbol}((
    :report_pass, :mode, :ignore_missing_comparison))

let valid_keys = GENERAL_CONFIGURATIONS ∪ JET_ANALYZER_CONFIGURATIONS
    @eval JETInterface.valid_configurations(::JETAnalyzer) = $valid_keys
end

# interactive
# -----------

"""
    report_call(f, [types]; jetconfigs...) -> JETCallResult
    report_call(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult
    report_call(mi::Core.MethodInstance; jetconfigs...) -> JETCallResult

Analyzes a function call with the given type signature to find type-level errors
and returns back detected problems.

The [general configurations](@ref) and [the error analysis specific configurations](@ref jetanalysis-config)
can be specified as a keyword argument.

See [the documentation of the error analysis](@ref jetanalysis) for more details.
"""
function report_call(args...; jetconfigs...)
    analyzer = JETAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end

"""
    @report_call [jetconfigs...] f(args...)

Evaluates the arguments to a function call, determines their types, and then calls
[`report_call`](@ref) on the resulting expression.
This macro works in a similar way as the `@code_typed` macro.

The [general configurations](@ref) and [the error analysis specific configurations](@ref jetanalysis-config)
can be specified as an optional argument.
"""
macro report_call(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_call, ex0)
end

# Test.jl integration
# -------------------

"""
    @test_call [jetconfigs...] [broken=false] [skip=false] f(args...)

Runs [`@report_call jetconfigs... f(args...)`](@ref @report_call) and tests that the function
call `f(args...)` is free from problems that `@report_call` can detect.
Returns a `Pass` result if the test is successful, a `Fail` result if any problems are detected,
or an `Error` result if the test encounters an unexpected error.
When the test `Fail`s, abstract call stack to each problem location will be printed to `stdout`.
```julia-repl
julia> @test_call sincos(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call sincos(10)
```

As with [`@report_call`](@ref), the [general configurations](@ref) and
[the error analysis specific configurations](@ref jetanalysis-config)
can be specified as an optional argument:
```julia-repl
julia> cond = false

julia> function f(n)
           # `cond` is untyped, and will be reported by the sound analysis pass,
           # while JET's default analysis pass will ignore it
           if cond
               return sin(n)
           else
               return cos(n)
           end
       end;

julia> @test_call f(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call f(10)

julia> @test_call mode=:sound f(10)
JET-test failed at none:1
  Expression: #= none:1 =# JET.@test_call mode = :sound f(10)
  ═════ 1 possible error found ═════
  ┌ @ none:2 goto %4 if not cond
  │ non-boolean (Any) used in boolean context: goto %4 if not cond
  └──────────

ERROR: There was an error during testing
```

`@test_call` is fully integrated with [`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/)'s unit-testing infrastructure.
This means that the result of `@test_call` will be included in a final `@testset` summary
and it supports `skip` and `broken` annotations, just like the `@test` macro:
```julia-repl
julia> using JET, Test

# Julia can't propagate the type constraint `ref[]::Number` to `sin(ref[])`, JET will report `NoMethodError`
julia> f(ref) = isa(ref[], Number) ? sin(ref[]) : nothing;

# we can make it type-stable if we extract `ref[]` into a local variable `x`
julia> g(ref) = (x = ref[]; isa(x, Number) ? sin(x) : nothing);

julia> @testset "check errors" begin
           ref = Ref{Union{Nothing,Int}}(0)
           @test_call f(ref)             # fail
           @test_call g(ref)             # fail
           @test_call broken=true f(ref) # annotated as broken, thus still "pass"
       end
check errors: JET-test failed at REPL[21]:3
  Expression: #= REPL[21]:3 =# JET.@test_call f(ref)
  ═════ 1 possible error found ═════
  ┌ f(ref::Base.RefValue{Union{Nothing, Int64}}) @ Main ./REPL[19]:1
  │ no matching method found `sin(::Nothing)` (1/2 union split): sin((ref::Base.RefValue{Union{Nothing, Int64}})[]::Union{Nothing, Int64})
  └────────────────────

Test Summary: | Pass  Fail  Broken  Total  Time
check errors  |    1     1       1      3  0.2s
ERROR: Some tests did not pass: 1 passed, 1 failed, 0 errored, 1 broken.
```
"""
macro test_call(ex0...)
    return call_test_ex(:report_call, Symbol("@test_call"), ex0, __module__, __source__)
end

"""
    test_call(f, [types]; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_call(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_call`](@ref) on a function call with the given type signature and tests that
it is free from problems that `report_call` can detect.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_call`](@ref).
"""
function test_call(args...; jetconfigs...)
    return func_test(report_call, :test_call, args...; jetconfigs...)
end

# top-level
# ---------

"""
    report_file(file::AbstractString; jetconfigs...) -> JETToplevelResult

Analyzes `file` to find type-level errors and returns back detected problems.

This function looks for `$CONFIG_FILE_NAME` configuration file in the directory of `file`,
and searches _upward_ in the file tree until a `$CONFIG_FILE_NAME` is (or isn't) found.
When found, the configurations specified in the file are applied.
See [JET's configuration file specification](@ref config-file) for more details.

The [general configurations](@ref) and [the error analysis specific configurations](@ref jetanalysis-config)
can be specified as a keyword argument, and if given, they are preferred over the configurations
specified by a `$CONFIG_FILE_NAME` configuration file.

!!! tip
    When you want to analyze your package but no files that actually use its functions are
    available, [the `analyze_from_definitions` option](@ref toplevel-config) may be useful
    since it allows JET to analyze methods based on their declared signatures.
    For example, JET can analyze JET itself in this way:
    ```julia-repl
    # from the root directory of JET.jl
    julia> report_file("src/JET.jl";
                       analyze_from_definitions = true)
    ```
    See also [`report_package`](@ref).

!!! note
    This function enables the `toplevel_logger` configuration with the default logging level
    by default. You can still explicitly specify and configure it:
    ```julia
    report_file(args...;
                toplevel_logger = nothing, # suppress the toplevel logger
                jetconfigs...) # other configurations
    ```
    See [JET's top-level analysis configurations](@ref toplevel-config) for more details.
"""
function report_file(args...; jetconfigs...)
    # TODO read a configuration file and apply it here?
    analyzer = JETAnalyzer(; jetconfigs...)
    return analyze_and_report_file!(analyzer, args...; jetconfigs...)
end

"""
    test_file(file::AbstractString; jetconfigs...)

Runs [`report_file`](@ref) and tests that there are no problems detected.

As with [`report_file`](@ref), the [general configurations](@ref) and
[the error analysis specific configurations](@ref jetanalysis-config)
can be specified as an optional argument.

Like [`@test_call`](@ref), `test_file` is fully integrated with the
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/).
See [`@test_call`](@ref) for the details.
"""
function test_file(args...; jetconfigs...)
    return func_test(report_file, :test_file, args...; jetconfigs...)
end

"""
    report_package(package::Module; jetconfigs...) -> JETToplevelResult
    report_package(package::AbstractString; jetconfigs...) -> JETToplevelResult

Analyzes `package` in the same way as [`report_file`](@ref) and returns back type-level errors
with the special default configurations, which are especially tuned for analyzing a package
(see below for details).
The `package` argument can be either a `Module` or a `AbstractString`.
In the latter case it must be the name of a package in your current environment.

The error analysis performed by this function is configured as follows by default:
- `analyze_from_definitions = true`: This allows JET to start analysis without top-level
  call sites. This is useful for analyzing a package since a package itself usually only
  contains definitions of types and methods but not their usages (i.e. call sites).
- `concretization_patterns = [:(x_)]`: Concretizes every top-level code in a given `package`.
  The concretizations are generally preferred for successful analysis as far as they can be
  performed cheaply. In most cases it is indeed cheap to interpret and concretize top-level
  code written in a package since it usually only defines types and methods.
- `ignore_missing_comparison = true`: JET ignores the possibility of a poorly-inferred
  comparison operator call (e.g. `==`) returning `missing`. This is useful because
  `report_package` often relies on poor input argument type information at the beginning of
  analysis, leading to noisy error reports from branching on the potential `missing` return
  value of such a comparison operator call. If a target package needs to handle `missing`,
  this  configuration shuold be turned off since it hides the possibility of errors that
  may actually at runtime.

See [`ToplevelConfig`](@ref) and [`JETAnalyzer`](@ref) for more details.

Still the [general configurations](@ref) and [the error analysis specific configurations](@ref jetanalysis-config)
can be specified as a keyword argument, and if given, they are preferred over the default
configurations described above.

---

    report_package(; jetconfigs...) -> JETToplevelResult

Like above but analyzes the package of the current project.

See also [`report_file`](@ref).
"""
function report_package(args...; ignore_missing_comparison::Bool=true, jetconfigs...)
    # TODO read a configuration file and apply it here?
    analyzer = JETAnalyzer(; ignore_missing_comparison, jetconfigs...)
    return analyze_and_report_package!(analyzer, args...; ignore_missing_comparison, jetconfigs...)
end

"""
    test_package(package::Module; jetconfigs...)
    test_package(package::AbstractString; jetconfigs...)
    test_package(; jetconfigs...)

Runs [`report_package`](@ref) and tests that there are no problems detected.

As with [`report_package`](@ref), the [general configurations](@ref) and
[the error analysis specific configurations](@ref jetanalysis-config)
can be specified as an optional argument.

Like [`@test_call`](@ref), `test_package` is fully integrated with the
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/).
See [`@test_call`](@ref) for the details.

```julia
julia> @testset "test_package" begin
           test_package("Example"; toplevel_logger=nothing)
       end;
Test Summary: | Pass  Total  Time
test_package  |    1      1  0.0s
```
"""
function test_package(args...; toplevel_logger=nothing, jetconfigs...)
    return func_test(report_package, :test_package, args...; toplevel_logger, jetconfigs...)
end

"""
    report_text(text::AbstractString; jetconfigs...) -> JETToplevelResult
    report_text(text::AbstractString, filename::AbstractString; jetconfigs...) -> JETToplevelResult

Analyzes top-level `text` and returns back type-level errors.
"""
function report_text(args...; jetconfigs...)
    analyzer = JETAnalyzer(; jetconfigs...)
    return analyze_and_report_text!(analyzer, args...; jetconfigs...)
end

"""
    test_text(text::AbstractString; jetconfigs...)
    test_text(text::AbstractString, filename::AbstractString; jetconfigs...)

Runs [`report_text`](@ref) and tests that there are no problems detected.

As with [`report_text`](@ref), the [general configurations](@ref) and
[the error analysis specific configurations](@ref jetanalysis-config)
can be specified as an optional argument.

Like [`@test_call`](@ref), `test_text` is fully integrated with the
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/).
See [`@test_call`](@ref) for the details.
"""
function test_text(args...; jetconfigs...)
    return func_test(report_text, :test_text, args...; jetconfigs...)
end

"""
    watch_file(file::AbstractString; jetconfigs...)

Watches `file` and keeps re-triggering analysis with [`report_file`](@ref) on code update.
JET will try to analyze all the `include`d files reachable from `file`, and it will
re-trigger analysis if there is code update detected in any of the `include`d files.

This entry point currently uses [Revise.jl](https://timholy.github.io/Revise.jl/stable/) to
monitor code updates, and _can only be used after Revise has been loaded into the session_.
So note that you'll need to have run e.g., `using Revise` at some earlier stage to use it.
Revise offers possibilities to track changes in files that are not directly analyzed by JET,
including changes made to `Base` files using configurations like `revise_modules = [Base]`.
See [watch configurations](@ref watch-config) for more details.

!!! warning
    This interface is very experimental and likely to subject to change or removal without notice.

See also [`report_file`](@ref).
"""
watch_file(args...; jetconfigs...) = watch_file_with_func(report_file, args...; jetconfigs...)
