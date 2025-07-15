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
  - `mode = :sound`: the sound error analysis pass.
    If this pass doesn't report any errors, then your program is assured to run without
    any runtime errors (unless JET's error definition is not accurate and/or there is an
    implementation flaw).\\
  - `mode = :typo`: a typo detection pass
    A simple analysis pass to detect "typo"s in your program.
    This analysis pass is essentially a subset of the default basic pass,
    and it only reports undefined global reference and undefined field access.
    This might be useful especially for a very complex code base, because even the basic pass
    tends to be too noisy (spammed with too many errors) for such a case.

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
abstract type JETAnalyzer <: ToplevelAbstractAnalyzer end

struct BasicJETAnalyzer <: JETAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    method_table::CachedMethodTable{OverlayMethodTable}
    config::JETAnalyzerConfig
end

struct SoundJETAnalyzer <: JETAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    method_table::CachedMethodTable{OverlayMethodTable}
    config::JETAnalyzerConfig
end

struct TypoJETAnalyzer <: JETAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    method_table::CachedMethodTable{OverlayMethodTable}
    config::JETAnalyzerConfig
end

struct FromDefinitionJETAnalyzer <: JETAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    method_table::CachedMethodTable{OverlayMethodTable}
    config::JETAnalyzerConfig
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
function JETInterface.AbstractAnalyzer(analyzer::T, state::AnalyzerState) where T<:JETAnalyzer
    method_table = CachedMethodTable(OverlayMethodTable(state.world, JET_METHOD_TABLE))
    cache_key = compute_hash(state.inf_params, nameof(T), analyzer.config)
    analysis_token = get!(AnalysisToken, JET_ANALYZER_CACHE, cache_key)
    return T(state, analysis_token, method_table, analyzer.config)
end
JETInterface.AnalysisToken(analyzer::JETAnalyzer) = analyzer.analysis_token
JETInterface.typeinf_world(::BasicJETAnalyzer) = JET_TYPEINF_WORLD[]
JETInterface.typeinf_world(::SoundJETAnalyzer) = JET_TYPEINF_WORLD[]
JETInterface.typeinf_world(::TypoJETAnalyzer) = JET_TYPEINF_WORLD[]

const JET_ANALYZER_CACHE = Dict{UInt, AnalysisToken}()

JETAnalyzerConfig(analyzer::JETAnalyzer) = analyzer.config

# Analyzer-specific filters
# =========================

function basic_filter(analyzer::Union{BasicJETAnalyzer,FromDefinitionJETAnalyzer}, sv::InferenceState)
    mi = sv.linfo
    is_compileable_mi(mi) && return true
    return is_entry(analyzer, mi) # `report_call` may start analysis with abstract signature
end

# For sound analyzer, we use the same filter as basic for now
const SoundBasicAnalyzer = Union{SoundJETAnalyzer,BasicJETAnalyzer}

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

# analysis injections
# ===================

function CC.InferenceState(result::InferenceResult, cache_mode::UInt8, analyzer::JETAnalyzer)
    frame = @invoke CC.InferenceState(result::InferenceResult, cache_mode::UInt8, analyzer::ToplevelAbstractAnalyzer)
    if isnothing(frame) # indicates something bad happened within `retrieve_code_info`
        report_generator_error!(analyzer, result)
    end
    return frame
end

function CC.finish!(analyzer::JETAnalyzer, caller::InferenceState, validation_world::UInt, time_before::UInt64)
    src = caller.result.src

    if isnothing(src)
        # caught in cycle, similar error should have been reported where the source is available
    elseif src isa CodeInfo
        # report pass for uncaught `throw` calls
        report_uncaught_exception!(analyzer, caller, src.code)
    else
        # NOTE `src` never be `OptpimizationState` since `CC.may_optimize(::JETAnalyzer) === false`
        Core.eval(@__MODULE__, :(src = $src))
        throw("unexpected state happened, inspect `$(@__MODULE__).src`")
    end

    return @invoke CC.finish!(analyzer::ToplevelAbstractAnalyzer, caller::InferenceState, validation_world::UInt, time_before::UInt64)
end

function CC.abstract_call_gf_by_type(analyzer::JETAnalyzer,
    @nospecialize(func), arginfo::ArgInfo, si::StmtInfo, @nospecialize(atype), sv::InferenceState,
    max_methods::Int)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::ToplevelAbstractAnalyzer,
        func::Any, arginfo::ArgInfo, si::StmtInfo, atype::Any, sv::InferenceState, max_methods::Int)
    atype′ = Ref{Any}(atype)
    function after_abstract_call_gf_by_type(analyzer′::JETAnalyzer, sv′::InferenceState)
        ret′ = ret[]
        report_method_error!(analyzer′, sv′, ret′, arginfo.argtypes, atype′[])
        report_unanalyzed_call!(analyzer′, sv′, ret′, atype′[])
        return true
    end
    if isready(ret)
        after_abstract_call_gf_by_type(analyzer, sv)
    else
        push!(sv.tasks, after_abstract_call_gf_by_type)
    end
    return ret
end

function CC.from_interprocedural!(analyzer::JETAnalyzer,
    @nospecialize(rt), sv::InferenceState, arginfo::ArgInfo, @nospecialize(maybecondinfo))
    ret = @invoke CC.from_interprocedural!(analyzer::ToplevelAbstractAnalyzer,
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
    $CC.bail_out_call(analyzer::JETAnalyzer, ...)

This overload makes call inference performed by `JETAnalyzer` not bail out even when
inferred return type grows up to `Any` to collect as much error reports as possible.
That potentially slows down inference performance, but it would stay to be practical
given that the number of matching methods are limited beforehand.
"""
CC.bail_out_call(::JETAnalyzer, ::CC.InferenceLoopState, ::InferenceState) = false

# TODO Reasons about error found by [semi-]concrete evaluation:
# For now JETAnalyzer allows the regular constant-prop' only,
# unless the analyzed effects are proven to be `:nothrow`.
function CC.concrete_eval_eligible(analyzer::JETAnalyzer,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
    if CC.is_nothrow(result.effects)
        neweffects = CC.Effects(result.effects; nonoverlayed=CC.ALWAYS_TRUE)
        newresult = MethodCallResult(result.rt, result.exct, neweffects, result.edge,
                                     result.edgecycle, result.edgelimited,
                                     result.volatile_inf_result)
        res = @invoke CC.concrete_eval_eligible(analyzer::ToplevelAbstractAnalyzer,
            f::Any, newresult::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
        if res === :concrete_eval
            return :concrete_eval
        end
    else
        if (f === Base.fieldindex ||
            f === Base.typejoin ||
            f === Base.typejoin_union_tuple)
            if concrete_eval_eligible_ignoring_overlay(result, arginfo)
                return :concrete_eval
            end
        end
    end
    # disables both concrete evaluation and semi-concrete interpretation
    return :none
end

function concrete_eval_eligible_ignoring_overlay(result::MethodCallResult, arginfo::ArgInfo)
    result.edge !== nothing || return false
    return CC.is_foldable(result.effects) && CC.is_all_const_arg(arginfo, #=start=#2)
end

function CC.abstract_invoke(analyzer::JETAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState)
    ret = @invoke CC.abstract_invoke(analyzer::ToplevelAbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState)
    function after_abstract_invoke(analyzer′::JETAnalyzer, sv′::InferenceState)
        ret′ = ret[]
        report_invalid_invoke!(analyzer′, sv′, ret′, arginfo.argtypes)
        return true
    end
    if isready(ret)
        after_abstract_invoke(analyzer, sv)
    else
        push!(sv.tasks, after_abstract_invoke)
    end
    return ret
end

# inject report pass for undefined static parameter
function CC.abstract_eval_statement_expr(analyzer::JETAnalyzer, e::Expr, sstate::StatementState, sv::InferenceState)
    ret = @invoke CC.abstract_eval_statement_expr(analyzer::ToplevelAbstractAnalyzer, e::Expr, sstate::StatementState, sv::InferenceState)
    if e.head === :static_parameter
        function after_abstract_eval_statement_expr(analyzer′::JETAnalyzer, sv′::InferenceState)
            ret′ = ret[]
            report_undef_static_param!(analyzer′, sv′, e.args[1]::Int)
            return true
        end
        if isready(ret)
            after_abstract_eval_statement_expr(analyzer, sv)
        else
            push!(sv.tasks, after_abstract_eval_statement_expr)
        end
    end
    return ret
end

# inject report pass for undefined local variables
function CC.abstract_eval_special_value(analyzer::JETAnalyzer, @nospecialize(e), sstate::StatementState, sv::InferenceState)
    if e isa SlotNumber
        vtypes = sstate.vtypes
        if vtypes !== nothing
            report_undef_local_var!(analyzer, sv, e, vtypes)
        end
    end
    return @invoke CC.abstract_eval_special_value(analyzer::ToplevelAbstractAnalyzer, e::Any, sstate::StatementState, sv::InferenceState)
end

function CC.abstract_eval_globalref(analyzer::JETAnalyzer, g::GlobalRef, saw_latestworld::Bool, sv::InferenceState)
    if saw_latestworld
        return CC.RTEffects(Any, Any, CC.generic_getglobal_effects)
    end
    (valid_worlds, ret) = CC.scan_leaf_partitions(analyzer, g, sv.world) do analyzer::JETAnalyzer, binding::Core.Binding, partition::Core.BindingPartition
        if partition.min_world ≤ sv.world.this ≤ partition.max_world # XXX This should probably be fixed on the Julia side
            report_undef_global_var!(analyzer, sv, binding, partition)
        end
        CC.abstract_eval_partition_load(analyzer, binding, partition)
    end
    CC.update_valid_age!(sv, valid_worlds)
    return ret
end

function CC.abstract_eval_setglobal!(analyzer::JETAnalyzer, sv::InferenceState, saw_latestworld::Bool,
                                     @nospecialize(M), @nospecialize(s), @nospecialize(v))
    ret = @invoke CC.abstract_eval_setglobal!(analyzer::AbstractInterpreter, sv::InferenceState, saw_latestworld::Bool,
                                              M::Any, s::Any, v::Any)
    report_global_assignment!(analyzer, sv, ret, M, s, v)
    return ret
end

function CC.abstract_eval_value(analyzer::JETAnalyzer, @nospecialize(e), sstate::StatementState, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::ToplevelAbstractAnalyzer, e::Any, sstate::StatementState, sv::InferenceState)

    # report non-boolean condition error
    stmt = get_stmt((sv, get_currpc(sv)))
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom
            report_non_boolean_cond!(analyzer, sv, t)
        end
    end

    return ret
end

function CC.abstract_throw(analyzer::JETAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    ft = popfirst!(argtypes)
    report_serious_exception!(analyzer, sv, argtypes)
    pushfirst!(argtypes, ft)
    return @invoke CC.abstract_throw(analyzer::ToplevelAbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
end

function CC.builtin_tfunction(analyzer::JETAnalyzer,
    @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::ToplevelAbstractAnalyzer,
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

    report_builtin_error!(analyzer, sv, f, argtypes, ret)

    # `IntrinsicError` is a special marker object that JET uses to indicate an erroneous
    # intrinsic function call, so fix it up here to `Bottom`
    if ret isa IntrinsicError
        ret = Bottom
    end

    return ret
end

"""
    bail_out_toplevel_call(analyzer::JETAnalyzer, ...)

This overload allows JET to keep inference performed by `JETAnalyzer` going on
non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
CC.bail_out_toplevel_call(::JETAnalyzer, ::InferenceState) = false

# analysis
# ========

@jetreport struct GeneratorErrorReport <: InferenceErrorReport
    @nospecialize err # actual error wrapped
end
JETInterface.print_report_message(io::IO, rep::GeneratorErrorReport) = showerror(io, rep.err)

# XXX what's the "soundness" of a `@generated` function ?
# adapted from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
report_generator_error!(::JETAnalyzer, ::InferenceResult) = nothing
report_generator_error!(analyzer::BasicJETAnalyzer, result::InferenceResult) = _report_generator_error!(analyzer, result)
report_generator_error!(analyzer::SoundJETAnalyzer, result::InferenceResult) = _report_generator_error!(analyzer, result)
report_generator_error!(analyzer::FromDefinitionJETAnalyzer, result::InferenceResult) = _report_generator_error!(analyzer, result)
function _report_generator_error!(analyzer::JETAnalyzer, result::InferenceResult)
    mi = result.linfo
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(analyzer, linfo) XXX doesn't work
        CC.may_invoke_generator(mi) || return false
        world = CC.get_inference_world(analyzer)
        try
            @ccall jl_code_for_staged(mi::Any, world::UInt)::Any
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
# this analysis pass is very special, since
# 1.) it's tightly bound to the report pass of `SeriousExceptionReport` and
# 2.) it involves "report filtering" on its own
report_uncaught_exception!(::JETAnalyzer, ::InferenceState, ::Vector{Any}) = nothing
report_uncaught_exception!(analyzer::BasicJETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    _report_uncaught_exception_maybe!(analyzer, frame, stmts)
report_uncaught_exception!(analyzer::SoundJETAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    _report_uncaught_exception!(analyzer, frame, stmts)
report_uncaught_exception!(analyzer::FromDefinitionJETAnalyzer, frame::InferenceState, ::Vector{Any}) =
    filter_maybe_throws!(analyzer, frame)

function _report_uncaught_exception_maybe!(analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any})
    if frame.bestguess === Bottom
        return _report_uncaught_exception!(analyzer, frame, stmts)
    else
        filter_maybe_throws!(analyzer, frame)
        return false
    end
end
function filter_maybe_throws!(analyzer::JETAnalyzer, frame::InferenceState)
    # the non-`Bottom` result may mean `throw` calls from the children frames
    # (if exists) are caught and not propagated here
    # we don't want to cache the caught `UncaughtExceptionReport`s for this frame and
    # its parents, and just filter them away now
    filter!(get_reports(analyzer, frame.result)) do @nospecialize(report::InferenceErrorReport)
        return !isa(report, UncaughtExceptionReport)
    end
end
function _report_uncaught_exception!(analyzer::JETAnalyzer, frame::InferenceState, stmts::Vector{Any})
    # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
    # `throw` calls
    # XXX it's possible that the `throw` calls within them are all caught but the other
    # critical errors still make the return type `Bottom`
    # NOTE to reduce the false positive cases described above, we count `throw` calls
    # after optimization, since it may have eliminated "unreachable" `throw` calls
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
        isexpr(stmt, :call) || continue
        isempty(stmt.args) && continue
        f = stmt.args[1]
        if f isa SSAValue
            f = stmts[f.id]
        end
        (f isa GlobalRef && f.name === :throw) || continue
        # if this `throw` is already reported, don't duplicate
        if !isnothing(reported_locs) && get_lin((frame, pc)) in reported_locs
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

report_method_error!(::JETAnalyzer, ::InferenceState, ::CallMeta, ::Argtypes, @nospecialize(atype)) = nothing
report_method_error!(analyzer::BasicJETAnalyzer, sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype)) =
    report_method_error!(analyzer, sv, call, argtypes, atype, #=sound=#false)
report_method_error!(analyzer::SoundJETAnalyzer, sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype)) =
    report_method_error!(analyzer, sv, call, argtypes, atype, #=sound=#true)
report_method_error!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype)) =
    report_method_error!(analyzer, sv, call, argtypes, atype, #=sound=#false)

function report_method_error!(analyzer::JETAnalyzer,
    sv::InferenceState, call::CallMeta, argtypes::Argtypes, @nospecialize(atype), sound::Bool)
    info = call.info
    if isa(info, ConstCallInfo)
        info = info.call
    end
    if !sound
        if isa(info, MethodMatchInfo) || isa(info, UnionSplitInfo)
            basic_filter(analyzer, sv) || return false
        end
    end
    if isa(info, MethodMatchInfo)
        return report_method_error!(analyzer, sv, info, atype, argtypes, call.rt, sound)
    elseif isa(info, UnionSplitInfo)
        return report_method_error_for_union_split!(analyzer, sv, info, argtypes, call.rt, sound)
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
    if CC.isempty(info.results)
        if !sound && report_reduce_empty_error!(analyzer, sv, argtypes)
            return true
        end
        report = MethodErrorReport(sv, atype, 0, #=uncovered=#false)
        add_new_report!(analyzer, sv.result, report)
        return true
    elseif sound && !CC.all(m->m.fully_covers, info.results)
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
    for (i, matchinfo) in enumerate(info.split)
        if CC.isempty(matchinfo.results)
            if isnothing(split_argtypes)
                split_argtypes = CC.switchtupleunion(typeinf_lattice(analyzer), argtypes)
            end
            argtypes′ = split_argtypes[i]::Vector{Any}
            if !sound && report_reduce_empty_error!(analyzer, sv, argtypes′)
                reported |= true
                continue
            end
            if empty_matches === nothing
                empty_matches = (Any[], length(info.split))
            end
            sig_n = argtypes_to_type(argtypes′)
            push!(empty_matches[1], sig_n)
        elseif sound && !CC.all(m->m.fully_covers, matchinfo.results)
            if isnothing(split_argtypes)
                split_argtypes = CC.switchtupleunion(typeinf_lattice(analyzer), argtypes)
            end
            argtypes′ = split_argtypes[i]::Vector{Any}
            if uncovered_matches === nothing
                uncovered_matches = (Any[], length(info.split))
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

@jetreport struct UnanalyzedCallReport <: InferenceErrorReport
    @nospecialize type
end
function JETInterface.print_report_message(io::IO, report::UnanalyzedCallReport)
    print(io, "unanalyzed method call ")
    print_callsig(io, report.type)
end

report_unanalyzed_call!(::JETAnalyzer, ::InferenceState, ::CallMeta, @nospecialize(atype)) = nothing
function report_unanalyzed_call!(analyzer::SoundJETAnalyzer,
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

report_invalid_invoke!(::JETAnalyzer, ::InferenceState, ::CallMeta, ::Argtypes) = nothing
report_invalid_invoke!(analyzer::BasicJETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes) =
    _report_invalid_invoke!(analyzer, sv, ret, argtypes)
report_invalid_invoke!(analyzer::SoundJETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes) =
    _report_invalid_invoke!(analyzer, sv, ret, argtypes)
report_invalid_invoke!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes) =
    _report_invalid_invoke!(analyzer, sv, ret, argtypes)

function _report_invalid_invoke!(analyzer::JETAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Argtypes)
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
    maybeundef::Bool
end
function JETInterface.print_report_message(io::IO, r::UndefVarErrorReport)
    var = r.var
    if isa(var, TypeVar) # TODO show "maybe undefined" case nicely?
        print(io, "`", var.name, "` not defined in static parameter matching")
    else
        if isa(var, GlobalRef)
            print(io, "`", var.mod, '.', var.name, "`")
        else
            print(io, "local variable `", var, "`")
        end
        if r.maybeundef
            print(io, " may be undefined")
        else
            print(io, " is not defined")
        end
    end
end

# global variable
# ---------------

# TODO InferenceParams(::JETAnalyzer).assume_bindings_static = true

report_undef_global_var!(::JETAnalyzer, ::InferenceState, ::Core.Binding, ::Core.BindingPartition) = nothing
report_undef_global_var!(analyzer::BasicJETAnalyzer, sv::InferenceState, binding::Core.Binding, partition::Core.BindingPartition) =
    _report_undef_global_var!(analyzer, sv, binding, partition, false)
report_undef_global_var!(analyzer::SoundJETAnalyzer, sv::InferenceState, binding::Core.Binding, partition::Core.BindingPartition) =
    _report_undef_global_var!(analyzer, sv, binding, partition, true)
report_undef_global_var!(analyzer::TypoJETAnalyzer, sv::InferenceState, binding::Core.Binding, partition::Core.BindingPartition) =
    _report_undef_global_var!(analyzer, sv, binding, partition, false)
report_undef_global_var!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, binding::Core.Binding, partition::Core.BindingPartition) =
    _report_undef_global_var!(analyzer, sv, binding, partition, false)

function _report_undef_global_var!(analyzer::JETAnalyzer, sv::InferenceState, binding::Core.Binding, partition::Core.BindingPartition, sound::Bool)
    gr = binding.globalref
    # TODO use `abstract_eval_isdefinedglobal` for respecting world age
    if @invokelatest isdefinedglobal(gr.mod, gr.name)
        # HACK/FIXME Concretize `AbstractBindingState`
        x = @invokelatest getglobal(gr.mod, gr.name)
        x isa AbstractBindingState || return false
        binding_state = x
    else
        binding_states = get_binding_states(analyzer)
        binding_state = get(binding_states, partition, nothing)
    end
    maybeundef = false
    if binding_state !== nothing
        if !binding_state.maybeundef
            return false
        end
        maybeundef = true
    end
    add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, gr, maybeundef))
    return true
end

# static parameter
# ----------------

report_undef_static_param!(::JETAnalyzer, ::InferenceState, ::Int) = nothing
report_undef_static_param!(analyzer::BasicJETAnalyzer, sv::InferenceState, n::Int) =
    _report_undef_static_param!(analyzer, sv, n, false)
report_undef_static_param!(analyzer::SoundJETAnalyzer, sv::InferenceState, n::Int) =
    _report_undef_static_param!(analyzer, sv, n, true)
report_undef_static_param!(analyzer::TypoJETAnalyzer, sv::InferenceState, n::Int) =
    _report_undef_static_param!(analyzer, sv, n, false)
report_undef_static_param!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, n::Int) =
    _report_undef_static_param!(analyzer, sv, n, false)

function _report_undef_static_param!(analyzer::JETAnalyzer, sv::InferenceState, n::Int, sound::Bool)
    if !(1 ≤ n ≤ length(sv.sptypes))
        add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, TypeVar(:unknown), false))
        return true
    end
    mi = sv.linfo
    if sv.sptypes[n].undef && (sound || is_compileable_mi(mi))
        tv = mi.sparam_vals[n]::TypeVar
        add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, tv, false))
        return true
    end
    return false
end

# local variable
# --------------

# TODO implement `sound` mode?
report_undef_local_var!(::JETAnalyzer, ::InferenceState, ::SlotNumber, ::VarTable) = nothing
report_undef_local_var!(analyzer::BasicJETAnalyzer, sv::InferenceState, var::SlotNumber, vtypes::VarTable) =
    _report_undef_local_var!(analyzer, sv, var, vtypes, false)
report_undef_local_var!(analyzer::SoundJETAnalyzer, sv::InferenceState, var::SlotNumber, vtypes::VarTable) =
    _report_undef_local_var!(analyzer, sv, var, vtypes, true)
report_undef_local_var!(analyzer::TypoJETAnalyzer, sv::InferenceState, var::SlotNumber, vtypes::VarTable) =
    _report_undef_local_var!(analyzer, sv, var, vtypes, false)
report_undef_local_var(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, var::SlotNumber, vtypes::VarTable) =
    _report_undef_local_var!(analyzer, sv, var, vtypes, false)

function _report_undef_local_var!(analyzer::JETAnalyzer, sv::CC.InferenceState, var::SlotNumber, vtypes::VarTable, sound::Bool)
    if isconcretized(analyzer, sv)
        return false # no need to be analyzed
    end
    vtype = vtypes[slot_id(var)]
    vtype.undef || return false
    if !is_constant_propagated(sv)
        if isempty(sv.ssavalue_uses[sv.currpc])
            # This case is when an undefined local variable is just declared,
            # but such cases can become reachable when constant propagation
            # for capturing closures doesn't occur.
            # In the future, improvements to the compiler should make such cases
            # unreachable in the first place, but for now we completely ignore
            # such cases to suppress false positives.
            return false
        end
    end
    name = get_slotname(sv, var)
    if name === Symbol("")
        # Such unnamed local variables are mainly introduced by `try/catch/finally` clauses.
        # Due to insufficient liveness analysis of the current compiler for such code,
        # the isdefined-ness of such variables may not be properly determined.
        # For the time being, until the compiler implementation is improved,
        # we ignore this case to suppress false positives.
        return false
    end
    maybeundef = vtype.typ !== Union{}
    add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, name, maybeundef))
    return true
end

@jetreport struct IncompatibleGlobalAssignmentError <: InferenceErrorReport
    mod::Module
    name::Symbol
end
JETInterface.print_report_message(io::IO, report::IncompatibleGlobalAssignmentError) =
    print(io, "cannot assign an incompatible value to the global ", report.mod, '.', report.name, '.')

report_global_assignment!(::JETAnalyzer, ::InferenceState, ::CallMeta, @nospecialize(M), @nospecialize(s), @nospecialize(v)) = nothing
report_global_assignment!(analyzer::BasicJETAnalyzer, sv::InferenceState, ret::CallMeta, @nospecialize(M), @nospecialize(s), @nospecialize(v)) =
    _report_global_assignment!(analyzer, sv, ret, M, s, v, false)
report_global_assignment!(analyzer::SoundJETAnalyzer, sv::InferenceState, ret::CallMeta, @nospecialize(M), @nospecialize(s), @nospecialize(v)) =
    _report_global_assignment!(analyzer, sv, ret, M, s, v, true)
report_global_assignment!(analyzer::TypoJETAnalyzer, sv::InferenceState, ret::CallMeta, @nospecialize(M), @nospecialize(s), @nospecialize(v)) =
    _report_global_assignment!(analyzer, sv, ret, M, s, v, false)
report_global_assignment!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, ret::CallMeta, @nospecialize(M), @nospecialize(s), @nospecialize(v)) =
    _report_global_assignment!(analyzer, sv, ret, M, s, v, false)

function _report_global_assignment!(analyzer::JETAnalyzer, sv::InferenceState, ret::CallMeta,
                                    @nospecialize(M), @nospecialize(s), @nospecialize(v),
                                    sound::Bool)
    if sound
        if ret.exct !== Union{}
            @goto report
        end
    else
        if ret.rt === Bottom && ret.exct === ErrorException
            @label report
            mod = name = nothing
            if M isa Const
                mod′ = M.val
                if mod′ isa Module
                    mod = mod′
                end
            end
            if s isa Const
                name′ = s.val
                if name′ isa Symbol
                    name = name′
                end
            end
            add_new_report!(analyzer, sv.result, IncompatibleGlobalAssignmentError(sv, @something(mod, Module(:Unknown)), @something(name, :unknown)))
        end
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

report_non_boolean_cond!(::JETAnalyzer, ::InferenceState, @nospecialize(t)) = nothing
report_non_boolean_cond!(analyzer::BasicJETAnalyzer, sv::InferenceState, @nospecialize(t)) =
    _report_non_boolean_cond!(analyzer, sv, t, false)
report_non_boolean_cond!(analyzer::SoundJETAnalyzer, sv::InferenceState, @nospecialize(t)) =
    _report_non_boolean_cond!(analyzer, sv, t, true)
report_non_boolean_cond!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, @nospecialize(t)) =
    _report_non_boolean_cond!(analyzer, sv, t, false)

function _report_non_boolean_cond!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(t), sound::Bool)
    if !sound
        basic_filter(analyzer, sv) || return nothing
    end
    check_uncovered = sound
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
        end
    else
        if !(check_uncovered ? t ⊑ Bool : hasintersect(t, Bool))
            add_new_report!(analyzer, sv.result, NonBooleanCondErrorReport(sv, t, 0, #=uncovered=#check_uncovered))
        end
    end
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

report_serious_exception!(::JETAnalyzer, ::InferenceState, ::Argtypes) = nothing
report_serious_exception!(analyzer::BasicJETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    _report_serious_exception!(analyzer, sv, argtypes, false)
report_serious_exception!(analyzer::SoundJETAnalyzer, sv::InferenceState, argtypes::Argtypes) =
    _report_serious_exception!(analyzer, sv, argtypes, true)

function _report_serious_exception!(analyzer::JETAnalyzer, sv::InferenceState, argtypes::Argtypes, sound::Bool)
    if !sound
        basic_filter(analyzer, sv) || return nothing
    end
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            err = a.val
            if isa(err, UndefKeywordError)
                add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
            elseif isa(err, MethodError)
                # ignore https://github.com/JuliaLang/julia/blob/7409a1c007b7773544223f0e0a2d8aaee4a45172/base/boot.jl#L261
                if err.f !== Bottom
                    add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, err, get_lin((sv, get_currpc(sv)))))
                end
            end
        elseif widenconst(CC.unwrapva(a)) <: ArgumentError
            # promote `ArgumentError` thrown by `to_index` method
            # so that we get reports from dangerous indexing (aviatesk/JET.jl#581)
            def = sv.linfo.def
            if isa(def, Method) && def.name === :to_index
                add_new_report!(analyzer, sv.result, SeriousExceptionReport(sv, ArgumentError("invalid index"), get_lin((sv, get_currpc(sv)))))
            end
        end
    end
end

@jetreport struct BuiltinErrorReport <: InferenceErrorReport
    @nospecialize f
    msg::AbstractString
end
JETInterface.print_report_message(io::IO, r::BuiltinErrorReport) = print(io, r.msg)
const GENERAL_BUILTIN_ERROR_MSG = "invalid builtin function call"

@jetreport struct UnsoundBuiltinErrorReport <: InferenceErrorReport
    @nospecialize(f)
    argtypes::Argtypes
    msg::String = "this builtin function call may throw"
end
JETInterface.print_report_message(io::IO, r::UnsoundBuiltinErrorReport) = print(io, r.msg)

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
    ty, _, isconcrete = instanceof_tfunc(t)
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

report_builtin_error!(analyzer::BasicJETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret)) =
    _report_builtin_error_basic!(analyzer, sv, f, argtypes, ret)
report_builtin_error!(analyzer::SoundJETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret)) =
    _report_builtin_error_sound!(analyzer, sv, f, argtypes, ret)
report_builtin_error!(analyzer::TypoJETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret)) =
    _report_builtin_error_typo!(analyzer, sv, f, argtypes, ret)
report_builtin_error!(analyzer::FromDefinitionJETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret)) =
    _report_builtin_error_basic!(analyzer, sv, f, argtypes, ret)

function _report_builtin_error_basic!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    if f === getfield
        report_getfield!(analyzer, sv, argtypes, ret) && return true
    elseif f === getglobal
        report_getglobal!(analyzer, sv, argtypes, ret) && return true
    elseif f === setfield!
        report_setfield!!(analyzer, sv, argtypes, ret) && return true
    elseif f === fieldtype
        report_fieldtype!(analyzer, sv, argtypes, ret) && return true
    # elseif f === setglobal!
    #     report_setglobal!!(analyzer, sv, argtypes, ret) && return true
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

function _report_builtin_error_typo!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(ret))
    if f === getfield
        report_getfield!(analyzer, sv, argtypes, ret) && return true
    elseif f === getglobal
        report_getglobal!(analyzer, sv, argtypes, ret) && return true
    elseif f === setfield!
        report_setfield!!(analyzer, sv, argtypes, ret) && return true
    # elseif f === setglobal!
    #     report_setglobal!!(analyzer, sv, argtypes, ret) && return true
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
    if @invokelatest isdefinedglobal(gr.mod, gr.name)
        # TODO use `abstract_eval_isdefinedglobal` for respecting world age
        return false
    end
    add_new_report!(analyzer, sv.result, UndefVarErrorReport(sv, gr, false))
    return true
end

function constant_globalref(argtypes::Vector{Any})
    length(argtypes) ≥ 2 || return nothing
    mod = argtypes[1]
    isa(mod, Const) || return nothing
    mod = mod.val
    isa(mod, Module) || return nothing
    sym = argtypes[2]
    isa(sym, Const) || return nothing
    sym = sym.val
    isa(sym, Symbol) || return nothing
    return GlobalRef(mod, sym)
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

using .CC: _getfield_fieldindex, _mutability_errorcheck

const MODULE_SETFIELD_MSG = "cannot assign variables in other modules"
const DIVIDE_ERROR_MSG = sprint(showerror, DivideError())
@nospecs type_error_msg(f, expected, actual) =
    lazy"TypeError: in $f, expected $expected, got a value of type $actual"
function field_error_msg(@nospecialize(typ), name::Symbol)
    typ = typ::Union{UnionAll,DataType}
    flds = join(map(n->"`$n`", fieldnames(typ)), ", ")
    if typ <: Tuple
        typ = Tuple # reproduce base error message
    end
    @static if VERSION ≥ v"1.12.0-beta4.14"
        # JuliaLang/julia#58507
        tname = string(typ.name.wrapper)
    else
        tname = nameof(typ)
    end
    return lazy"FieldError: type $tname has no field `$name`, available fields: $flds"
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
    objtyp = s
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

function _report_builtin_error_sound!(analyzer::JETAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Argtypes, @nospecialize(rt))
    if isa(f, IntrinsicFunction)
        nothrow = CC.intrinsic_nothrow(f, argtypes)
    else
        nothrow = CC.builtin_nothrow(CC.typeinf_lattice(analyzer), f, argtypes, rt)
    end
    nothrow && return false
    add_new_report!(analyzer, sv.result, UnsoundBuiltinErrorReport(sv, f, argtypes))
    return true
end

# entries
# =======

# the entry constructor
function JETAnalyzer(world::UInt = Base.get_world_counter();
                     mode::Symbol = :basic,
                     __cache_hash__::Any = nothing,
                     jetconfigs...)
    jetconfigs = kwargs_dict(jetconfigs)
    set_if_missing!(jetconfigs, :aggressive_constant_propagation, true)
    # Enable the `assume_bindings_static` option to terminate analysis a bit earlier when
    # there are undefined bindings detected. Note that this option will cause inference
    # cache inconsistency until JuliaLang/julia#40399 is merged. But the analysis cache of
    # JETAnalyzer has the same problem already anyway, so enabling this option does not
    # make the situation worse.
    jetconfigs[:assume_bindings_static] = true
    state = AnalyzerState(world; jetconfigs...)
    config = JETAnalyzerConfig(; jetconfigs...)
    method_table = CachedMethodTable(OverlayMethodTable(state.world, JET_METHOD_TABLE))

    # Create the appropriate analyzer type based on mode
    if mode === :basic
        cache_key = compute_hash(state.inf_params, BasicJETAnalyzer, config, __cache_hash__)
        analysis_token = get!(AnalysisToken, JET_ANALYZER_CACHE, cache_key)
        return BasicJETAnalyzer(state, analysis_token, method_table, config)
    elseif mode === :sound
        cache_key = compute_hash(state.inf_params, SoundJETAnalyzer, config, __cache_hash__)
        analysis_token = get!(AnalysisToken, JET_ANALYZER_CACHE, cache_key)
        return SoundJETAnalyzer(state, analysis_token, method_table, config)
    elseif mode === :typo
        cache_key = compute_hash(state.inf_params, TypoJETAnalyzer, config, __cache_hash__)
        analysis_token = get!(AnalysisToken, JET_ANALYZER_CACHE, cache_key)
        return TypoJETAnalyzer(state, analysis_token, method_table, config)
    else
        throw(JETConfigError("`mode` configuration should be either of `:basic`, `:sound` or `:typo`", :mode, mode))
    end
end

const JET_ANALYZER_CONFIGURATIONS = Set{Symbol}((
    :mode, :ignore_missing_comparison, :__cache_hash__))

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
    interp = JETConcreteInterpreter(JETAnalyzer(; jetconfigs...))
    return analyze_and_report_file!(interp, args...; jetconfigs...)
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
    interp = JETConcreteInterpreter(JETAnalyzer(; ignore_missing_comparison, jetconfigs...))
    return analyze_and_report_package!(interp, args...; ignore_missing_comparison, jetconfigs...)
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
    interp = JETConcreteInterpreter(JETAnalyzer(; jetconfigs...))
    return analyze_and_report_text!(interp, args...; jetconfigs...)
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
