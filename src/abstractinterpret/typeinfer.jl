# inter-procedural
# ================

function collect_callee_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    reports = get_report_stash(analyzer)
    if !isempty(reports)
        vf = get_virtual_frame(sv)
        for report in reports
            pushfirst!(report.vst, vf)
            add_new_report!(analyzer, sv.result, report)
        end
        empty!(reports)
    end
    return nothing
end

function collect_cached_callee_reports!(analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport},
                                        caller::InferenceState, origin_mi::MethodInstance)
    for cached in reports
        restored = add_cached_report!(analyzer, caller.result, cached)
        @static if JET_DEV_MODE
            actual, expected = first(restored.vst).linfo, origin_mi
            @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
        end
        stash_report!(analyzer, restored) # should be updated in `abstract_call_method_with_const_args`
    end
    return nothing
end

function CC.abstract_call_method(analyzer::AbstractAnalyzer,
    method::Method, @nospecialize(sig), sparams::SimpleVector,
    hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    ret = @invoke CC.abstract_call_method(analyzer::AbstractInterpreter,
        method::Method, sig::Any, sparams::SimpleVector,
        hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    function after_call_method(analyzer′, sv′)
        ret′ = ret[]
        collect_callee_reports!(analyzer′, sv′)
        return true
    end
    if isready(ret)
        after_call_method(analyzer, sv)
    else
        push!(sv.tasks, after_call_method)
    end
    return ret
end

function CC.const_prop_call(analyzer::AbstractAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
    concrete_eval_result::Union{Nothing,ConstCallResult})
    set_cache_target!(analyzer, :const_prop_call => sv)
    const_result = @invoke CC.const_prop_call(analyzer::AbstractInterpreter,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
        concrete_eval_result::Union{Nothing,ConstCallResult})
    @assert get_cache_target(analyzer) === nothing "invalid JET analysis state"
    if const_result !== nothing
        # successful constant prop', we need to update reports
        collect_callee_reports!(analyzer, sv)
    end
    return const_result
end

function CC.concrete_eval_call(analyzer::AbstractAnalyzer,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo,
    sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    ret = @invoke CC.concrete_eval_call(analyzer::AbstractInterpreter,
        f::Any, result::MethodCallResult, arginfo::ArgInfo,
        sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    if ret isa ConstCallResult
        # this frame has been concretized, now we throw away reports collected
        # during the previous non-constant, abstract-interpretation
        filter_lineages!(analyzer, sv.result, result.edge.def)
    end
    return ret
end

function CC.abstract_call_known(analyzer::AbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
    ret = @invoke CC.abstract_call_known(analyzer::AbstractInterpreter,
        f::Any, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
    f′ = Ref{Any}(f)
    function after_call_known(analyzer′, sv′)
        ret′ = ret[]
        analyze_task_parallel_code!(analyzer′, f′[], arginfo, sv′)
        return true
    end
    if isready(ret)
        after_call_known(analyzer, sv)
    else
        push!(sv.tasks, after_call_known)
    end
    return ret
end

"""
    analyze_task_parallel_code!(analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState)

Adds special cased analysis pass for task parallelism.
In Julia's task parallelism implementation, parallel code is represented as closure and it's
wrapped in a `Task` object. `Core.Compiler.NativeInterpreter` doesn't infer nor optimize the
bodies of those closures when compiling code that creates parallel tasks, but JET will try
to run additional analysis pass by recurring into the closures.

See also: <https://github.com/aviatesk/JET.jl/issues/114>

!!! note
    JET won't do anything other than doing JET analysis, e.g. won't annotate return type
    of wrapped code block in order to not confuse the original `AbstractInterpreter` routine
    track <https://github.com/JuliaLang/julia/pull/39773> for the changes in native abstract
    interpretation routine.
"""
function analyze_task_parallel_code!(analyzer::AbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, sv::InferenceState)
    # TODO we should analyze a closure wrapped in a `Task` only when it's `schedule`d
    # But the `Task` construction may not happen in the same frame where it's `schedule`d
    # and so we may not be able to access to the closure at that point.
    # As a compromise, here we invoke the additional analysis on `Task` construction,
    # regardless of whether it's really `schedule`d or not.
    f === Task || return nothing
    argtypes = arginfo.argtypes
    length(argtypes) ≥ 2 || return nothing
    v = argtypes[2]
    v ⊑ Function || return nothing
    # if we encounter `Task(::Function)`,
    # try to get its inner function and run analysis on it:
    # the closure can be a nullary lambda that really doesn't depend on
    # the captured environment, and in that case we can retrieve it as
    # a function object, otherwise we will try to retrieve the type of the closure
    if isa(v, Const)
        ft = Core.Typeof(v.val)
    elseif isa(v, Core.PartialStruct)
        ft = v.typ
    elseif isa(v, DataType)
        ft = v
    else
        return nothing
    end
    analyze_additional_pass_by_type!(analyzer, Tuple{ft}, sv)
end

# run additional interpretation with a new analyzer
function analyze_additional_pass_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt), sv::InferenceState)
    newanalyzer = AbstractAnalyzer(analyzer)

    # in order to preserve the inference termination, we keep to use the current frame
    # and borrow the `AbstractInterpreter`'s cycle detection logic
    # XXX the additional analysis pass by `abstract_call_method` may involve various site-effects,
    # but what we're doing here is essentially equivalent to modifying the user code and inlining
    # the threaded code block as a usual code block, and thus the side-effects won't (hopefully)
    # confuse the abstract interpretation, which is supposed to terminate on any kind of code
    match = find_single_match(tt, newanalyzer)
    CC.abstract_call_method(newanalyzer, match.method, match.spec_types, match.sparams,
        #=hardlimit=#false, #=si=#StmtInfo(false, false), sv)

    return nothing
end

# `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
# and here we should NOT catch error reports detected within the virtualized call
# because it is not abstraction of actual execution
function CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
    # stash and discard the result from the simulated call, and keep the original result (`result0`)
    result = sv.result
    oldresult = analyzer[result]
    init_result!(analyzer, result)
    newanalyzer = AbstractAnalyzer(analyzer)
    sv.interp = newanalyzer
    ret = @invoke CC.return_type_tfunc(newanalyzer::AbstractInterpreter, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
    sv.interp = analyzer
    analyzer[result] = oldresult
    return ret
end

# cache
# =====

cache_report!(cache::Vector{InferenceErrorReport}, @nospecialize report::InferenceErrorReport) =
    push!(cache, copy_report_stable(report))

struct AbstractAnalyzerView{Analyzer<:AbstractAnalyzer}
    analyzer::Analyzer
end

# global
# ------

CC.cache_owner(analyzer::AbstractAnalyzer) = AnalysisCache(analyzer)

function CC.code_cache(analyzer::AbstractAnalyzer)
    view = AbstractAnalyzerView(analyzer)
    worlds = WorldRange(CC.get_inference_world(analyzer))
    return WorldView(view, worlds)
end

AnalysisCache(wvc::WorldView{<:AbstractAnalyzerView}) = AnalysisCache(wvc.cache.analyzer)

CC.haskey(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance) = haskey(AnalysisCache(wvc), mi)

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::InferenceState,
                         edgecycle::Bool, edgelimited::Bool)
    set_cache_target!(analyzer, :typeinf_edge => caller)
    ret = @invoke CC.typeinf_edge(analyzer::AbstractInterpreter, method::Method, atype::Any, sparams::SimpleVector, caller::InferenceState,
                                  edgecycle::Bool, edgelimited::Bool)
    @assert get_cache_target(analyzer) === nothing "invalid JET analysis state"
    return ret
end

function CC.get(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance, default)
    codeinst = get(AnalysisCache(wvc), mi, default) # will ignore native code cache for a `MethodInstance` that is not analyzed by JET yet

    analyzer = wvc.cache.analyzer

    # XXX this relies on a very dirty analyzer state manipulation, the reason for this is
    # that this method (and `code_cache(::AbstractAnalyzer)`) can be called from multiple
    # contexts including edge inference, constant prop' heuristics and inlining, where we
    # want to use report cache only in edge inference, but we can't tell which context is
    # the caller of this specific method call here and thus can't tell whether we should
    # enable report cache reconstruction without the information
    # XXX move this logic into `typeinf_edge`?
    cache_target = get_cache_target(analyzer)
    if cache_target !== nothing
        context, caller = cache_target
        if context === :typeinf_edge
            if isa(codeinst, CodeInstance)
                # cache hit, now we need to append cached reports associated with this `MethodInstance`
                cached_reports = CC.traverse_analysis_results(codeinst) do @nospecialize analysis_result
                    analysis_result isa CachedAnalysisResult ? analysis_result.reports : nothing
                end
                cached_reports !== nothing &&
                    collect_cached_callee_reports!(analyzer, cached_reports, caller, mi)
            end
        end
        set_cache_target!(analyzer, nothing)
    end

    return codeinst
end

function CC.getindex(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance)
    codeinst = CC.get(wvc, mi, nothing)
    codeinst === nothing && throw(KeyError(mi))
    return codeinst::CodeInstance
end

function CC.setindex!(wvc::WorldView{<:AbstractAnalyzerView}, codeinst::CodeInstance, mi::MethodInstance)
    return AnalysisCache(wvc)[mi] = codeinst
end

# local
# -----

CC.get_inference_cache(analyzer::AbstractAnalyzer) = AbstractAnalyzerView(analyzer)

function CC.cache_lookup(𝕃ᵢ::CC.AbstractLattice, mi::MethodInstance, given_argtypes::Argtypes, view::AbstractAnalyzerView)
    # XXX the very dirty analyzer state observation again
    # this method should only be called from the single context i.e. `abstract_call_method_with_const_args`,
    # and so we should reset the cache target immediately we reach here
    analyzer = view.analyzer
    cache_target = get_cache_target(analyzer)
    set_cache_target!(analyzer, nothing)

    inf_result = CC.cache_lookup(𝕃ᵢ, mi, given_argtypes, get_inf_cache(view.analyzer))

    isa(inf_result, InferenceResult) || return inf_result

    # constant prop' hits a cycle (recur into same non-constant analysis), we just bail out
    inf_result.result === nothing && return inf_result

    # cache hit, restore reports from the local report cache

    if cache_target !== nothing
        context, caller = cache_target
        @assert context === :const_prop_call "invalid JET analysis state"

        # as the analyzer uses the reports that are cached by the abstract-interpretation
        # with the extended lattice elements, here we should throw-away the error reports
        # that are collected during the previous non-constant abstract-interpretation
        # (see the `CC.typeinf(::AbstractAnalyzer, ::InferenceState)` overload)
        filter_lineages!(analyzer, caller.result, mi)

        cached_reports = CC.traverse_analysis_results(inf_result) do @nospecialize analysis_result
            analysis_result isa CachedAnalysisResult ? analysis_result.reports : nothing
        end
        cached_reports !== nothing &&
            collect_cached_callee_reports!(analyzer, cached_reports, caller, mi)
    end
    return inf_result
end

CC.push!(view::AbstractAnalyzerView, inf_result::InferenceResult) = CC.push!(get_inf_cache(view.analyzer), inf_result)

# main driver
# ===========

# in this overload we will work on some meta/debug information management per inference frame
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    parent = CC.frame_parent(frame)

    if is_constant_propagated(frame) && parent !== nothing
        parent::InferenceState
        # JET is going to perform the abstract-interpretation with the extended lattice elements:
        # throw-away the error reports that are collected during the previous non-constant abstract-interpretation
        # NOTE that the `linfo` here is the exactly same object as the method instance used
        # for the previous non-constant abstract-interpretation
        filter_lineages!(analyzer, parent.result, CC.frame_instance(frame))
    end

    ret = @invoke CC.typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    return ret
end

"""
    islineage(parent::MethodInstance, current::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport`
- is generated from `current`, and
- is "lineage" of `parent` (i.e. entered from it).

This function is supposed to be used when additional analysis with extended lattice information
happens in order to filter out reports collected from `current` by analysis without
using that extended information. When a report should be filtered out, the first virtual
stack frame represents `parent` and the second does `current`.

Example:
```
entry
└─ linfo1 (report1: linfo1->linfo2)
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report2: linfo3->linfo2)
   │  └─ linfo2 (report2: linfo2)
   └─ linfo3′ (~~report2: linfo3->linfo2~~)
```
In the example analysis above, `report2` should be filtered out on re-entering into `linfo3′`
(i.e. when we're analyzing `linfo3` with constant arguments), nevertheless `report1` shouldn't
because it is not detected within `linfo3` but within `linfo1` (so it's not a "lineage of `linfo3`"):
- `islineage(linfo1, linfo3)(report2) === true`
- `islineage(linfo1, linfo3)(report1) === false`
"""
function islineage(parent::MethodInstance, current::MethodInstance)
    function (report::InferenceErrorReport)
        @nospecialize report
        @inbounds begin
            vst = report.vst
            length(vst) > 1 || return false
            vst[1].linfo === parent || return false
            return vst[2].linfo === current
        end
    end
end

function filter_lineages!(analyzer::AbstractAnalyzer, caller::InferenceResult, current::MethodInstance)
     filter!(!islineage(caller.linfo, current), get_reports(analyzer, caller))
end

function finish_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    caller = frame.result

    reports = get_reports(analyzer, caller)

    # XXX this is a dirty fix for performance problem, we need more "proper" fix
    # https://github.com/aviatesk/JET.jl/issues/75
    unique!(aggregation_policy(analyzer), reports)

    if CC.frame_parent(frame) !== nothing
        # inter-procedural handling: get back to the caller what we got from these results
        stash_reports!(analyzer, reports)
    end

    cache_reports!(analyzer, caller, reports)
end

function cache_reports!(analyzer::AbstractAnalyzer, caller::InferenceResult,
                        reports::Vector{InferenceErrorReport})
    cached_reports = InferenceErrorReport[]
    mi = caller.linfo
    for report in reports
        @static if JET_DEV_MODE
            actual, expected = first(report.vst).linfo, mi
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cached_reports, report)
    end
    CC.stack_analysis_result!(caller, CachedAnalysisResult(cached_reports))
end

function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState, validation_world::UInt, time_before::UInt64)
    finish_frame!(analyzer, frame)
    return @invoke CC.finish!(analyzer::AbstractInterpreter, frame::InferenceState, validation_world::UInt, time_before::UInt64)
end

# top-level bridge
# ================

function CC.abstract_eval_basic_statement(analyzer::AbstractAnalyzer, @nospecialize(stmt),
    sstate::StatementState, frame::InferenceState, result::Union{Nothing,Future{RTEffects}})
    if istoplevelframe(frame)
        if get_concretized(analyzer)[frame.currpc]
            return CC.AbstractEvalBasicStatementResult(nothing, Bottom, nothing, nothing, nothing, false) # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end
    return @invoke CC.abstract_eval_basic_statement(analyzer::AbstractInterpreter, stmt::Any,
        sstate::StatementState, frame::InferenceState, result::Union{Nothing,Future{RTEffects}})
end

function CC.global_assignment_rt_exct(analyzer::AbstractAnalyzer, sv::AbsIntState, saw_latestworld::Bool, g::GlobalRef, @nospecialize(newty))
    if saw_latestworld
        return Pair{Any,Any}(newty, ErrorException)
    end
    istoplevel = istoplevelframe(sv)
    ⊔ = CC.join(typeinf_lattice(analyzer))
    (valid_worlds, ret) = CC.scan_partitions(analyzer, g, sv.world) do analyzer::AbstractAnalyzer, binding::Core.Binding, partition::Core.BindingPartition
        rte = CC.global_assignment_binding_rt_exct(analyzer, partition, newty)
        if istoplevel
            # XXX `binding_states` tracks the binding types that are known at top-level only,
            #     so using the type information may result in incorrect results,
            #     while just Deriving defined-ness information would probably be fine
            # TODO need to represent conditional case
            get_binding_states(analyzer)[partition] = AbstractBindingState(false)
        end
        return rte
    end
    CC.update_valid_age!(sv, valid_worlds)
    return ret
end

function CC.abstract_eval_statement_expr(analyzer::AbstractAnalyzer, e::Expr, sstate::StatementState,
                                         sv::AbsIntState)::Future{RTEffects}
    if isexpr(e, :const)
        return abstract_eval_const_stmt(analyzer, e, sstate, sv)
    end
    return @invoke CC.abstract_eval_statement_expr(analyzer::AbstractInterpreter, e::Expr, sstate::StatementState, sv::AbsIntState)
end

# XXX Do we need to port this back to Julia base?
function abstract_eval_const_stmt(analyzer::AbstractAnalyzer, stmt::Expr, sstate::StatementState, sv::AbsIntState)
    na = length(stmt.args)
    if na == 0 # currently noub
        return RTEffects(Union{}, ErrorException, EFFECTS_THROWS)
    elseif !istoplevelframe(sv) # shouldn't be hit since blocked by the frontend
        return RTEffects(Union{}, ErrorException, EFFECTS_THROWS)
    end
    lastargtype = CC.abstract_eval_value(analyzer, stmt.args[end], sstate, sv)
    if !CC.isvarargtype(lastargtype)
        if na == 1 || na == 2
            val = stmt.args[1]
            if val isa Symbol
                val = GlobalRef(CC.frame_module(sv), val)
            end
            val isa GlobalRef || return RTEffects(Nothing, ErrorException, EFFECTS_THROWS)
            rt, exct = const_assignment_rt_exct(analyzer, sv, sstate.saw_latestworld, val, na == 2 ? lastargtype : nothing)
            return RTEffects(rt, exct, CC.Effects(EFFECTS_THROWS; nothrow=exct===Union{}))
        else
            return RTEffects(Union{}, ErrorException, EFFECTS_THROWS)
        end
    else
        return RTEffects(Nothing, ErrorException, EFFECTS_THROWS)
    end
end

function const_assignment_rt_exct(analyzer::AbstractAnalyzer, sv::AbsIntState, saw_latestworld::Bool, gr::GlobalRef,
                                  @nospecialize(new_binding_typ))
    if saw_latestworld
        return Pair{Any,Any}(Nothing, ErrorException)
    end
    ⊔ = CC.join(CC.typeinf_lattice(analyzer))
    (valid_worlds, ret) = CC.scan_partitions(analyzer, gr, sv.world) do analyzer::AbstractAnalyzer, binding::Core.Binding, partition::Core.BindingPartition
        rte = const_assignment_binding_rt_exct(analyzer, partition)
        rt, exct = rte
        if rt !== Union{}
            # `:const` assignment destructively overrides the binding type
            # TODO need to represent conditional case
            binding_states = get_binding_states(analyzer)
            binding_states[partition] = AbstractBindingState(false, new_binding_typ)
        end
        return rte
    end
    CC.update_valid_age!(sv, valid_worlds)
    return ret
end

function const_assignment_binding_rt_exct(interp::AbstractInterpreter, partition::Core.BindingPartition)
    kind = CC.binding_kind(partition)
    if CC.is_some_const_binding(kind) && !CC.is_some_implicit(kind)
        return Pair{Any,Any}(Nothing, Union{})
    elseif CC.is_some_explicit_imported(kind)
        return Pair{Any,Any}(Union{}, ErrorException)
    elseif kind == CC.PARTITION_KIND_GLOBAL
        return Pair{Any,Any}(Union{}, ErrorException)
    end
    return Pair{Any,Any}(Nothing, ErrorException)
end

function CC.abstract_eval_partition_load(analyzer::AbstractAnalyzer, binding::Core.Binding, partition::Core.BindingPartition)
    res = @invoke CC.abstract_eval_partition_load(analyzer::AbstractInterpreter, binding::Core.Binding, partition::Core.BindingPartition)
    binding_states = get_binding_states(analyzer)
    if haskey(binding_states, partition)
        binding_state = binding_states[partition]
        if isdefined(binding_state, :typ)
            res = RTEffects(binding_state.typ, res.exct, res.effects)
        end
    end
    return res
end

function CC.abstract_eval_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)

    # HACK if we encounter `_INACTIVE_EXCEPTION`, it means `ConcreteInterpreter` tried to
    # concretize an exception which was not actually thrown – yet the actual error hasn't
    # happened thanks to JuliaInterpreter's implementation detail, i.e. JuliaInterpreter
    # could retrieve `FrameData.last_exception`, which is initialized with
    # `_INACTIVE_EXCEPTION.instance` – but it's obviously not a sound approximation of an
    # actual execution and so here we will fix it to `Any`, since we don't analyze types of
    # exceptions in general
    if is_inactive_exception(ret)
        ret = Any
    end

    return ret
end

is_inactive_exception(@nospecialize rt) = isa(rt, Const) && rt.val === _INACTIVE_EXCEPTION()

function CC.cache_result!(analyzer::AbstractAnalyzer, caller::InferenceResult, ci::CodeInstance)
    istoplevelframe(caller.linfo) && return nothing # don't need to cache toplevel frame
    @invoke CC.cache_result!(analyzer::AbstractInterpreter, caller::InferenceResult, ci::CodeInstance)
end
