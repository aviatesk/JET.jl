# inter-procedural
# ================

const ABSTRACT_CALL_USES_VTYPES = hasmethod(CC.abstract_call_known,
    Tuple{AbstractInterpreter,Any,ArgInfo,StmtInfo, Union{VarTable,Nothing},InferenceState,Int})
const FINISHINFER_USES_OPT_CACHE = hasmethod(CC.finishinfer!,
    Tuple{InferenceState,AbstractInterpreter,Int,IdDict{MethodInstance,CodeInstance}})

function collect_callee_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    reports = get_report_stash(analyzer)
    if !isempty(reports)
        if analyzer isa ToplevelAbstractAnalyzer && isconcretized(analyzer, sv)
            # Concrete execution owns diagnostics for this call, but the callee cache
            # must retain its reports for non-concretized callers.
            empty!(reports)
            return nothing
        end
        vf = get_virtual_frame(sv)
        for report in reports
            pushfirst!(report.vst, vf)
            add_new_report!(analyzer, sv.result, report)
        end
        empty!(reports)
    end
    return nothing
end

# Restore each cached report as a fresh copy — the cached report is shared on the callee's
# `CodeInstance` and must not be mutated by the re-rooting `pushfirst!` in
# `collect_callee_reports!` — and stash it. Insertion into the caller's result and re-rooting
# onto the caller frame both happen there, uniformly with freshly inferred callee reports.
function collect_cached_callee_reports!(
        analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport},
        origin_mi::MethodInstance
    )
    for cached in reports
        restored = copy_report_stable(cached)
        @static if JET_DEV_MODE
            actual, expected = first(restored.vst).linfo, origin_mi
            @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
        end
        stash_report!(analyzer, restored)
    end
    return nothing
end

function CC.abstract_call_method(analyzer::AbstractAnalyzer,
    method::Method, @nospecialize(sig), sparams::SimpleVector,
    hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    ret = @invoke CC.abstract_call_method(analyzer::AbstractInterpreter,
        method::Method, sig::Any, sparams::SimpleVector,
        hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    function after_call_method(analyzer′::AbstractAnalyzer, sv′::InferenceState)
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
    report_stash = get_report_stash(analyzer)
    nstashed = length(report_stash)
    const_result = @invoke CC.const_prop_call(analyzer::AbstractInterpreter,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
        concrete_eval_result::Union{Nothing,ConstCallResult})
    @assert get_cache_target(analyzer) === nothing "invalid JET analysis state"
    if const_result !== nothing
        # Keep the generic reports until constant propagation succeeds: a cycle-limited
        # attempt may finish and stash reports even though Compiler discards its result.
        filter_lineages!(analyzer, sv, mi)
        collect_callee_reports!(analyzer, sv)
    else
        # drop the reports that the failed attempt has stashed
        @assert length(report_stash) ≥ nstashed "invalid JET analysis state"
        resize!(report_stash, nstashed)
    end
    return const_result
end

function CC.concrete_eval_call(analyzer::AbstractAnalyzer,
    @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo,
    sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    ret = @invoke CC.concrete_eval_call(analyzer::AbstractInterpreter,
        f::Any, result::MethodCallResult, arginfo::ArgInfo,
        sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    if ret isa ConstCallResult && ret.rt !== Bottom
        # This frame has been concretized without erroring, invalidating the reports
        # collected during the previous non-constant abstract-interpretation:
        # throw them away.
        # When concretization instead proves the call always throws (`ret.rt === Bottom`),
        # keep the generic reports: analyzers may discard such a result to fall back to
        # constant propagation for precise error reporting, but that fallback is not
        # guaranteed to run (e.g. `const_prop_argument_heuristic` may refuse it), and
        # when it does run successfully, the `CC.const_prop_call` overload replaces
        # the generic reports at that point.
        edge = result.edge
        if edge isa CodeInstance
            filter_lineages!(analyzer, sv, edge.def)
        end
    end
    return ret
end

@static if ABSTRACT_CALL_USES_VTYPES
function CC.abstract_call_known(analyzer::AbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, vtypes::Union{VarTable,Nothing},
    sv::InferenceState, max_methods::Int)
    ret = @invoke CC.abstract_call_known(analyzer::AbstractInterpreter,
        f::Any, arginfo::ArgInfo, si::StmtInfo, vtypes::Union{VarTable,Nothing},
        sv::InferenceState, max_methods::Int)
    return postprocess_abstract_call_known!(analyzer, ret, f, arginfo, sv)
end
else
function CC.abstract_call_known(analyzer::AbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
    ret = @invoke CC.abstract_call_known(analyzer::AbstractInterpreter,
        f::Any, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
    return postprocess_abstract_call_known!(analyzer, ret, f, arginfo, sv)
end
end

# Early take-in of https://github.com/JuliaLang/julia/pull/57222 for v1.12
@static if VERSION < v"1.13.0-DEV.21"
function refine_setfield_callmeta(res::CallMeta, arginfo::ArgInfo, sv::InferenceState)
    (; argtypes, fargs) = arginfo
    if res.rt !== Bottom && length(argtypes) == 4 &&
        isa(argtypes[3], Const) && isa(fargs, Vector{Any})
        # on successful return, the struct field can no longer be undefined,
        # so we try to encode that information with a `PartialStruct`
        farg2 = CC.ssa_def_slot(fargs[2], sv)
        if farg2 isa SlotNumber
            refined = CC.form_partially_defined_struct(argtypes[2], argtypes[3])
            if refined !== nothing
                refinements = CC.SlotRefinement(farg2, refined)
                return CallMeta(res.rt, res.exct, res.effects, res.info, refinements)
            end
        end
    end
    return res
end
end

function postprocess_abstract_call_known!(analyzer::AbstractAnalyzer, ret::Future,
    @nospecialize(f), arginfo::ArgInfo, sv::InferenceState)
    if isready(ret)
        if f === Task
            analyze_task_parallel_code!(analyzer, arginfo, sv)
        end
        # `setfield!` is handled synchronously by the builtin path in `abstract_call_known`,
        # so its `ret` is always ready and requires no delayed processing in the branch below.
        @static if VERSION < v"1.13.0-DEV.21"
            if f === setfield!
                res = ret[]
                res′ = refine_setfield_callmeta(res, arginfo, sv)
                return res′ === res ? ret : Future(res′)
            end
        end
    else
        if f === Task
            function after_call_known(analyzer′::AbstractAnalyzer, sv′::InferenceState)
                analyze_task_parallel_code!(analyzer′, arginfo, sv′)
                return true
            end
            push!(sv.tasks, after_call_known)
        end
    end
    return ret
end

"""
    analyze_task_parallel_code!(analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState)

Adds special cased analysis pass for task parallelism.
In Julia's task parallelism implementation, parallel code is represented as closure and it's
wrapped in a `Task` object. `$CC.NativeInterpreter` doesn't infer nor optimize the
bodies of those closures when compiling code that creates parallel tasks, but JET will try
to run additional analysis pass by recurring into the closures.

See also: <https://github.com/aviatesk/JET.jl/issues/114>

!!! note
    JET won't do anything other than doing JET analysis, e.g. won't annotate return type
    of wrapped code block in order to not confuse the original `AbstractInterpreter` routine
    track <https://github.com/JuliaLang/julia/pull/39773> for the changes in native abstract
    interpretation routine.
"""
function analyze_task_parallel_code!(
        analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState
    )
    # TODO we should analyze a closure wrapped in a `Task` only when it's `schedule`d
    # But the `Task` construction may not happen in the same frame where it's `schedule`d
    # and so we may not be able to access to the closure at that point.
    # As a compromise, here we invoke the additional analysis on `Task` construction,
    # regardless of whether it's really `schedule`d or not.
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

# `return_type_tfunc` internally uses `abstract_call` to model `$CC.return_type`
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

CC.cache_owner(analyzer::AbstractAnalyzer) = AnalysisToken(analyzer)

function CC.code_cache(analyzer::AbstractAnalyzer)
    view = AbstractAnalyzerView(analyzer)
    worlds = WorldRange(CC.get_inference_world(analyzer))
    return WorldView(view, worlds)
end

to_internal_code_cache_view(wvc::WorldView{<:AbstractAnalyzerView}) =
    WorldView(CC.InternalCodeCache(CC.cache_owner(wvc.cache.analyzer)), wvc.worlds)

CC.haskey(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance) = haskey(to_internal_code_cache_view(wvc), mi)

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::InferenceState,
                         edgecycle::Bool, edgelimited::Bool)
    set_cache_target!(analyzer, :typeinf_edge => caller)
    ret = @invoke CC.typeinf_edge(analyzer::AbstractInterpreter, method::Method, atype::Any, sparams::SimpleVector, caller::InferenceState,
                                  edgecycle::Bool, edgelimited::Bool)
    @assert get_cache_target(analyzer) === nothing "invalid JET analysis state"
    return ret
end

function CC.get(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance, default)
    codeinst = get(to_internal_code_cache_view(wvc), mi, default)

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
        context, _ = cache_target
        if context === :typeinf_edge
            if isa(codeinst, CodeInstance)
                # cache hit, now we need to append cached reports associated with this `MethodInstance`
                cached_reports = CC.traverse_analysis_results(codeinst) do @nospecialize analysis_result
                    analysis_result isa CachedAnalysisResult ? analysis_result.reports : nothing
                end
                cached_reports !== nothing &&
                    collect_cached_callee_reports!(analyzer, cached_reports, mi)
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
    return to_internal_code_cache_view(wvc)[mi] = codeinst
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
        context, _ = cache_target
        @assert context === :const_prop_call "invalid JET analysis state"

        cached_reports = CC.traverse_analysis_results(inf_result) do @nospecialize analysis_result
            analysis_result isa CachedAnalysisResult ? analysis_result.reports : nothing
        end
        cached_reports !== nothing &&
            collect_cached_callee_reports!(analyzer, cached_reports, mi)
    end
    return inf_result
end

CC.push!(view::AbstractAnalyzerView, inf_result::InferenceResult) = CC.push!(get_inf_cache(view.analyzer), inf_result)

# main driver
# ===========

"""
    islineage(parent_frame::VirtualFrame, current::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport`
- is generated from `current`, and
- is "lineage" of `parent_frame` (i.e. entered from it).

This function is supposed to be used when additional analysis with extended lattice
information happens in order to filter out reports collected from `current` by analysis
without using that extended information. When a report should be filtered out, the first
virtual stack frame should match `parent_frame` and the second should represent `current`.

Example:
```
entry
└─ linfo1 (report1: linfo1->linfo2)
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report2: linfo3->linfo2)
   │  └─ linfo2 (report2: linfo2)
   └─ linfo3′ (~~report2: linfo3->linfo2~~)
```
In the example analysis above, `report2` should be filtered out on re-entering into
`linfo3′` (i.e. when we're analyzing `linfo3` with constant arguments), nevertheless
`report1` shouldn't because it is not detected within `linfo3` but within `linfo1`
(so it's not a "lineage of `linfo3`"):
- `islineage(vf1, linfo3)(report2) === true`, where `vf1` is `linfo1`'s frame at
  the callsite of `linfo3`
- `islineage(vf1, linfo3)(report1) === false`
"""
function islineage(parent_frame::VirtualFrame, current::MethodInstance)
    function (report::InferenceErrorReport)
        @nospecialize report
        vst = report.vst
        return length(vst) > 1 && vst[1] == parent_frame && vst[2].linfo === current
    end
end

function filter_lineages!(
        analyzer::AbstractAnalyzer, caller::InferenceState, current::MethodInstance
    )
    reports = get_reports(analyzer, caller.result)
    isempty(reports) && return
    parent_frame = get_virtual_frame(caller)
    filter!(!islineage(parent_frame, current), reports)
end

function contains_edge(edges::Vector{Any}, edge::MethodInstance)
    return any(edges) do existing
        existing === edge
    end
end

function add_report_dependency_edges!(
        edges::Vector{Any}, caller::MethodInstance,
        reports::Vector{InferenceErrorReport}
    )
    for report in reports, vf in report.vst
        linfo = vf.linfo
        linfo === caller && continue
        linfo.def isa Method || continue
        contains_edge(edges, linfo) || push!(edges, linfo)
    end
    return nothing
end

function finish_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    caller = frame.result

    reports = get_reports(analyzer, caller)

    # XXX this is a dirty fix for performance problem, we need more "proper" fix
    # https://github.com/aviatesk/JET.jl/issues/75
    unique!(aggregation_policy(analyzer), reports)

    # Cached reports can be produced by analyzer hooks whose methods appear in
    # report stacks even when the ordinary inference result for `caller` does
    # not depend on them. Add those frames as edges before caching reports, so
    # method redefinitions invalidate diagnostics restored from the cache.
    add_report_dependency_edges!(frame.edges, caller.linfo, reports)
    # the cycle members are not cached (see the `CC.finishinfer!` overloads)
    is_cycle_member(frame) || cache_reports!(analyzer, caller, reports)

    # The frames of a call cycle are finished one by one from the cycle top, so wait for the
    # last one, when every frame of the cycle has got the reports from analyzer hooks such
    # as `CC.finish!` overloads.
    callstack = frame_callstack(frame)
    if frame.frameid == 0 || callstack[end] === frame
        top = frame.cycleid == 0 ? frame : callstack[frame.cycleid]::InferenceState
        top === frame || handoff_cycle_member_reports!(top)
        if CC.frame_parent(top) !== nothing
            # inter-procedural handling: get back to the caller what we got from these results
            top_analyzer = top.interp::AbstractAnalyzer
            stash_reports!(top_analyzer, get_reports(top_analyzer, top.result))
        end
    end
end

# Cycle members are finished only after the whole cycle converges, when their callers have
# already collected callee reports from the stash. Hand their reports over along a spanning
# tree of the calls within the cycle instead, which is searched breadth-first from the cycle
# top through the call sites where Compiler has not used a constant prop' result in place of
# the non-constant callee result. Members reachable only through such call sites don't hand
# their reports over at all. The tree is prepared by the `CC.finishinfer!` overloads for the
# cycle top, when the cycle has converged and no frame of it has been optimized yet, and the
# reports are handed from the leaves once the last frame has got its reports from analyzer
# hooks such as `CC.finish!` overloads.
function prepare_cycle_handoff!(analyzer::AbstractAnalyzer, top::InferenceState)
    callstack = frame_callstack(top)
    cycle = top.frameid:length(callstack)
    # caller frame id => [(callee frame id, program counter, superseded)]
    calls = Dict{Int,Vector{Tuple{Int,Int,Bool}}}()
    for frameid = cycle
        callee = callstack[frameid]::InferenceState
        for (caller, pc) in callee.cycle_backedges
            caller === callee && continue
            caller.frameid in cycle && callstack[caller.frameid] === caller || continue
            caller.interp === callee.interp || continue
            superseded = uses_constprop_result(caller, pc, callee.linfo)
            push!(get!(Vector{Tuple{Int,Int,Bool}}, calls, caller.frameid),
                  (frameid, pc, superseded))
        end
    end
    order, parents = search_cycle_calls(calls, top.frameid, #=through_superseded=#false)
    callsites = Dict{Int,Tuple{Int,VirtualFrame}}()
    for (calleeid, (callerid, pc)) in parents
        caller = callstack[callerid]::InferenceState
        callsites[calleeid] = (callerid, get_virtual_frame((caller, pc)))
    end
    reachable = BitSet(first(search_cycle_calls(calls, top.frameid, #=through_superseded=#true)))
    untracked = Int[frameid for frameid = cycle if frameid ∉ reachable]
    get_cycle_handoffs(analyzer)[top] = CycleHandoff(order, callsites, untracked)
    return nothing
end

# The final call info retains const-prop results even when the return type is unchanged.
# Every occurrence of `mi` must be replaced, including implicit calls within wrappers.
uses_constprop_result(caller::InferenceState, pc::Int, mi::MethodInstance) =
    constprop_result_status(caller.stmt_info[pc], mi) === true

# `nothing` means unrelated, `true` means the call site doesn't use the non-constant callee
# reports, e.g. when a constant prop' result replaces them, and `false` vetoes suppression.
# Unknown call info is conservative: it cannot prove the absence of a generic call.
function constprop_result_status(@nospecialize(info::CC.CallInfo), mi::MethodInstance)
    if (info isa CC.MethodResultPure || info isa CC.ModifyOpInfo ||
        info isa CC.FinalizerInfo || info isa CC.VirtualMethodMatchInfo)
        return constprop_result_status(info.info, mi)
    elseif info isa CC.ReturnTypeCallInfo
        # `return_type` analyzes a simulated call whose reports JET deliberately discards,
        # so the reports shouldn't reach the cycle top through this call site either
        return true
    elseif info isa CC.GlobalAccessInfo
        return nothing
    elseif info isa CC.ApplyCallInfo
        status = constprop_result_status(info.call, mi)
        status === false && return false
        for arg in info.arginfo
            arg === nothing && continue
            for call in arg.each
                status = merge_constprop_status(status, constprop_result_status(call.info, mi))
                status === false && return false
            end
        end
        return status
    elseif info isa CC.UnionSplitApplyCallInfo
        status = nothing
        for split in info.infos
            status = merge_constprop_status(status, constprop_result_status(split, mi))
            status === false && return false
        end
        return status
    elseif info isa CC.InvokeCallInfo || info isa CC.OpaqueClosureCallInfo
        return constprop_match_status(info.match, info.result, mi)
    elseif info isa CC.OpaqueClosureCreateInfo
        return constprop_result_status(info.unspec.info, mi)
    elseif info isa CC.InvokeCICallInfo
        return info.edge.def === mi ? false : nothing
    end
    nsplit = @something CC.nsplit(info) return false
    status = nothing
    result_index = 0
    for i = 1:nsplit
        for match in CC.getsplit(info, i)
            result_index += 1
            result = CC.getresult(info, result_index)
            status = merge_constprop_status(status, constprop_match_status(match, result, mi))
            status === false && return false
        end
    end
    return status
end

function constprop_match_status(match::Core.MethodMatch,
                               result::Union{Nothing,CC.ConstResult}, mi::MethodInstance)
    if result isa CC.ConstPropResult && result.result.linfo === mi
        return true
    end
    return specialize_method(match; preexisting=true) === mi ? false : nothing
end

function merge_constprop_status(a::Union{Nothing,Bool}, b::Union{Nothing,Bool})
    (a === false || b === false) && return false
    return (a === true || b === true) ? true : nothing
end

# Search the calls within a cycle breadth-first from `root`, returning the frame ids in the
# visited order and the caller frame id and program counter that each frame is visited from.
function search_cycle_calls(calls::Dict{Int,Vector{Tuple{Int,Int,Bool}}}, root::Int,
                            through_superseded::Bool)
    order = Int[root]
    parents = Dict{Int,Tuple{Int,Int}}()
    i = 1
    while i ≤ length(order)
        callerid = order[i]
        i += 1
        haskey(calls, callerid) || continue
        for (calleeid, pc, superseded) in calls[callerid]
            superseded && !through_superseded && continue
            (calleeid == root || haskey(parents, calleeid)) && continue
            parents[calleeid] = (callerid, pc)
            push!(order, calleeid)
        end
    end
    return order, parents
end

function handoff_cycle_member_reports!(top::InferenceState)
    top_analyzer = top.interp::AbstractAnalyzer
    (; order, callsites, untracked) = pop!(get_cycle_handoffs(top_analyzer), top)
    callstack = frame_callstack(top)
    handed = false
    for i = length(order):-1:2
        member = callstack[order[i]]::InferenceState
        analyzer = member.interp::AbstractAnalyzer
        reports = get_reports(analyzer, member.result)
        isempty(reports) && continue
        unique!(aggregation_policy(analyzer), reports)
        callerid, vf = callsites[order[i]]
        caller = callstack[callerid]::InferenceState
        for report in reports
            new = copy_report_stable(report)
            pushfirst!(new.vst, vf)
            add_new_report!(analyzer, caller.result, new)
        end
        handed = true
    end
    # Compiler validates the cycle `CodeInstance`s in the current world only after all the
    # frames are finished, so the cached reports of the cycle top can still be updated here.
    handed && refresh_cached_reports!(top_analyzer, top.result, get_reports(top_analyzer, top.result))
    for frameid in untracked
        # keep the conventional handling for the members whose calls are not tracked, which
        # attributes the reports to the caller of the cycle top
        member = callstack[frameid]::InferenceState
        analyzer = member.interp::AbstractAnalyzer
        reports = get_reports(analyzer, member.result)
        isempty(reports) || stash_reports!(analyzer, reports)
    end
    return nothing
end

function refresh_cached_reports!(analyzer::AbstractAnalyzer, caller::InferenceResult,
                                 reports::Vector{InferenceErrorReport})
    unique!(aggregation_policy(analyzer), reports)
    cached_reports = CC.traverse_analysis_results(caller) do @nospecialize analysis_result
        analysis_result isa CachedAnalysisResult ? analysis_result.reports : nothing
    end
    cached_reports === nothing && return nothing
    empty!(cached_reports)
    fill_cached_reports!(cached_reports, caller.linfo, reports)
    return nothing
end

function cache_reports!(::AbstractAnalyzer, caller::InferenceResult,
                        reports::Vector{InferenceErrorReport})
    cached_reports = fill_cached_reports!(InferenceErrorReport[], caller.linfo, reports)
    CC.stack_analysis_result!(caller, CachedAnalysisResult(cached_reports))
end

function fill_cached_reports!(cached_reports::Vector{InferenceErrorReport},
                              mi::MethodInstance, reports::Vector{InferenceErrorReport})
    for report in reports
        @static if JET_DEV_MODE
            actual, expected = first(report.vst).linfo, mi
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cached_reports, report)
    end
    return cached_reports
end

# `InferenceState.callstack` is untyped on Julia 1.12 and 1.13
@static if fieldtype(InferenceState, :callstack) === Any
    frame_callstack(sv::InferenceState) = sv.callstack::Vector{CC.AbsIntState}
else
    frame_callstack(sv::InferenceState) = sv.callstack
end

is_cycle_top(frame::InferenceState) = frame.frameid ≠ 0 && frame.cycleid == frame.frameid &&
    length(frame_callstack(frame)) > frame.frameid
is_cycle_member(frame::InferenceState) = frame.cycleid ≠ frame.frameid

# `CC.finishinfer!` is called for every frame of a cycle once the cycle has converged, before
# any frame of it is optimized or finished, so prepare the hand-off of the member reports
# there. The reports of a member depend on the spanning tree that hands them over to the
# cycle top, so like Compiler does for the results limited by recursion, don't cache this
# intermediate work globally either but let later analyses infer it again.
@static if FINISHINFER_USES_OPT_CACHE
function CC.finishinfer!(
        frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int,
        opt_cache::IdDict{MethodInstance,CodeInstance}
    )
    is_cycle_top(frame) && prepare_cycle_handoff!(analyzer, frame)
    ret = @invoke CC.finishinfer!(
        frame::InferenceState, analyzer::AbstractInterpreter, cycleid::Int,
        opt_cache::IdDict{MethodInstance,CodeInstance})
    # `CC.finish!` publishes the result, so the member stays in `opt_cache` for the inlining
    # within the cycle
    is_cycle_member(frame) && (frame.cache_mode &= ~CC.CACHE_MODE_GLOBAL)
    return ret
end
else
function CC.finishinfer!(frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int)
    is_cycle_top(frame) && prepare_cycle_handoff!(analyzer, frame)
    # Julia 1.12 publishes the result within `CC.finishinfer!`, before the optimization that
    # the cache mode also controls, so make the member volatile: it is still optimized but
    # not cached, at the cost of the inlining within the cycle no longer finding it
    if is_cycle_member(frame) && CC.is_cached(frame)
        frame.cache_mode = CC.CACHE_MODE_VOLATILE
    end
    return @invoke CC.finishinfer!(frame::InferenceState, analyzer::AbstractInterpreter, cycleid::Int)
end
end

function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState, validation_world::UInt, time_before::UInt64)
    finish_frame!(analyzer, frame)
    return @invoke CC.finish!(analyzer::AbstractInterpreter, frame::InferenceState, validation_world::UInt, time_before::UInt64)
end

# top-level bridge
# ================

function isconcretized(analyzer::ToplevelAbstractAnalyzer, frame::InferenceState, pc::Int=frame.currpc)
    return istoplevelframe(frame) && get_concretized(analyzer)[pc]
end

function CC.abstract_eval_basic_statement(analyzer::ToplevelAbstractAnalyzer, @nospecialize(stmt),
    sstate::StatementState, frame::InferenceState, result::Union{Nothing,Future{RTEffects}})
    if isexpr(stmt, :latestworld)
        if isconcretized(analyzer, frame)
            # ignore the effect of `:latestworld` if its effect took in place by `ConcreteInterpreter`
            return CC.AbstractEvalBasicStatementResult(nothing, Any, nothing, nothing, nothing, #=saw_latestworld=#false)
        end
    end
    return @invoke CC.abstract_eval_basic_statement(analyzer::AbstractAnalyzer, stmt::Any,
        sstate::StatementState, frame::InferenceState, result::Union{Nothing,Future{RTEffects}})
end

function CC.global_assignment_rt_exct(analyzer::ToplevelAbstractAnalyzer, sv::InferenceState, saw_latestworld::Bool, g::GlobalRef, @nospecialize(newty))
    if saw_latestworld
        return Pair{Any,Any}(newty, ErrorException)
    end
    isconcretized = JET.isconcretized(analyzer, sv) # this statement has been analyzed by `ConcreteInterpreter`
    newty′ = Ref{Any}(newty)
    istoplevel = istoplevelframe(sv)
    assignment = istoplevel ? get_current_toplevel_assignment(analyzer) : nothing
    isconditional = istoplevel ? let postdomtree = CC.construct_postdomtree(sv.cfg)
        !CC.postdominates(postdomtree, sv.currbb, 1)
    end : true
    (valid_worlds, ret) = CC.scan_partitions(analyzer, g, sv.world) do analyzer::AbstractAnalyzer, ::Core.Binding, partition::Core.BindingPartition
        return CC.global_assignment_binding_rt_exct(analyzer, partition, newty′[])
    end
    CC.update_valid_age!(sv, valid_worlds)
    rt, _exct = ret
    if !isconcretized && rt !== Union{}
        # Historical queries must not update the inference world's binding state.
        partition = Base.lookup_binding_partition(sv.world.this, g)
        # Non-const bindings may be assigned in any call, so it is fundamentally impossible
        # to track their types precisely.
        # However, by accurately determining whether a top-level assignment is conditional,
        # it is possible to track such bindings’ `isdefined` status precisely.
        binding_states = get_binding_states(analyzer)
        @lock binding_states.lock begin
            bindings = binding_states.bindings
            new_state = if haskey(bindings, partition)
                old_state = bindings[partition]
                if old_state.isconst
                    # Ordinary assignments to constants throw; `const` redefinitions use
                    # `const_assignment_rt_exct` instead.
                    old_state
                else
                    maybeundef = old_state.maybeundef & isconditional
                    same_statement = old_state.assignment === assignment
                    merged_assignment = same_statement ? assignment : nothing
                    AbstractBindingState(false, maybeundef; assignment = merged_assignment)
                end
            else
                AbstractBindingState(false, isconditional; assignment)
            end
            bindings[partition] = new_state
        end
    end
    return ret
end

@static if isdefinedglobal(Core, :declare_const)

function abstract_eval_declare_const(
        analyzer::ToplevelAbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo,
        sv::InferenceState
    )
    istoplevelframe(sv) || return nothing
    isconcretized(analyzer, sv) && return nothing
    argtypes = arginfo.argtypes
    length(argtypes) in (3, 4) || return nothing
    CC.isvarargtype(argtypes[end]) && return nothing
    mod, name = argtypes[2], argtypes[3]
    mod isa Const && mod.val isa Module || return nothing
    name isa Const && name.val isa Symbol || return nothing
    gr = GlobalRef(mod.val, name.val)
    new_binding_typ = length(argtypes) == 4 ? argtypes[4] : nothing
    ((rt, exct), _isimported) = const_assignment_rt_exct(analyzer, sv, si.saw_latestworld, gr, new_binding_typ)
    if rt !== Union{}
        rt = length(argtypes) == 4 ? new_binding_typ : Nothing
    end
    effects = CC.Effects(EFFECTS_THROWS; nothrow=exct===Union{})
    return Future(CallMeta(rt, exct, effects, CC.NoCallInfo()))
end

@static if ABSTRACT_CALL_USES_VTYPES
function CC.abstract_call_known(analyzer::ToplevelAbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, vtypes::Union{VarTable,Nothing},
    sv::InferenceState, max_methods::Int)
    if f === Core.declare_const
        ret = abstract_eval_declare_const(analyzer, arginfo, si, sv)
        ret === nothing || return ret
    end
    return @invoke CC.abstract_call_known(analyzer::AbstractAnalyzer,
        f::Any, arginfo::ArgInfo, si::StmtInfo, vtypes::Union{VarTable,Nothing},
        sv::InferenceState, max_methods::Int)
end
else
function CC.abstract_call_known(analyzer::ToplevelAbstractAnalyzer,
    @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
    if f === Core.declare_const
        ret = abstract_eval_declare_const(analyzer, arginfo, si, sv)
        ret === nothing || return ret
    end
    return @invoke CC.abstract_call_known(analyzer::AbstractAnalyzer,
        f::Any, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState, max_methods::Int)
end
end # @static if ABSTRACT_CALL_USES_VTYPES

else # @static if isdefinedglobal(Core, :declare_const)

function CC.abstract_eval_statement_expr(analyzer::ToplevelAbstractAnalyzer, e::Expr, sstate::StatementState,
                                         sv::InferenceState)::Future{RTEffects}
    if isexpr(e, :const)
        if !isconcretized(analyzer, sv) # skip the assignment effect if this has been concretized already
            return abstract_eval_const_stmt(analyzer, e, sstate, sv)
        end
    end
    return @invoke CC.abstract_eval_statement_expr(analyzer::AbstractAnalyzer, e::Expr, sstate::StatementState, sv::InferenceState)
end

# XXX Do we need to port this back to Julia base?
function abstract_eval_const_stmt(analyzer::ToplevelAbstractAnalyzer, stmt::Expr, sstate::StatementState, sv::InferenceState)
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
            ((rt, exct), _isimported) = const_assignment_rt_exct(analyzer, sv, sstate.saw_latestworld, val, na == 2 ? lastargtype : nothing)
            return RTEffects(rt, exct, CC.Effects(EFFECTS_THROWS; nothrow=exct===Union{}))
        else
            return RTEffects(Union{}, ErrorException, EFFECTS_THROWS)
        end
    else
        return RTEffects(Nothing, ErrorException, EFFECTS_THROWS)
    end
end

end # @static if isdefinedglobal(Core, :declare_const)

function const_assignment_rt_exct(analyzer::ToplevelAbstractAnalyzer, sv::InferenceState, saw_latestworld::Bool, gr::GlobalRef,
                                  @nospecialize(new_binding_typ))
    @assert istoplevelframe(sv)
    if saw_latestworld
        return Pair{Any,Any}(Nothing, ErrorException), false
    end
    (valid_worlds, (ret, isimported)) = CC.scan_partitions(analyzer, gr, sv.world) do analyzer::ToplevelAbstractAnalyzer, _binding::Core.Binding, partition::Core.BindingPartition
        return const_assignment_binding_rt_exct(analyzer, partition)
    end
    CC.update_valid_age!(sv, valid_worlds)
    rt, _exct = ret
    if rt !== Union{}
        # Historical partitions queried by the scan must not trigger constant declarations.
        partition = Base.lookup_binding_partition(sv.world.this, gr)
        if new_binding_typ === nothing
            Core.eval(gr.mod, Expr(:const, gr.name))
        else
            ⊔ = CC.join(CC.typeinf_lattice(analyzer))
            postdomtree = CC.construct_postdomtree(sv.cfg)
            isconditional = !CC.postdominates(postdomtree, sv.currbb, 1)
            assignment = get_current_toplevel_assignment(analyzer)
            # `:const` assignment destructively overrides the binding type
            binding_states = get_binding_states(analyzer)
            binding_state = @lock binding_states.lock begin
                bindings = binding_states.bindings
                if !isconditional
                    new_state = AbstractBindingState(true, false, new_binding_typ; assignment)
                elseif haskey(bindings, partition)
                    old_binding_state = bindings[partition]
                    @assert old_binding_state.isconst && isdefined(old_binding_state, :typ)
                    newmaybeundef = old_binding_state.maybeundef & isconditional
                    newtyp = old_binding_state.typ ⊔ new_binding_typ
                    # each top-level statement builds its own `ToplevelAssignment`, so
                    # `===` here means "the same statement"; keep it only then, since
                    # no single pattern can stand for two conflicting statements
                    same_statement = old_binding_state.assignment === assignment
                    merged_assignment = same_statement ? assignment : nothing
                    new_state = AbstractBindingState(true, newmaybeundef, newtyp; assignment=merged_assignment)
                else
                    new_state = AbstractBindingState(true, true, new_binding_typ; assignment)
                end
                bindings[partition] = new_state
                new_state
            end
            # HACK/FIXME Concretize `AbstractBindingState`
            # For top-level analysis implementation reasons, we actually define this
            # `AbstractBindingState` in the analyzed module’s namespace.
            # This is necessary because binding resolution cannot be accurately tracked
            # when using `export`/`using`.
            Core.eval(gr.mod, Expr(:const, gr.name, binding_state))
        end
    end
    return ret, isimported
end

function const_assignment_binding_rt_exct(_interp::ToplevelAbstractAnalyzer, partition::Core.BindingPartition)
    kind = CC.binding_kind(partition)
    if CC.is_some_const_binding(kind) && !CC.is_some_imported(kind)
        return Pair{Any,Any}(Nothing, Union{}), false
    elseif CC.is_some_explicit_imported(kind)
        return Pair{Any,Any}(Union{}, ErrorException), true
    elseif kind == CC.PARTITION_KIND_GLOBAL
        return Pair{Any,Any}(Union{}, ErrorException), false
    end
    return Pair{Any,Any}(Nothing, ErrorException), false
end

function CC.abstract_eval_partition_load(analyzer::ToplevelAbstractAnalyzer, binding::Core.Binding, partition::Core.BindingPartition)
    res = @invoke CC.abstract_eval_partition_load(analyzer::AbstractAnalyzer, binding::Core.Binding, partition::Core.BindingPartition)
    ⊑ = CC.partialorder(CC.typeinf_lattice(analyzer))
    if res.rt !== Union{} && res.rt ⊑ AbstractBindingState
        # HACK/FIXME Concretize `AbstractBindingState`
        rt = res.rt
        if rt isa Const
            binding_state = rt.val::AbstractBindingState
            if isdefined(binding_state, :typ)
                (; exct, effects) = res
                if binding_state.maybeundef
                    ⊔ = CC.join(CC.typeinf_lattice(analyzer))
                    exct = exct ⊔ UndefVarError
                    effects = CC.Effects(effects; nothrow=exct===Union{})
                end
                return RTEffects(binding_state.typ, exct, effects)
            end
        end
        return RTEffects(Any, res.exct, res.effects)
    end
    binding_state = get(get_binding_states(analyzer), partition, nothing)
    if binding_state !== nothing && isdefined(binding_state, :typ)
        return RTEffects(binding_state.typ, res.exct, res.effects)
    end
    return res
end

function CC.abstract_eval_value(analyzer::ToplevelAbstractAnalyzer, @nospecialize(e), sstate::StatementState, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractAnalyzer, e::Any, sstate::StatementState, sv::InferenceState)

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

function CC.cache_result!(analyzer::ToplevelAbstractAnalyzer, caller::InferenceResult, ci::CodeInstance)
    istoplevelframe(caller.linfo) && return nothing # don't need to cache toplevel frame
    @invoke CC.cache_result!(analyzer::AbstractAnalyzer, caller::InferenceResult, ci::CodeInstance)
end
