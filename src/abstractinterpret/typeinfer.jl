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
    function after_call_known(analyzer′, sv′)
        ret′ = ret[]
        analyze_task_parallel_code!(analyzer′, f, arginfo, sv′)
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

# types
# =====

const AbsIntState = Union{InferenceState,Nothing}

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

# TODO update or delete
# function CC.cache_result!(analyzer::AbstractAnalyzer, caller::InferenceResult)
#     istoplevel(caller.linfo) && return nothing # don't need to cache toplevel frame
#     @invoke CC.cache_result!(analyzer::AbstractInterpreter, caller::InferenceResult)
# end

# top-level bridge
# ================

"""
    mutable struct AbstractGlobal
        t::Any     # analyzed type
        isconst::Bool # is this abstract global variable declarared as constant or not
    end

Wraps a global variable whose type is analyzed by abstract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
analysis may refer to or alter its type on future load and store operations.

!!! note
    The type of the wrapped global variable will be propagated only when in a toplevel frame,
    and thus we don't care about the analysis cache invalidation on a refinement of the
    wrapped global variable, since JET doesn't cache the toplevel frame.
"""
mutable struct AbstractGlobal
    t::Any        # analyzed type
    isconst::Bool # is this abstract global variable declarared as constant or not
    AbstractGlobal(@nospecialize(t), isconst::Bool) = new(t, isconst)
end

"""
    bail_out_toplevel_call(analyzer::AbstractAnalyzer, ...)

This overload allows JET to keep inference performed by `AbstractAnalyzer` going on
non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
CC.bail_out_toplevel_call(::AbstractAnalyzer, ::CC.InferenceLoopState, ::InferenceState) = false

function CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    istoplevel = istoplevel(sv)

    if istoplevel
        if isa(e, SlotNumber) && is_global_slot(analyzer, e)
            if get_slottype((sv, get_currpc(sv)), e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract interpret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(get_toplevelmod(analyzer), get_slotname(sv, e))
            end
        end
    end

    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)

    if istoplevel
        if isa(e, GlobalRef)
            mod, name = e.mod, e.name
            if isdefined(mod, name)
                # eagerly propagate the type of this global variable:
                # of course the traced type might be difference from its type in actual execution
                # e.g. we don't track global variable assignments that can happen somewhere
                # in this call graph, but it's highly possible this is a toplevel callsite
                # and we take a risk here since we can't enter the analysis otherwise
                val = getglobal(mod, name)
                ret = CC.RTEffects(isa(val, AbstractGlobal) ? val.t : Const(val), ret.exct, ret.effects)
            end
        end
    end

    return ret
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

# NOTE `abstract_eval_statement` has been inlined into `typeinf_local`,
# and currently we don't have any way to inject custom behavior into it
# function CC.abstract_eval_statement(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
#     if istoplevel(sv)
#         if get_concretized(analyzer)[get_currpc(sv)]
#             return CC.RTEffects(Any, Any, CC.Effects()) # bail out if it has been interpreted by `ConcreteInterpreter`
#         end
#     end
#     return @invoke CC.abstract_eval_statement(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)
# end

function CC.builtin_tfunction(analyzer::AbstractAnalyzer,
    @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::AbstractInterpreter,
        f::Any, argtypes::Vector{Any}, sv::Union{InferenceState,Nothing})

    if f === getglobal
        if istoplevel_getproperty(sv)
            ret = narrow_toplevel_getglobal(argtypes, ret)
        end
    elseif f === Core.get_binding_type
        if istoplevel(sv)
            ret = narrow_toplevel_binding_type(argtypes, ret)
        end
    end

    return ret
end

# check if this frame is for `getproperty(::Module, ::Symbol)`,
# that may access to abstract global variable traced by `analyzer`
function istoplevel_getproperty(sv::InferenceState)
    def = sv.linfo.def
    isa(def, Method) || return false
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = CC.frame_parent(sv)
    parent === nothing && return false
    return istoplevel(parent::InferenceState)
end

# if this `getglobal` access to a global variable in a module concretized by `AbstractAnalyzer`,
# eargely propagate its type (NOTE the logic here should be synced with the implementation
# of `abstract_eval_special_value(::AbstractAnalyzer, ...)`)
function narrow_toplevel_getglobal(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. constant or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    isdefined(gr.mod, gr.name) || return ret
    val = getglobal(gr.mod, gr.name)
    return isa(val, AbstractGlobal) ? val.t : Const(val)
end

# if the type for a variable hasn't been declared explicitly, return the narrower type
# declaration (`Const(Any)`) to allow JET to analyze the first assignment for the variable
# (, that would follow most of the time as generated by the frontend) more precisely
function narrow_toplevel_binding_type(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. already declared or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    @assert !isdefined(gr.mod, gr.name) "`get_binding_type` should resolve the already-defined variable type"
    return Const(Any)
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

function CC.finishinfer!(me::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int)
    ret = @invoke CC.finishinfer!(me::InferenceState, analyzer::AbstractInterpreter, cycleid::Int)

    istoplevel(me) || return ret

    # find assignments of abstract global variables, and assign types to them,
    # so that later analysis can refer to them

    stmts = me.src.code
    assigns = Dict{Int,Bool}() # slot id => is this deterministic
    for (pc, stmt) in enumerate(stmts)
        if isexpr(stmt, :(=))
            lhs = first(stmt.args)
            if isa(lhs, SlotNumber)
                slot = slot_id(lhs)
                if is_global_slot(analyzer, slot)
                    isd = is_deterministic(me.cfg, pc)
                    # COMBAK this approach is really not true when there're multiple
                    # assignments in different basic blocks
                    assigns[slot] = isd & get!(assigns, slot, isd)
                end
            end
        end
    end

    if !isempty(assigns)
        slottypes = collect_slottypes(me)
        for (slot, isd) in assigns
            slotname = get_global_slots(analyzer)[slot]
            typ = slottypes[slot]
            set_abstract_global!(analyzer, get_toplevelmod(analyzer), slotname, typ, isd, me)
        end
    end

    return ret
end

is_global_slot(analyzer::AbstractAnalyzer, slot::Int) = slot in keys(get_global_slots(analyzer))
is_global_slot(analyzer::AbstractAnalyzer, slot::SlotNumber) = is_global_slot(analyzer, slot_id(slot))
is_global_slot(analyzer::AbstractAnalyzer, sym::Symbol) = sym in values(get_global_slots(analyzer))

# check if `pc` may not be executed in a given control graph
function is_deterministic(cfg::CFG, pc::Int)
    domtree = CC.construct_domtree(cfg.blocks)
    bb = CC.block_for_inst(cfg, pc)
    return CC.dominates(domtree, bb, length(cfg.blocks))
end

function collect_slottypes(sv::InferenceState)
    body = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in 1:length(sv.slottypes)]
    ssavaluetypes = sv.ssavaluetypes
    for i = 1:length(body)
        expr = body[i]
        # find all reachable assignments to locals
        if CC.was_reached(sv, i) && isexpr(expr, :(=))
            lhs = expr.args[1]
            if isa(lhs, SlotNumber)
                typ = ssavaluetypes[i]
                @assert typ !== NOT_FOUND "active slot in unreached region"
                vt = typ
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    if otherTy === Bottom
                        slottypes[id] = vt
                    elseif otherTy === Any
                        slottypes[id] = Any
                    else
                        slottypes[id] = tmerge(otherTy, vt)
                    end
                end
            end
        end
    end
    return slottypes
end

function set_abstract_global!(analyzer::AbstractAnalyzer, mod::Module, name::Symbol,
                              @nospecialize(t), is_deterministic::Bool, sv::InferenceState)
    prev_agv = prev_t = nothing
    isconst = is_constant_declared(name, sv)

    # check if this global variable is already assigned previously
    if isdefined(mod, name)
        val = getglobal(mod, name)
        if isa(val, AbstractGlobal)
            prev_t = val.t
            if val.isconst && (prev_t′ = widenconst(prev_t)) !== (t′ = widenconst(t))
                warn_invalid_const_global!(name)
                ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t′, t′)
                return
            end
            prev_agv = val
        else
            prev_t = Core.Typeof(val)
            if Base.isconst(mod, name)
                invalid = prev_t !== (t′ = widenconst(t))
                if invalid || !isa(t, Const)
                    warn_invalid_const_global!(name)
                    if invalid
                        ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t, t′) # ignored by default
                    end
                    return
                end
                # otherwise, we can just redefine this constant, and Julia will warn it
                ex = isconst ? :(const $name = $(QuoteNode(t.val))) : :($name = $(QuoteNode(t.val)))
                return Core.eval(mod, ex)
            end
        end
    end

    isnew = isnothing(prev_t)

    # if this constant declaration is invalid, just report it and bail out
    if isconst && !isnew
        warn_invalid_const_global!(name)
        ReportPass(analyzer)(InvalidConstantDeclaration, analyzer, sv, mod, name) # ignored by default
        return
    end

    if is_deterministic
        # if this assignment happens deterministically, and the assigned value is known
        # to be constant statically, let's concretize it for good reason; doing so allows
        # us to use it in concrete interpretation and define structs with type aliases, etc.
        v = singleton_type(t)
        if v !== nothing
            if isconst
                @assert isnew # means, this is a valid constant declaration
                return Core.eval(mod, :(const $name = $(QuoteNode(v))))
            else
                # we've checked `mod.name` wasn't declared as constant previously
                return Core.eval(mod, :($name = $(QuoteNode(v))))
            end
        end
    elseif !isnew
        # if this assignment happens non-deterministically,
        # we need to take the previous type into account
        t = tmerge(prev_t, t)
    end

    # okay, we will define new abstract global variable from here on
    if isa(prev_agv, AbstractGlobal)
        return Core.eval(mod, :(let name = $name::$AbstractGlobal
            name.t = $t
            name
        end))
    else
        return Core.eval(mod, :($name = $(AbstractGlobal(t, isconst))))
    end
end

warn_invalid_const_global!(name::Symbol) = @warn """
JET.jl can't update the definition of this constant declared global variable: `$name`
This may fail, cause incorrect analysis, or produce unexpected errors.
"""

# IDEA we may want to hoist `InvalidConstXXX` errors into top-level errors

@jetreport struct InvalidConstantRedefinition <: InferenceErrorReport
    mod::Module
    name::Symbol
    @nospecialize t′
    @nospecialize t
end
function JETInterface.print_report_message(io::IO, report::InvalidConstantRedefinition)
    print(io, "invalid redefinition of constant `", report.mod, '.', report.name, "` (from `", report.t′, "` to `", report.t, "`)")
end
JETInterface.print_signature(::InvalidConstantRedefinition) = false

@jetreport struct InvalidConstantDeclaration <: InferenceErrorReport
    mod::Module
    name::Symbol
end
function JETInterface.print_report_message(io::IO, report::InvalidConstantDeclaration)
    print(io, "cannot declare a constant `", report.mod, '.', report.name, "`; it already has a value")
end
JETInterface.print_signature(::InvalidConstantDeclaration) = false

function is_constant_declared(name::Symbol, sv::InferenceState)
    return any(sv.src.code) do @nospecialize(x)
        if isexpr(x, :const)
            arg = first(x.args)
            # `transform_abstract_global_symbols!` replaces all the global symbols in this toplevel frame with `SlotNumber`s
            if isa(arg, SlotNumber)
                return get_slotname(sv, arg) === name
            end
        end
        return false
    end
end
