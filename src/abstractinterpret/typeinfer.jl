# top-level bridge
# ================

"""
    mutable struct AbstractGlobal
        t::Any     # analyzed type
        iscd::Bool # is this abstract global variable declarared as constant or not
    end

Wraps a global variable whose type is analyzed by abtract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
analysis may refer to or alter its type on future load and store operations.

!!! note
    The type of the wrapped global variable will be propagated only when in a toplevel frame,
    and thus we don't care about the analysis cache invalidation on a refinement of the
    wrapped global variable, since JET doesn't cache the toplevel frame.
"""
mutable struct AbstractGlobal
    t::Any     # analyzed type
    iscd::Bool # is this abstract global variable declarared as constant or not

    function AbstractGlobal(@nospecialize(t),
                            iscd::Bool,
                            )
        return new(t,
                   iscd,
                   )
    end
end

@doc """
    bail_out_toplevel_call(analyzer::AbstractAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::AbstractAnalyzer, ...)`, which keeps
inference on non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
CC.bail_out_toplevel_call(analyzer::AbstractAnalyzer, @nospecialize(sig), sv) = false

function CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if isa(e, Slot) && is_global_slot(analyzer, e)
            if get_slottype((sv, get_currpc(sv)), e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract intepret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(get_toplevelmod(analyzer), get_slotname(sv, e))
            end
        end
    end

    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    if istoplevel(sv)
        if isa(e, GlobalRef)
            mod, name = e.mod, e.name
            if isdefined(mod, name)
                # here we eagerly propagate the type of this global variable:
                # of course the traced type might be difference from its type in actual execution
                # e.g. we don't track global variable assignments that happen in a function,
                # but it's highly possible this is a toplevel callsite and we take a risk here,
                # otherwise we can't enter the analysis !
                val = getfield(mod, name)
                ret = isa(val, AbstractGlobal) ? val.t : Const(val)
            end
        end
    end

    return ret
end

function CC.abstract_eval_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # HACK if we encounter `_INACTIVE_EXCEPTION`, it means `ConcreteInterpreter` tried to
    # concretize an exception which was not actually thrown – yet the actual error hasn't
    # happened thanks to JuliaInterpreter's implementation detail, i.e. JuliaInterpreter
    # could retrieve `FrameData.last_exception`, which is initialized with
    # `_INACTIVE_EXCEPTION.instance` – but it's obviously not a sound approximation of an
    # actual execution and so here we will fix it to `Any`, since we don't analyze types of
    # exceptions in general
    if ret ⊑ _INACTIVE_EXCEPTION
        ret = Any
    end

    return ret
end

function CC.abstract_eval_statement(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if get_concretized(analyzer)[get_currpc(sv)]
            return Any # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end

    return @invoke CC.abstract_eval_statement(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)
end

function CC.builtin_tfunction(analyzer::AbstractAnalyzer, @nospecialize(f), argtypes::Array{Any,1},
                              sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::AbstractInterpreter, f, argtypes::Array{Any,1},
                                       sv::Union{InferenceState,Nothing})

    if f === getfield && 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const)
                    mod = obj.val
                    if isa(mod, Module)
                        if isdefined(mod, name)
                            if istoplevel_globalref(analyzer, sv)
                                # when accessing to a global variable in a module
                                # concretized by `analyzer`, eagerly propagate its type
                                # NOTE logic here should be synced with that of `abstract_eval_special_value(::AbstractAnalyzer, ::Any, ::VarTable, ::InferenceState)`
                                val = getfield(mod, name)
                                return isa(val, AbstractGlobal) ? val.t : Const(val)
                            end
                        end
                    end
                end
            end
        end
    elseif f === fieldtype
        # the valid widest possible return type of `fieldtype_tfunc` is `Union{Type,TypeVar}`
        # because fields of unwrapped `DataType`s can legally be `TypeVar`s,
        # but this will cause lots of false positive `NoMethodErrorReport`s for inference
        # with accessing to abstract fields since most methods don't expect `TypeVar`
        # (e.g. `@report_call readuntil(stdin, 'c')`)
        # JET.jl further widens this case to `Any` and give up further analysis rather than
        # trying hard to do sound and noisy analysis
        # xref: https://github.com/JuliaLang/julia/pull/38148
        if ret === Union{Type, TypeVar}
            return Any
        end
    end

    return ret
end

# check if this frame is for `getproperty(::Module, ::Symbol)`, which accesses to a global
# variable traced by `analyzer`
function istoplevel_globalref(analyzer::AbstractAnalyzer, sv::InferenceState)
    def = sv.linfo.def
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = sv.parent
    return !isnothing(parent) && istoplevel(parent)
end

# inter-procedural
# ================

function collect_callee_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    reports = get_caller_cache(analyzer)
    if !isempty(reports)
        vf = get_virtual_frame(sv)
        for report in reports
            pushfirst!(report.vst, vf)
            add_new_report!(sv.result, report)
        end
        empty!(reports)
    end
end

# works within inter-procedural context
function CC.abstract_call_method(analyzer::AbstractAnalyzer, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    ret = @invoke CC.abstract_call_method(analyzer::AbstractInterpreter, method::Method, sig, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)

    collect_callee_reports!(analyzer, sv)

    @static if VERSION < v"1.8.0-DEV.510"
        # manually take in https://github.com/JuliaLang/julia/pull/42195
        if method === ISEQUAL_ANY_ANY && ret.rt === Union{Bool,Missing}
            ret = MethodCallResult(Bool, ret.edgecycle, ret.edgelimited, ret.edge)
        end
    end

    return ret
end

@static if VERSION < v"1.8.0-DEV.510"
    # manually take in https://github.com/JuliaLang/julia/pull/42195
    const ISEQUAL_ANY_ANY = let
        ms = methods(isequal)
        i = findfirst(m->m.sig===Tuple{typeof(isequal),Any,Any}, ms)::Int
        ms[i]
    end
end

function CC.abstract_call_method_with_const_args(analyzer::AbstractAnalyzer, result::MethodCallResult,
                                                 @nospecialize(f), arginfo::(@static IS_AFTER_42529 ? ArgInfo : Argtypes), match::MethodMatch,
                                                 sv::InferenceState, va_override::Bool)
    set_cacher!(analyzer, :abstract_call_method_with_const_args => sv.result)

    const_result =
        @invoke CC.abstract_call_method_with_const_args(analyzer::AbstractInterpreter, result::MethodCallResult,
                                                        @nospecialize(f), arginfo::(@static IS_AFTER_42529 ? ArgInfo : Argtypes), match::MethodMatch,
                                                        sv::InferenceState, va_override::Bool)

    # we should make sure we reset the cacher because at this point we may have not hit
    # `CC.cache_lookup(linfo::MethodInstance, given_argtypes::Argtypes, cache::JETLocalCache)`
    set_cacher!(analyzer, nothing)

    if const_result !== nothing
        # successful constant prop', we also need to update reports
        collect_callee_reports!(analyzer, sv)
    end

    return const_result
end

@static if IS_AFTER_42529
function CC.abstract_call(analyzer::AbstractAnalyzer, arginfo::ArgInfo,
                          sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke CC.abstract_call(analyzer::AbstractInterpreter, arginfo::ArgInfo,
                                   sv::InferenceState, max_methods::Int)
    analyze_task_parallel_code!(analyzer, arginfo.argtypes, sv)
    return ret
end
else # @static if IS_AFTER_42529
function CC.abstract_call(analyzer::AbstractAnalyzer, fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes,
                          sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke CC.abstract_call(analyzer::AbstractInterpreter, fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes,
                                   sv::InferenceState, max_methods::Int)
    analyze_task_parallel_code!(analyzer, argtypes, sv)
    return ret
end
end # @static if IS_AFTER_42529

"""
    analyze_task_parallel_code!(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)

Adds special cased analysis pass for task parallelism.
In Julia's task parallelism implementation, parallel code is represented as closure and it's
wrapped in a `Task` object. `NativeInterpreter` doesn't infer nor optimize the bodies of
those closures when compiling code that creates parallel tasks, but JET will try to run
additional analysis pass by recurring into the closures.

See also: <https://github.com/aviatesk/JET.jl/issues/114>

!!! note
    JET won't do anything other than doing JET analysis, e.g. won't annotate return type
    of wrapped code block in order to not confuse the original `AbstractInterpreter` routine
    track <https://github.com/JuliaLang/julia/pull/39773> for the changes in native abstract
    interpretation routine.
"""
function analyze_task_parallel_code!(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
    f = singleton_type(argtypes[1])

    # TODO we should analyze a closure wrapped in a `Task` only when it's `schedule`d
    # But the `Task` construction may not happen in the same frame where it's `schedule`d
    # and so we may not be able to access to the closure at that point.
    # As a compromise, here we invoke the additional analysis on `Task` construction,
    # regardless of whether it's really `schedule`d or not.
    if f === Task &&
       length(argtypes) ≥ 2 &&
       (v = argtypes[2]; v ⊑ Function)
        # if we encounter `Task(::Function)`, try to get its inner function and run analysis on it
        # the closure can be a nullary lambda that really doesn't depend on
        # the captured environment, and in that case we can retrieve it as
        # a function object, otherwise we will try to retrieve the type of the closure
        ft = (isa(v, Const) ? Core.Typeof(v.val) :
              isa(v, Core.PartialStruct) ? v.typ :
              isa(v, DataType) ? v :
              return)::Type
        analyze_additional_pass_by_type!(analyzer, Tuple{ft}, sv)
        return
    end

    return nothing
end

# run additional interpretation with a new analyzer
function analyze_additional_pass_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}), sv::InferenceState)
    newanalyzer = AbstractAnalyzer(analyzer)

    # in order to preserve the inference termination, we keep to use the current frame
    # and borrow the `AbstractInterpreter`'s cycle detection logic
    # XXX the additional analysis pass by `abstract_call_method` may involve various site-effects,
    # but what we're doing here is essentially equivalent to modifying the user code and inlining
    # the threaded code block as a usual code block, and thus the side-effects won't (hopefully)
    # confuse the abstract interpretation, which is supposed to terminate on any kind of code
    mm = get_single_method_match(tt, InferenceParams(newanalyzer).MAX_METHODS, get_world_counter(newanalyzer))
    abstract_call_method(newanalyzer, mm.method, mm.spec_types, mm.sparams, false, sv)

    return nothing
end

# `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
# and here we should NOT catch error reports detected within the simulated call
# because it is really not any abstraction of actual execution
function CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
    # stash and discard the result from the simulated call, and keep the original result (`result0`)
    result = sv.result
    result0 = result.src::JETResult
    set_result!(result)
    ret = @invoke return_type_tfunc(analyzer::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState)
    set_result!(sv.result, result0)
    return ret
end

# cache
# =====

# global
# ------

"""
    JET_CACHE::$(typeof(JET_CACHE))

Keeps `src::CodeInstance` cache associated with `mi::MethodInstace` that represents the
analysis result on `mi` performed by [`analyzer::AbstractAnalyzer`](@ref AbstractAnalyzer),
where [`src.inferred::JETCachedResult`](@ref JETCachedResult) caches JET's analysis result.
This cache is separated by the identities of `AbstractAnalyzer`s, which are hash keys
computed by [`get_cache_key(analyzer::AbstractAnalyzer)`](@ref get_cache_key).

`JET_CACHE` is completely separated from the `NativeInterpreter`'s global cache, so that
JET's analysis never interacts with actual code execution.
"""
const JET_CACHE = IdDict{UInt, IdDict{MethodInstance,CodeInstance}}()

# just used for interactive developments
__clear_caches!() = empty!(JET_CACHE)

function CC.code_cache(analyzer::AbstractAnalyzer)
    cache  = JETGlobalCache(analyzer)
    worlds = WorldRange(get_world_counter(analyzer))
    return WorldView(cache, worlds)
end

struct JETGlobalCache{Analyzer<:AbstractAnalyzer}
    analyzer::Analyzer
end

# cache existence for this `analyzer` is ensured on its construction
jet_cache(analyzer::AbstractAnalyzer)       = JET_CACHE[get_cache_key(analyzer)]
jet_cache(wvc::WorldView{<:JETGlobalCache}) = jet_cache(wvc.cache.analyzer)

CC.haskey(wvc::WorldView{<:JETGlobalCache}, mi::MethodInstance) = haskey(jet_cache(wvc), mi)

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    # enable the report cache restoration at `code = get(code_cache(interp), mi, nothing)`
    set_cacher!(analyzer, :typeinf_edge => caller.result)
    return @invoke typeinf_edge(analyzer::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
end

function CC.get(wvc::WorldView{<:JETGlobalCache}, mi::MethodInstance, default)
    codeinf = get(jet_cache(wvc), mi, default) # will ignore native code cache for a `MethodInstance` that is not analyzed by JET yet

    analyzer = wvc.cache.analyzer

    # XXX this relies on a very dirty analyzer state manipulation, the reason for this is
    # that this method (and `code_cache(::AbstractAnalyzer)`) can be called from multiple
    # contexts including edge inference, constant prop' heuristics and inlining, where we
    # want to use report cache only in edge inference, but we can't tell which context is
    # the caller of this specific method call here and thus can't tell whether we should
    # enable report cache reconstruction without the information
    # XXX move this logic into `typeinf_edge` ?
    cacher = get_cacher(analyzer)
    if isa(cacher, Pair{Symbol,InferenceResult})
        setter, caller = cacher
        if setter === :typeinf_edge
            if isa(codeinf, CodeInstance)
                # cache hit, now we need to append cached reports associated with this `MethodInstance`
                for cached in get_cached_reports(codeinf.inferred::JETCachedResult)
                    restored = add_cached_report!(caller, cached)
                    @static if JET_DEV_MODE
                        actual, expected = first(restored.vst).linfo, mi
                        @assert actual === expected "invalid global cache restoration, expected $expected but got $actual"
                    end
                    add_caller_cache!(analyzer, restored) # should be updated in `abstract_call` (after exiting `typeinf_edge`)
                end
            end
            set_cacher!(analyzer, nothing)
        end
    end

    return codeinf
end

function CC.getindex(wvc::WorldView{<:JETGlobalCache}, mi::MethodInstance)
    r = CC.get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

function CC.transform_result_for_cache(analyzer::AbstractAnalyzer, linfo::MethodInstance,
                                       valid_worlds::WorldRange, @nospecialize(inferred_result))
    jetresult = inferred_result::JETResult
    cache = InferenceErrorReportCache[]
    for report in get_reports(jetresult)
        @static if JET_DEV_MODE
            actual, expected = first(report.vst).linfo, linfo
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cache, report)
    end
    return JETCachedResult(cache, get_source(jetresult))
end

function CC.setindex!(wvc::WorldView{<:JETGlobalCache}, ci::CodeInstance, mi::MethodInstance)
    setindex!(jet_cache(wvc), ci, mi)
    return nothing
end

function add_jet_callback!(linfo)
    if !isdefined(linfo, :callbacks)
        linfo.callbacks = Any[invalidate_jet_cache!]
    else
        callbacks = linfo.callbacks::Vector{Any}
        if !any(function (@nospecialize(cb),)
                    cb === invalidate_jet_cache!
                end,
                callbacks)
            push!(callbacks, invalidate_jet_cache!)
        end
    end
    return nothing
end

function invalidate_jet_cache!(replaced, max_world, depth = 0)
    for cache in values(JET_CACHE)
        delete!(cache, replaced)
    end

    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            mi = mi::MethodInstance
            if !any(cache->haskey(cache, mi), values(JET_CACHE))
                continue # otherwise fall into infinite loop
            end
            invalidate_jet_cache!(mi, max_world, depth+1)
        end
    end
    return nothing
end

# local
# -----

struct JETLocalCache{Analyzer<:AbstractAnalyzer}
    analyzer::Analyzer
    cache::Vector{InferenceResult}
end

CC.get_inference_cache(analyzer::AbstractAnalyzer) = JETLocalCache(analyzer, get_inference_cache(get_native(analyzer)))

function CC.cache_lookup(linfo::MethodInstance, given_argtypes::Argtypes, cache::JETLocalCache)
    # XXX the very dirty analyzer state observation again
    # this method should only be called from the single context i.e. `abstract_call_method_with_const_args`,
    # and so we should reset the cacher immediately we reach here
    analyzer = cache.analyzer
    setter, caller = get_cacher(analyzer)::Pair{Symbol,InferenceResult}
    @assert setter === :abstract_call_method_with_const_args
    set_cacher!(analyzer, nothing)

    inf_result = cache_lookup(linfo, given_argtypes, cache.cache)

    isa(inf_result, InferenceResult) || return inf_result

    # constant prop' hits a cycle (recur into same non-constant analysis), we just bail out
    isa(inf_result.result, InferenceState) && return inf_result

    # cache hit, try to restore local report caches

    # corresponds to the throw-away logic in `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
    filter!(!is_from_same_frame(caller.linfo, linfo), get_reports(caller))

    for cached in get_cached_reports(inf_result)
        restored = add_cached_report!(caller, cached)
        @static if JET_DEV_MODE
            actual, expected = first(restored.vst).linfo, linfo
            @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
        end
        add_caller_cache!(analyzer, restored) # should be updated in `abstract_call_method_with_const_args`
    end

    return inf_result
end

CC.push!(cache::JETLocalCache, inf_result::InferenceResult) = CC.push!(cache.cache, inf_result)

# main driver
# ===========

# in this overload we will work on some meta/debug information management per inference frame
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    (; linfo, parent, result) = frame
    isentry = isnothing(parent)

    #= logging stage1 start =#
    local sec::Float64
    logger_activated = isa(JETLogger(analyzer).inference_logger, IO)
    depth = get_depth(analyzer)
    if logger_activated
        sec = time()
        with_inference_logger(analyzer, ==(DEBUG_LOGGER_LEVEL)) do @nospecialize(io)
            print_rails(io, depth)
            printstyled(io, "┌ @ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            print(io, linfo)
            file, line = get_file_line(linfo)
            print(io, ' ', file, ':', line)
            println(io)
            set_depth!(analyzer, get_depth(analyzer) + 1) # manipulate this only in debug mode
        end
    end
    #= logging stage1 end =#

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previous non-constant inference result (under the current design constant prop' always
    # happens after inference with non-constant abstract elements)
    # JET also needs that in order to reduce false positive reports, and here we will
    # throw-away previously-collected error reports that are "lineage" of this frame,
    # when it is being re-inferred with constants
    # NOTE `frame.linfo` is the exactly same object as that of the previous non-constant inference
    # IDEA we may still want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    # even when constant prop' reveals it never happ∫ens given the current constant arguments
    if is_constant_propagated(frame) && !isentry
        filter!(!is_from_same_frame(parent.linfo, linfo), get_reports(parent.result))
    end

    @assert isa(result.src, JETResult)

    ret = @invoke typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    ret && @assert isa(result.src, isentry ? JETResult : JETCachedResult)

    #= logging stage2 start =#
    if logger_activated
        elapsed = round(time() - sec; digits = 3)
        with_inference_logger(analyzer, ==(INFO_LOGGER_LEVEL)) do @nospecialize(io)
            println(io, "inference on $linfo finished in $elapsed sec")
        end
        with_inference_logger(analyzer, ==(DEBUG_LOGGER_LEVEL)) do @nospecialize(io)
            print_rails(io, depth)
            printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            printstyled(io, frame.bestguess; color = TYPE_ANNOTATION_COLOR)
            println(io, " (", join(filter(!isnothing, (
                             linfo,
                             ret ? nothing : "in cycle",
                             string(length((isentry ? get_reports : get_cached_reports)(result)), " reports"),
                             string(elapsed, " sec"),
                             )), ", "),
                         ')')
            set_depth!(analyzer, get_depth(analyzer) - 1) # manipulate this only in debug mode
        end
    end
    #= logging stage2 end =#

    return ret
end

"""
    is_from_same_frame(parent_linfo::MethodInstance, current_linfo::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport` is generated from `current_linfo`.
It also checks `current_linfo` is a "lineage" of `parent_linfo` (i.e. entered from it).

This function is supposed to be used to filter out reports collected from analysis on `current_linfo`
without using constants when entering into the constant analysis. As such, this function
assumes that when a report should be filtered out, the first elment of its virtual stack
frame `st` is for `parent_linfo` and the second element of that is for `current_linfo`.

Example: Assume `linfo2` will produce a report for some reason.
```
entry
└─ linfo1
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report1: linfo1->linfo2, report2: linfo3->linfo2)
   │  └─ linfo2 (report1: linfo1->linfo2, report2: linfo2)
   └─ linfo3′ (report1: linfo1->linfo2, ~~report2: linfo1->linfo3->linfo2~~)
```
In the example analysis above, `report2` will be filtered out on re-entering into `linfo3′`
(i.e. we're analyzing `linfo3` with constants argument), because
`is_from_same_frame(linfo1, linfo3)(report2)` returns `true`.
Note that `report1` is still kept there because of the lineage check, i.e.
`is_from_same_frame(linfo1, linfo3)(report1)` returns `false`.
"""
function is_from_same_frame(parent_linfo::MethodInstance,
                            current_linfo::MethodInstance,
                            )
    function (report::InferenceErrorReport)
        @inbounds begin
            vst = report.vst
            length(vst) > 1 || return false
            vst[1].linfo === parent_linfo || return false
            return vst[2].linfo === current_linfo
        end
    end
end

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    CC.typeinf_nocycle(analyzer, frame) || return false # frame is now part of a higher cycle
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    valid_worlds = WorldRange()
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
        # might might not fully intersect these earlier, so do that now
        valid_worlds = CC.intersect(caller.valid_worlds, valid_worlds)
    end
    for caller in frames
        caller.valid_worlds = valid_worlds
        CC.finish(caller, analyzer)
        # finalize and record the linfo result
        caller.inferred = true
    end
    # collect results for the new expanded frame
    results = Tuple{InferenceState, Vector{Any}, Bool}[
            ( frames[i],
              frames[i].stmt_edges[1]::Vector{Any},
              frames[i].cached )
        for i in 1:length(frames) ]
    empty!(frames)
    for (frame, _, _) in results
        caller = frame.result
        opt = get_source(caller)
        if opt isa OptimizationState # implies `may_optimize(analyzer) === true`
            result_type = caller.result
            @assert !(result_type isa LimitedAccuracy)
            CC.optimize(analyzer, opt, OptimizationParams(analyzer), result_type)
            # # COMBAK we may want to enable inlining ?
            # if opt.const_api
            #     # XXX: The work in ir_to_codeinf! is essentially wasted. The only reason
            #     # we're doing it is so that code_llvm can return the code
            #     # for the `return ...::Const` (which never runs anyway). We should do this
            #     # as a post processing step instead.
            #     CC.ir_to_codeinf!(opt)
            #     if result_type isa Const
            #         caller.src = result_type
            #     else
            #         @assert CC.isconstType(result_type)
            #         caller.src = Const(result_type.parameters[1])
            #     end
            # end
            caller.valid_worlds = CC.getindex((opt.inlining.et::CC.EdgeTracker).valid_worlds)
        end
    end

    for (frame, edges, cached) in results
        caller = frame.result
        valid_worlds = caller.valid_worlds
        if CC.last(valid_worlds) >= get_world_counter()
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            CC.store_backedges(caller, edges)
        end
        CC.finish!(analyzer, frame)

        # XXX this is a dirty fix for performance problem, we need more "proper" fix
        # https://github.com/aviatesk/JET.jl/issues/75
        unique!(aggregation_policy(analyzer), get_reports(caller))

        # global cache management
        # part 1: transform collected reports to `JETCachedResult` and put it into `CodeInstance.inferred`
        if cached && !istoplevel(frame)
            CC.cache_result!(analyzer, caller)
        end
        # part 2: register invalidation callback for JET cache
        add_jet_callback!(caller.linfo)

        reports = get_reports(caller)
        if frame.parent !== nothing
            # inter-procedural handling: get back to the caller what we got from these results
            add_caller_cache!(analyzer, reports)

            # local cache management
            # TODO there are duplicated work here and `transform_result_for_cache`
            cache = InferenceErrorReportCache[]
            for report in reports
                cache_report!(cache, report)
            end
            set_cached_result!(caller, cache)
        end
    end

    return true
end

function CC.finish(me::InferenceState, analyzer::AbstractAnalyzer)
    # @invoke CC.finish(me::InferenceState, analyzer::AbstractInterpreter)
    # prepare to run optimization passes on fulltree
    s_edges = me.stmt_edges[1]
    if s_edges === nothing
        s_edges = me.stmt_edges[1] = []
    end
    for edges in me.stmt_edges
        edges === nothing && continue
        edges === s_edges && continue
        append!(s_edges, edges)
        empty!(edges)
    end
    if me.src.edges !== nothing
        edges = me.src.edges
        if isa(edges, Vector{Any})
            append!(s_edges, edges)
        else
            # this pass should never happen in ordinal code, but some external
            # code generator like IRTools.jl may produce this case
            # see https://github.com/aviatesk/JET.jl/issues/271
            append!(s_edges, edges::Vector{MethodInstance})
        end
        me.src.edges = nothing
    end
    # inspect whether our inference had a limited result accuracy,
    # else it may be suitable to cache
    me.bestguess = CC.cycle_fix_limited(me.bestguess, me)
    limited_ret = me.bestguess isa LimitedAccuracy
    limited_src = false
    if !limited_ret
        gt = me.src.ssavaluetypes::Vector{Any}
        for j = 1:length(gt)
            gt[j] = gtj = CC.cycle_fix_limited(gt[j], me)
            if gtj isa LimitedAccuracy && me.parent !== nothing
                limited_src = true
                break
            end
        end
    end
    if limited_ret
        # a parent may be cached still, but not this intermediate work:
        # we can throw everything else away now
        set_source!(me.result, nothing)
        me.cached = false
        me.src.inlineable = false
        unlock_mi_inference(analyzer, me.linfo)
    elseif limited_src
        # a type result will be cached still, but not this intermediate work:
        # we can throw everything else away now
        set_source!(me.result, nothing)
        me.src.inlineable = false
    else
        # annotate fulltree with type information,
        # either because we are the outermost code, or we might use this later
        doopt = (me.cached || me.parent !== nothing)
        CC.type_annotate!(me, doopt)
        if doopt && may_optimize(analyzer)
            set_source!(me.result, OptimizationState(me, OptimizationParams(analyzer), analyzer))
        else
            set_source!(me.result, me.src::CodeInfo) # stash a convenience copy of the code (e.g. for reflection)
        end
    end
    me.result.valid_worlds = me.valid_worlds
    me.result.result = me.bestguess

    if istoplevel(me)
        # find assignments of abstract global variables, and assign types to them,
        # so that later analysis can refer to them

        stmts = me.src.code
        cfg = compute_basic_blocks(stmts)
        assigns = Dict{Int,Bool}() # slot id => is this deterministic
        for (pc, stmt) in enumerate(stmts)
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, Slot)
                    slot = slot_id(lhs)
                    if is_global_slot(analyzer, slot)
                        isnd = is_assignment_nondeterministic(cfg, pc)

                        # COMBAK this approach is really not true when there're multiple
                        # assignments in different basic blocks
                        if haskey(assigns, slot)
                            assigns[slot] |= isnd
                        else
                            assigns[slot] = isnd
                        end
                    end
                end
            end
        end

        if !isempty(assigns)
            slottypes = collect_slottypes(me)
            for (slot, isnd) in assigns
                slotname = get_global_slots(analyzer)[slot]
                typ = slottypes[slot]
                set_abstract_global!(analyzer, get_toplevelmod(analyzer), slotname, typ, isnd, me)
            end
        end
    end
end

# simple cfg analysis to check if the assignment at `pc` will happen non-deterministically
function is_assignment_nondeterministic(cfg::CFG, pc::Int)
    isnd = false

    blocks = cfg.blocks
    for (idx, block) in enumerate(blocks)
        if pc in rng(block)
            for block′ in blocks
                succs = block′.succs
                if idx in succs
                    isnd |= length(succs) > 1
                end
            end
        end
    end

    return isnd
end

# at this point all the types of SSA values are iterated to maximum fixed point,
# and we can compute types of slot as least upper bound of types of all the possible
# assignment of the slot (the type of assignment statement is available as SSA value type)
# the implementation is mostly same as `record_slot_assign!(sv::InferenceState)`, but
# we don't `widenconst` each SSA value type
function collect_slottypes(sv::InferenceState)
    states = sv.stmt_types
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    stmts = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in sv.src.slottypes::Vector{Any}]
    for i = 1:length(stmts)
        stmt = stmts[i]
        state = states[i]
        # find all reachable assignments to locals
        if isa(state, VarTable) && isexpr(stmt, :(=))
            lhs = first(stmt.args)
            if isa(lhs, Slot)
                vt = ssavaluetypes[i] # don't widen const
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    slottypes[id] = tmerge(otherTy, vt)
                end
            end
        end
    end
    return slottypes
end

function set_abstract_global!(analyzer::AbstractAnalyzer, mod::Module, name::Symbol, @nospecialize(t), isnd::Bool, sv::InferenceState)
    prev_agv = nothing
    prev_t = nothing
    iscd = is_constant_declared(name, sv)

    # check if this global variable is already assigned previously
    if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, AbstractGlobal)
            prev_t = val.t
            if val.iscd && (prev_t′ = widenconst(prev_t)) !== (t′ = widenconst(t))
                warn_invalid_const_global!(name)
                ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t′, t′)
                return
            end
            prev_agv = val
        else
            prev_t = Core.Typeof(val)
            if isconst(mod, name)
                invalid = prev_t !== (t′ = widenconst(t))
                if invalid || !isa(t, Const)
                    warn_invalid_const_global!(name)
                    if invalid
                        ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t, t′) # ignored by default
                    end
                    return
                end
                # otherwise, we can just redefine this constant, and Julia will warn it
                ex = iscd ? :(const $name = $(QuoteNode(t.val))) : :($name = $(QuoteNode(t.val)))
                return Core.eval(mod, ex)
            end
        end
    end

    isnew = isnothing(prev_t)

    # if this constant declaration is invalid, just report it and bail out
    if iscd && !isnew
        warn_invalid_const_global!(name)
        ReportPass(analyzer)(InvalidConstantDeclaration, analyzer, sv, mod, name) # ignored by default
        return
    end

    # if this assignment happens non-deterministically, we need to take the previous type into account
    if isnd
        if !isnew # if this assignment is an initialization, we just need to use `t`
            t = tmerge(prev_t, t)
        end
    else
        # if this assignment happens deterministically, and the assigned value is known to be
        # constant statically, let's concretize it for good reasons;
        # we will be able to use it in concrete interpretation and so this allows to define
        # structs with type aliases, etc.
        if isa(t, Const)
            if iscd
                @assert isnew # means, this is a valid constant declaration
                return Core.eval(mod, :(const $name = $(QuoteNode(t.val))))
            else
                # we've checked `mod.name` wasn't declared as constant previously
                return Core.eval(mod, :($name = $(QuoteNode(t.val))))
            end
        end
    end

    # okay, we will define new abstract global variable from here on
    if isa(prev_agv, AbstractGlobal)
        return Core.eval(mod, :(let name = $name::$AbstractGlobal
            name.t = $t
            name
        end))
    else
        return Core.eval(mod, :($name = $(AbstractGlobal(t, iscd))))
    end
end

warn_invalid_const_global!(name::Symbol) = @warn """
JET.jl can't update the definition of this constant declared global variable: `$name`
This may fail, cause incorrect analysis, or produce unexpected errors.
"""

# IDEA we may want to hoist `InvalidConstXXX` errors into top-level errors

@reportdef struct InvalidConstantRedefinition <: InferenceErrorReport
    mod::Module
    name::Symbol
    @nospecialize(t′::Any)
    @nospecialize(t::Any)
end
get_msg(::Type{InvalidConstantRedefinition}, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(t′::Any), @nospecialize(t::Any)) =
    "invalid redefinition of constant $(mod).$(name) (from $(t′) to $(t))"

@reportdef struct InvalidConstantDeclaration <: InferenceErrorReport
    mod::Module
    name::Symbol
end
get_msg(::Type{InvalidConstantDeclaration}, sv::InferenceState, mod::Module, name::Symbol) =
    "cannot declare a constant $(mod).$(name); it already has a value"

function is_constant_declared(name::Symbol, sv::InferenceState)
    return any(sv.src.code) do @nospecialize(x)
        if isexpr(x, :const)
            arg = first(x.args)
            # `transform_abstract_global_symbols!` replaces all the global symbols in this toplevel frame with `Slot`s
            if isa(arg, Slot)
                return get_slotname(sv, arg) === name
            end
        end
        return false
    end
end

function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
    caller = frame.result

    # transform optimized `IRCode` to optimized `CodeInfo`
    src = get_source(caller)
    if src isa OptimizationState # implies `may_optimize(analyzer) === true`
        opt = src
        @assert opt.ir !== nothing # `_typeinf(::AbstractAnalyzer, ::InferenceState)` disabled `const_api`

        src = CC.ir_to_codeinf!(opt)
        set_source!(caller, src)
    end

    return src
end
