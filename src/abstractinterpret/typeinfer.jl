# report management
# =================

function collect_callee_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    reports = get_caller_cache(analyzer)
    if !isempty(reports)
        vf = get_virtual_frame(sv)
        for report in reports
            pushfirst!(report.vst, vf)
            add_new_report!(analyzer, sv.result, report)
        end
        empty!(reports)
    end
end


let # overload `abstract_call_method`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, si::StmtInfo, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            method::Method, sig::Any, sparams::SimpleVector, hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            method::Method, sig::Any, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    end
    @eval function CC.abstract_call_method($(sigs_ex.args...))
        ret = @invoke CC.abstract_call_method($(args_ex.args...))
        collect_callee_reports!(analyzer, sv)
        return ret
    end
end

let # overload `abstract_call_method_with_const_args`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, match::MethodMatch,
            sv::InferenceState, $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, si::StmtInfo, match::MethodMatch,
            sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    elseif isdefined(CC, :InvokeCall)
        # https://github.com/JuliaLang/julia/pull/46743
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    elseif hasmethod(CC.abstract_call_method_with_const_args, (AbstractInterpreter,
        MethodCallResult, Any, ArgInfo, MethodMatch,
        InferenceState, Any))
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, @nospecialize(invoketypes=nothing))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, invoketypes::Any)
    elseif IS_V18
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState)
    elseif IS_AFTER_42529
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, va_override::Bool)
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, va_override::Bool)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), argtypes::Argtypes, match::MethodMatch,
            sv::InferenceState, va_override::Bool)
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, argtypes::Argtypes, match::MethodMatch,
            sv::InferenceState, va_override::Bool)
    end
    @eval function CC.abstract_call_method_with_const_args($(sigs_ex.args...))
        set_cacher!(analyzer, :abstract_call_method_with_const_args => sv.result)
        const_result =
            @invoke CC.abstract_call_method_with_const_args($(args_ex.args...))
        # we should make sure we reset the cacher because at this point we may have not hit
        # `CC.cache_lookup(linfo::MethodInstance, given_argtypes::Argtypes, cache::JETLocalCache)`
        set_cacher!(analyzer, nothing)
        if const_result !== nothing
            # successful constant prop', we also need to update reports
            collect_callee_reports!(analyzer, sv)
        end
        return const_result
    end
end

let # overload `concrete_eval_call`
    sv_available = true
    @static if VERSION ≥ v"1.9.0-DEV.1502"
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
    elseif @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
        sv_available = false
    elseif isdefined(CC, :InvokeCall)
        # https://github.com/JuliaLang/julia/pull/46743
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
    elseif IS_V18
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
    else
        return # `concrete_eval_call` isn't defined for this case
    end
    @eval function CC.concrete_eval_call($(sigs_ex.args...))
        ret = @invoke CC.concrete_eval_call($(args_ex.args...))
        if $(sv_available)
            if $(isdefined(CC, :ConstCallResults) ? :(ret isa CC.ConstCallResults) : :(ret !== nothing))
                # this frame has been happily concretized, now we throw away reports collected
                # during the previous abstract-interpretation based analysis
                filter_lineages!(analyzer, sv.result, result.edge::MethodInstance)
            end
        end
        return ret
    end
end

let # overload `abstract_call`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).MAX_METHODS))))
        args_ex = :(analyzer::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :(arginfo.argtypes)
    elseif IS_AFTER_42529
        sigs_ex = :(analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).MAX_METHODS))))
        args_ex = :(analyzer::AbstractInterpreter, arginfo::ArgInfo, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :(arginfo.argtypes)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer, fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).MAX_METHODS))))
        args_ex = :(analyzer::AbstractInterpreter, fargs::Union{Nothing,Vector{Any}}, argtypes::Argtypes, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :argtypes
    end
    @eval function CC.abstract_call($(sigs_ex.args...))
        ret = @invoke CC.abstract_call($(args_ex.args...))
        analyze_task_parallel_code!(analyzer, $(argtypes_ex), sv)
        return ret
    end
end

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
              return)
        analyze_additional_pass_by_type!(analyzer, Tuple{ft}, sv)
        return
    end

    return nothing
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
    mm = get_single_method_match(tt, InferenceParams(newanalyzer).MAX_METHODS, get_world_counter(newanalyzer))
    @static if @isdefined(StmtInfo)
        abstract_call_method(newanalyzer, mm.method, mm.spec_types, mm.sparams, #=hardlimit=#false, #=si=#StmtInfo(false), sv)
    else
        abstract_call_method(newanalyzer, mm.method, mm.spec_types, mm.sparams, #=hardlimit=#false, sv)
    end

    return nothing
end

let # overload `return_type_tfunc`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
        args_ex = :(AbstractAnalyzer(analyzer)::AbstractInterpreter, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
        args_ex = :(AbstractAnalyzer(analyzer)::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState)
    end
    # `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
    # and here we should NOT catch error reports detected within the simulated call
    # because it is really not any abstraction of actual execution
    @eval function CC.return_type_tfunc($(sigs_ex.args...))
        # stash and discard the result from the simulated call, and keep the original result (`result0`)
        result = sv.result
        oldresult = analyzer[result]
        init_result!(analyzer, result)
        ret = @invoke return_type_tfunc($(args_ex.args...))
        analyzer[result] = oldresult
        return ret
    end
end

# cache
# =====

cache_report!(cache, @nospecialize(report::InferenceErrorReport)) =
    push!(cache, copy_report′(report)::InferenceErrorReport)

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

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::InferenceState)
    # enable the report cache restoration at `code = get(code_cache(interp), mi, nothing)`
    set_cacher!(analyzer, :typeinf_edge => caller.result)
    return @invoke typeinf_edge(analyzer::AbstractInterpreter, method::Method, atype::Any, sparams::SimpleVector, caller::InferenceState)
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
                for cached in (codeinf.inferred::JETCachedResult).reports
                    restored = add_cached_report!(analyzer, caller, cached)
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

@static if !hasmethod(CC.transform_result_for_cache, (
    AbstractInterpreter, MethodInstance, WorldRange, InferenceResult))
function CC.cache_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    valid_worlds = result.valid_worlds
    if CC.last(valid_worlds) == get_world_counter()
        # if we've successfully recorded all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        valid_worlds = WorldRange(CC.first(valid_worlds), typemax(UInt))
    end
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    linfo = result.linfo
    already_inferred = CC.already_inferred_quick_test(analyzer, linfo)
    if !already_inferred && CC.haskey(WorldView(code_cache(analyzer), valid_worlds), linfo)
        already_inferred = true
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        inferred_result = transform_result_for_cache(analyzer, linfo, valid_worlds, result)
        CC.setindex!(code_cache(analyzer), CodeInstance(result, inferred_result, valid_worlds), linfo)
    end
    unlock_mi_inference(analyzer, linfo)
    nothing
end
end # @static if hasmethod(CC.transform_result_for_cache, (...))

function CC.transform_result_for_cache(analyzer::AbstractAnalyzer,
    linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    cache = InferenceErrorReport[]
    for report in get_reports(analyzer, result)
        @static if JET_DEV_MODE
            actual, expected = first(report.vst).linfo, linfo
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cache, report)
    end
    @static if hasmethod(CC.transform_result_for_cache, (
        AbstractInterpreter, MethodInstance, WorldRange, InferenceResult))
        inferred_result = @invoke transform_result_for_cache(analyzer::AbstractInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    elseif isdefined(CC, :Effects) && hasmethod(CC.transform_result_for_cache, (
        AbstractInterpreter, MethodInstance, WorldRange, Any, CC.Effects))
        inferred_result = @invoke transform_result_for_cache(analyzer::AbstractInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, result.src::Any, result.ipo_effects::CC.Effects)
    else
        inferred_result = @invoke transform_result_for_cache(analyzer::AbstractInterpreter,
            linfo::MethodInstance, valid_worlds::WorldRange, result.src::Any)
    end
    return JETCachedResult(inferred_result, cache)
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

let
    if isdefined(CC, :AbstractLattice)
        sigs_ex = :(lattice::CC.AbstractLattice, linfo::MethodInstance, given_argtypes::Argtypes, cache::JETLocalCache)
        args_ex = :(lattice, linfo, given_argtypes, cache.cache)
    else
        sigs_ex = :(linfo::MethodInstance, given_argtypes::Argtypes, cache::JETLocalCache)
        args_ex = :(linfo, given_argtypes, cache.cache)
    end
    @eval function CC.cache_lookup($(sigs_ex.args...))
        # XXX the very dirty analyzer state observation again
        # this method should only be called from the single context i.e. `abstract_call_method_with_const_args`,
        # and so we should reset the cacher immediately we reach here
        analyzer = cache.analyzer
        setter, caller = get_cacher(analyzer)::Pair{Symbol,InferenceResult}
        @assert setter === :abstract_call_method_with_const_args
        set_cacher!(analyzer, nothing)

        inf_result = cache_lookup($(args_ex.args...))

        isa(inf_result, InferenceResult) || return inf_result

        # constant prop' hits a cycle (recur into same non-constant analysis), we just bail out
        isa(inf_result.result, InferenceState) && return inf_result

        # cache hit, try to restore local report caches

        # corresponds to the throw-away logic in `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
        filter_lineages!(analyzer, caller, linfo)

        for cached in get_cached_reports(analyzer, inf_result)
            restored = add_cached_report!(analyzer, caller, cached)
            @static if JET_DEV_MODE
                actual, expected = first(restored.vst).linfo, linfo
                @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
            end
            add_caller_cache!(analyzer, restored) # should be updated in `abstract_call_method_with_const_args`
        end

        return inf_result
    end
end

CC.push!(cache::JETLocalCache, inf_result::InferenceResult) = CC.push!(cache.cache, inf_result)

# main driver
# ===========

# in this overload we will work on some meta/debug information management per inference frame
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    (; linfo, parent, result) = frame
    isentry = isnothing(parent)

    #= logging stage1 start =#
    # io = stdout
    # sec = time()
    # depth = get_depth(analyzer)
    # print_rails(io, depth)
    # printstyled(io, "┌ @ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
    # print(io, linfo)
    # file, line = get_file_line(linfo)
    # print(io, ' ', file, ':', line)
    # println(io)
    # set_depth!(analyzer, get_depth(analyzer) + 1) # manipulate this only in debug mode
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
        filter_lineages!(analyzer, parent.result, linfo)
    end

    ret = @invoke typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    #= logging stage2 start =#
    # elapsed = round(time() - sec; digits = 3)
    # print_rails(io, depth)
    # printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
    # printstyled(io, frame.bestguess; color = TYPE_ANNOTATION_COLOR)
    # println(io, " (", join(filter(!isnothing, (
    #                  linfo,
    #                  ret ? nothing : "in cycle",
    #                  string(length(get_any_reports(analyzer, result)), " reports"),
    #                  string(elapsed, " sec"),
    #                  )), ", "),
    #              ')')
    # set_depth!(analyzer, get_depth(analyzer) - 1) # manipulate this only in debug mode
    #= logging stage2 end =#

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
    # NOTE we don't discard `InferenceState`s here so that some analyzers can use them in `finish!`
    # # collect results for the new expanded frame
    # results = Tuple{InferenceResult, Vector{Any}, Bool}[
    #         ( frames[i].result,
    #           frames[i].stmt_edges[1]::Vector{Any},
    #           frames[i].cached )
    #     for i in 1:length(frames) ]
    # empty!(frames)
    for frame in frames
        caller = frame.result
        opt = caller.src
        if opt isa OptimizationState # implies `may_optimize(analyzer) === true`
            @static if VERSION ≥ v"1.8.0-DEV.1425"
                CC.optimize(analyzer, opt, OptimizationParams(analyzer), caller)
            else
                result_type = caller.result
                @assert !(result_type isa LimitedAccuracy)
                CC.optimize(analyzer, opt, OptimizationParams(analyzer), result_type)
            end
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

    for frame in frames
        caller = frame.result
        edges = frame.stmt_edges[1]::Vector{Any}
        cached = frame.cached
        valid_worlds = caller.valid_worlds
        if CC.last(valid_worlds) >= get_world_counter()
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            CC.store_backedges(caller, edges)
        end
        CC.finish!(analyzer, frame)

        reports = get_reports(analyzer, caller)

        # XXX this is a dirty fix for performance problem, we need more "proper" fix
        # https://github.com/aviatesk/JET.jl/issues/75
        unique!(aggregation_policy(analyzer), reports)

        # global cache management
        # part 1: transform collected reports to `JETCachedResult` and put it into `CodeInstance.inferred`
        if cached && !istoplevel(frame)
            CC.cache_result!(analyzer, caller)
        end
        # part 2: register invalidation callback for JET cache
        add_jet_callback!(caller.linfo)

        if frame.parent !== nothing
            # inter-procedural handling: get back to the caller what we got from these results
            add_caller_cache!(analyzer, reports)

            # local cache management
            # TODO there are duplicated work here and `transform_result_for_cache`
            cache = InferenceErrorReport[]
            for report in reports
                cache_report!(cache, report)
            end
            set_cached_result!(analyzer, caller, cache)
        end
    end

    return true
end

# by default, this overload just is forwarded to the AbstractInterpreter's implementation
# but the only reason we have this overload is that some analyzers (like `JETAnalyzer`)
# can further overload this to generate `InferenceErrorReport` with an access to `frame`
function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
    return CC.finish!(analyzer, frame.result)
end
