# global cache
# ============

"""
    JET_CACHE::$(typeof(JET_CACHE))

Keeps `src::CodeInstance` cache associated with `mi::MethodInstace` that represents the
analysis result on `mi` performed by [`analyzer::AbstractAnalyzer`](@ref AbstractAnalyzer),
where [`src.inferred::JETCachedResult`](@ref JETCachedResult) caches JET's analysis result.
This cache is separated by the identities of `AbstractAnalyzer`, which are hash keys
computed by `get_cache_key(analyzer::AbstractAnalyzer)`

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

struct JETGlobalCache
    analyzer::AbstractAnalyzer
end

# cache existence for this `analyzer` is ensured on its construction
jet_cache(analyzer::AbstractAnalyzer)     = JET_CACHE[get_cache_key(analyzer)]
jet_cache(wvc::WorldView{JETGlobalCache}) = jet_cache(wvc.cache.analyzer)

CC.haskey(wvc::WorldView{JETGlobalCache}, mi::MethodInstance) = haskey(jet_cache(wvc), mi)

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    # NOTE enable the report cache restoration at `code = get(code_cache(interp), mi, nothing)`
    set_cacher!(analyzer, :typeinf_edge => caller.result)
    return @invoke typeinf_edge(analyzer::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
end

function CC.get(wvc::WorldView{JETGlobalCache}, mi::MethodInstance, default)
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
                    @static JET_DEV_MODE && if isa(analyzer, JETAnalyzer)
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

function CC.getindex(wvc::WorldView{JETGlobalCache}, mi::MethodInstance)
    r = CC.get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

function CC.transform_result_for_cache(interp::AbstractAnalyzer, linfo::MethodInstance,
                                       valid_worlds::WorldRange, @nospecialize(inferred_result))
    jetresult = inferred_result::JETResult
    cache = InferenceErrorReportCache[]
    for report in get_reports(jetresult)
        @static JET_DEV_MODE && if isa(interp, JETAnalyzer)
            actual, expected = first(report.vst).linfo, linfo
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cache, report)
    end
    return JETCachedResult(cache, get_source(jetresult))
end

function CC.setindex!(wvc::WorldView{JETGlobalCache}, ci::CodeInstance, mi::MethodInstance)
    setindex!(jet_cache(wvc), ci, mi)
    add_jet_callback!(mi) # register the callback on invalidation
    return nothing
end

function add_jet_callback!(linfo)
    if !isdefined(linfo, :callbacks)
        linfo.callbacks = Any[invalidate_jet_cache!]
    else
        if !any(@nospecialize(cb)->cb===invalidate_jet_cache!, linfo.callbacks)
            push!(linfo.callbacks, invalidate_jet_cache!)
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

# local cache
# ===========

struct JETLocalCache
    analyzer::AbstractAnalyzer
    cache::Vector{InferenceResult}
end

CC.get_inference_cache(analyzer::AbstractAnalyzer) = JETLocalCache(analyzer, get_inference_cache(get_native(analyzer)))

function CC.cache_lookup(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::JETLocalCache)
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
        @static JET_DEV_MODE && if isa(analyzer, JETAnalyzer)
            actual, expected = first(restored.vst).linfo, linfo
            @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
        end
        add_caller_cache!(analyzer, restored) # should be updated in `abstract_call_method_with_const_args`
    end

    return inf_result
end

CC.push!(cache::JETLocalCache, inf_result::InferenceResult) = CC.push!(cache.cache, inf_result)
