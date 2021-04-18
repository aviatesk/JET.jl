# global cache
# ============

# XXX `JET_REPORT_CACHE` isn't synced with `JET_CODE_CACHE`, and this may lead to a problem ?

"""
    JET_REPORT_CACHE::$(typeof(JET_REPORT_CACHE))

Keeps JET report cache for a `MethodInstance`.
Reports are cached when `JETInterpreter` exits from `_typeinf`.
"""
const JET_REPORT_CACHE = IdDict{MethodInstance,Vector{InferenceErrorReportCache}}()

"""
    JET_CODE_CACHE::$(typeof(JET_CODE_CACHE))

Keeps `CodeInstance` cache associated with `mi::MethodInstace` that represent the result of
  an inference on `mi` performed by `JETInterpreter`.
This cache is completely separated from the `NativeInterpreter`'s global cache, so that
  JET analysis never interacts with actual code execution.
"""
const JET_CODE_CACHE = IdDict{MethodInstance,CodeInstance}()

function CC.code_cache(interp::JETInterpreter)
    cache  = JETGlobalCache(interp)
    worlds = WorldRange(get_world_counter(interp))
    return WorldView(cache, worlds)
end

struct JETGlobalCache
    interp::JETInterpreter
end

CC.haskey(wvc::WorldView{JETGlobalCache}, mi::MethodInstance) = haskey(JET_CODE_CACHE, mi)

function CC.get(wvc::WorldView{JETGlobalCache}, mi::MethodInstance, default)
    # ignore code cache for a `MethodInstance` that is not yet analyzed by JET;
    ret = get(JET_CODE_CACHE, mi, default)
    if isa(ret, CodeInstance)
        # cache hit, now we need to append cached reports associated with this `MethodInstance`
        global_cache = get(JET_REPORT_CACHE, mi, nothing)
        if isa(global_cache, Vector{InferenceErrorReportCache})
            interp = wvc.cache.interp
            for cached in global_cache
                restored = restore_cached_report!(cached, interp)
                push!(interp.to_be_updated, restored) # should be updated in `abstract_call` (after exiting `typeinf_edge`)
                # # TODO make this hold
                # @assert first(cached.st).linfo === mi "invalid global restoring"
            end
        end
    end
    return ret
end

function CC.getindex(wvc::WorldView{JETGlobalCache}, mi::MethodInstance)
    r = CC.get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

function CC.setindex!(wvc::WorldView{JETGlobalCache}, ci::CodeInstance, mi::MethodInstance)
    setindex!(JET_CODE_CACHE, ci, mi)
    add_jet_callback!(mi) # register the callback on invalidation
    return nothing
end

function add_jet_callback!(linfo)
    @static BACKEDGE_CALLBACK_ENABLED || return nothing

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
    delete!(JET_REPORT_CACHE, replaced)
    delete!(JET_CODE_CACHE, replaced)

    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            mi = mi::MethodInstance
            (haskey(JET_REPORT_CACHE, mi) || haskey(JET_CODE_CACHE, mi)) || continue # otherwise infinite loop)
            invalidate_jet_cache!(mi, max_world, depth+1)
        end
    end
    return nothing
end

# local cache
# ===========

struct JETLocalCache
    interp::JETInterpreter
    cache::Vector{InferenceResult}
end

CC.get_inference_cache(interp::JETInterpreter) = JETLocalCache(interp, get_inference_cache(interp.native))

function CC.cache_lookup(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::JETLocalCache)
    inf_result = cache_lookup(linfo, given_argtypes, cache.cache)

    isa(inf_result, InferenceResult) || return nothing

    # cache hit, try to restore local report caches
    interp = cache.interp
    sv = interp.current_frame::InferenceState

    # constant prop' hits a cycle (recur into same non-constant analysis), we just bail out
    isa(inf_result.result, InferenceState) && return inf_result

    analysis_result = jet_cache_lookup(linfo, given_argtypes, interp.cache)

    # corresponds to report throw away logic in `_typeinf(interp::JETInterpreter, frame::InferenceState)`
    filter!(!is_from_same_frame(sv.linfo, linfo), interp.reports)

    if isa(analysis_result, AnalysisResult)
        for cached in analysis_result.cache
            restored = restore_cached_report!(cached, interp)
            push!(interp.to_be_updated, restored) # should be updated in `abstract_call_method_with_const_args`
            # # TODO make this hold
            # @assert first(cached.st).linfo === linfo "invalid local restoring"
        end
    end

    return inf_result
end

# should sync with the implementation of `cache_lookup(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::Vector{InferenceResult})`
function jet_cache_lookup(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::Vector{AnalysisResult})
    method = linfo.def::Method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(given_argtypes) >= nargs || return nothing
    for cached_result in cache
        cached_result.linfo === linfo || continue
        cache_match = true
        cache_argtypes = cached_result.argtypes
        cache_overridden_by_const = cached_result.overridden_by_const
        for i in 1:nargs
            if !is_argtype_match(given_argtypes[i],
                                 cache_argtypes[i],
                                 CC.getindex(cache_overridden_by_const, i))
                cache_match = false
                break
            end
        end
        if method.isva && cache_match
            cache_match = is_argtype_match(tuple_tfunc(given_argtypes[(nargs + 1):end]),
                                           cache_argtypes[end],
                                           CC.getindex(cache_overridden_by_const, CC.length(cache_overridden_by_const)))
        end
        cache_match || continue
        return cached_result
    end
    return nothing
end

CC.push!(cache::JETLocalCache, inf_result::InferenceResult) = CC.push!(cache.cache, inf_result)
