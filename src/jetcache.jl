# global cache
# ============

const ANALYZED_LINFOS  = IdSet{MethodInstance}() # keeps `MethodInstance`s analyzed by JET

const JET_GLOBAL_CACHE = IdDict{MethodInstance,Vector{InferenceErrorReportCache}}()

function CC.code_cache(interp::JETInterpreter)
    cache  = JETCache(interp, code_cache(interp.native))
    worlds = WorldRange(get_world_counter(interp))
    return WorldView(cache, worlds)
end

struct JETCache
    interp::JETInterpreter
    native::WorldView{InternalCodeCache}
end

function CC.haskey(wvc::WorldView{JETCache}, mi::MethodInstance)
    ret = CC.haskey(WorldView(wvc.cache.native, wvc.worlds), mi)
    if ret && !(mi in ANALYZED_LINFOS)
        add_jet_callback!(mi) # XXX: forcibly register a callback for caches in system image
    end
    return ret
end

function CC.get(wvc::WorldView{JETCache}, mi::MethodInstance, default)
    # ignore code cache for a `MethodInstance` that is not yet analyzed by JET;
    # this happens when the `MethodInstance` is cached within the system image
    mi in ANALYZED_LINFOS || return default # force inference

    ret = CC.get(WorldView(wvc.cache.native, wvc.worlds), mi, default)
    if isa(ret, CodeInstance)
        # cache hit, now we need to append cached reports associated with this `MethodInstance`
        global_cache = get(JET_GLOBAL_CACHE, mi, nothing)
        if isa(global_cache, Vector{InferenceErrorReportCache})
            interp = wvc.cache.interp
            caller = interp.current_frame::InferenceState
            for cached in global_cache
                restore_cached_report!(cached, interp, caller)
            end
        end
    end
    return ret
end

function CC.getindex(wvc::WorldView{JETCache}, mi::MethodInstance)
    r = CC.get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

function CC.setindex!(wvc::WorldView{JETCache}, ci::CodeInstance, mi::MethodInstance)
    CC.setindex!(WorldView(wvc.cache.native, wvc.worlds), ci, mi)
    add_jet_callback!(mi)
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
    delete!(ANALYZED_LINFOS, replaced)
    delete!(JET_GLOBAL_CACHE, replaced)

    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            mi = mi::MethodInstance
            (mi in ANALYZED_LINFOS || haskey(JET_GLOBAL_CACHE, mi)) || continue # otherwise infinite loop
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

    # cache hit, restore local report caches
    interp = cache.interp
    sv = interp.current_frame::InferenceState
    if !isa(inf_result.result, InferenceState)
        # corresponds to report throw away logic in `_typeinf(interp::JETInterpreter, frame::InferenceState)`
        filter!(r->!is_lineage(r.lineage, sv, inf_result.linfo), interp.reports)

        local_cache = get(interp.cache, given_argtypes, nothing)
        if isa(local_cache, Vector{InferenceErrorReportCache})
            for cached in local_cache
                restore_cached_report!(cached, interp, sv)
            end
        end
    end

    return inf_result
end

CC.push!(cache::JETLocalCache, inf_result::InferenceResult) = CC.push!(cache.cache, inf_result)
