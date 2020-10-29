# code cache interface
# --------------------

CC.code_cache(interp::JETInterpreter) = JETCache(interp, code_cache(interp.native))

struct JETCache{NativeCache}
    interp::JETInterpreter
    native::NativeCache
    JETCache(interp::JETInterpreter, native::NativeCache) where {NativeCache} =
        new{NativeCache}(interp, native)
end
CC.WorldView(tpc::JETCache, wr::WorldRange) = JETCache(tpc.interp, WorldView(tpc.native, wr))
CC.WorldView(tpc::JETCache, args...) = WorldView(tpc, WorldRange(args...))

CC.haskey(tpc::JETCache, mi::MethodInstance) = CC.haskey(tpc.native, mi)

const ANALYZED_LINFOS   = IdSet{MethodInstance}()         # keeps `MethodInstances` analyzed by JET
const ERRORNEOUS_LINFOS = IdDict{MethodInstance,Symbol}() # keeps `MethodInstances` analyzed as "errorneous" by JET

function CC.get(tpc::JETCache, mi::MethodInstance, default)
    # if we haven't analyzed this linfo, just invalidate native code cache and force analysis by JET
    if mi ∉ ANALYZED_LINFOS
        push!(ANALYZED_LINFOS, mi)
        return default
    end

    ret = CC.get(tpc.native, mi, default)

    # cache isn't really found
    if ret === default
        return ret
    end

    # cache hit, now we need to invalidate the cache lookup if this `mi` has been profiled
    # as erroneous by JET analysis; otherwise the error reports that can occur from this
    # frame will just be ignored
    force_inference = false

    if haskey(ERRORNEOUS_LINFOS, mi)
        # don't force re-inference for frames from the same inference process;
        # FIXME: this is critical for profiling performance, but seems to lead to lots of false positives ...
        if ERRORNEOUS_LINFOS[mi] !== get_id(tpc.interp)
            force_inference = true
        end
    end

    return force_inference ? default : ret
end

CC.getindex(tpc::JETCache, mi::MethodInstance) = CC.getindex(tpc.native, mi)

CC.setindex!(tpc::JETCache, ci::CodeInstance, mi::MethodInstance) = CC.setindex!(tpc.native, ci, mi)
