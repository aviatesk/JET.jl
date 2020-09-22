# code cache interface
# --------------------

code_cache(interp::TPInterpreter) = TPCache(interp, code_cache(interp.native))

struct TPCache{NativeCache}
    interp::TPInterpreter
    native::NativeCache
    TPCache(interp::TPInterpreter, native::NativeCache) where {NativeCache} =
        new{NativeCache}(interp, native)
end
WorldView(tpc::TPCache, wr::WorldRange) = TPCache(tpc.interp, WorldView(tpc.native, wr))
WorldView(tpc::TPCache, args...) = WorldView(tpc, WorldRange(args...))

CC.haskey(tpc::TPCache, mi::MethodInstance) = CC.haskey(tpc.native, mi)

function CC.get(tpc::TPCache, mi::MethodInstance, default)
    ret = CC.get(tpc.native, mi, default)

    ret === default && return default

    # cache hit, now we need to invalidate the cache lookup if this `mi` has been profiled
    # as erroneous; otherwise the error reports that can occur from this frame will just be
    # ignored
    # FIXME: this can insanely slow down profiling performance, find a workaround
    force_inference = false
    if mi in ERRORNEOUS_LINFOS
        force_inference = true
    end

    return force_inference ? default : ret
end

CC.getindex(tpc::TPCache, mi::MethodInstance) = CC.getindex(tpc.native, mi)

CC.setindex!(tpc::TPCache, ci::CodeInstance, mi::MethodInstance) = CC.setindex!(tpc.native, ci, mi)
