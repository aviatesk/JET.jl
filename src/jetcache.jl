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

function CC.get(tpc::JETCache, mi::MethodInstance, default)
    # # for self profiling
    # if isa(mi.def, Method)
    #     # ignore cache for JET's code itself
    #     mod = mi.def.module
    #     if mod == (@__MODULE__) || mod == JuliaInterpreter
    #         return default
    #     end
    #
    #     file = mi.def.file
    #     # ignoring entire cache for `Core.Compiler` will slows down profiling performance too bad
    #     if file === Symbol("compiler/typeinfer.jl") || file === Symbol("compiler/abstractinterpretation.jl")
    #         return default
    #     end
    # end

    ret = CC.get(tpc.native, mi, default)

    isa(ret, CodeInstance) || return default

    # cache hit, now we need to invalidate the cache lookup if this `mi` has been profiled
    # as erroneous by JET or seemingly erroneous (checked by its return type annotation);
    # otherwise the error reports that can occur from this frame will just be ignored
    force_inference = false

    if haskey(ERRORNEOUS_LINFOS, mi)
        # don't force re-inference for frames from the same inference process;
        # FIXME: this is critical for profiling performance, but seems to lead to lots of false positives ...
        if ERRORNEOUS_LINFOS[mi] !== get_id(tpc.interp)
            force_inference = true
        end
    elseif isdefined(ret, :rettype) && ret.rettype === Bottom
        # return type is annotated as `Bottom` (by native compiler), let's force inference
        force_inference = true
    end

    return force_inference ? default : ret
end

CC.getindex(tpc::JETCache, mi::MethodInstance) = CC.getindex(tpc.native, mi)

CC.setindex!(tpc::JETCache, ci::CodeInstance, mi::MethodInstance) = CC.setindex!(tpc.native, ci, mi)
