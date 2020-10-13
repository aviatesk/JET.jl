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
    # TODO: move this condition check into src/JET.jl
    if is_self_profiling()
        # when self-profiling, we need to invalidate caches for "our" code
        if isa(mi.def, Method)
            mod = mi.def.module

            # ignore cache for code defined in JET
            mod == (@__MODULE__) && return default

            # caches for overloaded code also need to be invalidated
            mod == JuliaInterpreter && return default

            # ignoring entire cache for `Core.Compiler` slows down performance too bad, so
            # let's do more fine-grained, per-file basis check
            # XXX: needs more files to be added ?
            file = mi.def.file
            file === Symbol("compiler/cicache.jl") && return default
            file === Symbol("compiler/abstractinterpretation.jl") && return default
            file === Symbol("compiler/typeinfer.jl") && return default
        end
    end

    ret = CC.get(tpc.native, mi, default)

    isa(ret, CodeInstance) || return default

    # cache hit, now we need to invalidate the cache lookup if this `mi` has been profiled
    # as erroneous by JET or is seemingly erroneous (checked by its return type annotation);
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
