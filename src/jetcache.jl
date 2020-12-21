# XXX: additional backedges for JET analysis
# currently invalidation of JET cache just relies on invalidation of native code cache, i.e.
# it happens only when there is a backedge from erroneous frame, which is not always true
# e.g. native interpreter doesn't add backedge when the return type is `Any`, but invalidation
# may still change the result of JET analysis
# so we may need to add additional backedges for frames from which some errors are reported

# we just overload `cache_result!` and so we don't need to set up our own cache
CC.code_cache(interp::JETInterpreter) = code_cache(interp.native)

# TODO better to use `overload_cache_result!!` hack ?
let

# branching on https://github.com/JuliaLang/julia/pull/38820
islatest = first(methods(CC.cache_result!, CC)).nargs == 3

Core.eval(@__MODULE__, Expr(
    :function,
    # signature
    islatest ? :(CC.cache_result!(interp::JETInterpreter, result::InferenceResult)) :
               :(CC.cache_result!(interp::JETInterpreter, result::InferenceResult, valid_worlds::WorldRange)),
    # body
    Expr(:block, LineNumberNode(@__LINE__, @__FILE__), quote
    linfo = result.linfo

    $(islatest && :(valid_worlds = result.valid_worlds))
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    already_inferred = CC.already_inferred_quick_test(interp, linfo) # always false
    if !already_inferred && CC.haskey(WorldView(code_cache(interp), valid_worlds), linfo)
        already_inferred = true
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        $(islatest ? :(inferred_result = CC.transform_result_for_cache(interp, linfo, valid_worlds, result.src)) :
                     :(inferred_result = CC.transform_result_for_cache(interp, linfo, result.src)))
        CC.setindex!(code_cache(interp), CodeInstance(result, inferred_result, valid_worlds), linfo)
    end
    unlock_mi_inference(interp, linfo)

    @static :backedges ∉ fieldnames(MethodInstance) && return nothing

    # this function is called from `_typeinf(interp::AbstractInterpreter, frame::InferenceState)`
    # and so at this point `linfo` is not registered in `ANALYZED_LINFOS`
    # (unless inference happens multiple times for this frame)
    if linfo ∉ ANALYZED_LINFOS
        add_jet_callback!(linfo)
    end

    return nothing
    end) # Expr(:block, LineNumberNode(@__LINE__, @__FILE__), quote
))

end

function add_jet_callback!(linfo)
    if !isdefined(linfo, :callbacks)
        linfo.callbacks = Any[invalidate_jet_cache!]
    else
        if !any(@nospecialize(cb)->cb!==invalidate_jet_cache!, linfo.callbacks)
            push!(linfo.callbacks, invalidate_jet_cache!)
        end
    end
end

function invalidate_jet_cache!(replaced, max_world, depth = 0)
    replaced ∉ ANALYZED_LINFOS && return
    delete!(ANALYZED_LINFOS, replaced)
    delete!(JET_GLOBAL_CACHE, replaced)

    if isdefined(replaced, :backedges)
        for mi in replaced.backedges
            invalidate_jet_cache!(mi, max_world, depth+1)
        end
    end
    return nothing
end
