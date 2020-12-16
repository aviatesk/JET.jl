# https://github.com/JuliaLang/julia/blob/341d6c905037b9b5b1bde7877274ce11072ea909/base/compiler/typeinfer.jl#L722-L771

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    code = get(code_cache(interp), mi, nothing)
    if code isa CodeInstance # return existing rettype if the code is already inferred
        update_valid_age!(caller, WorldRange(min_world(code), max_world(code)))
        if isdefined(code, :rettype_const)
            if isa(code.rettype_const, Vector{Any}) && !(Vector{Any} <: code.rettype)
                return PartialStruct(code.rettype, code.rettype_const), mi
            else
                return Const(code.rettype_const), mi
            end
        else
            return code.rettype, mi
        end
    end
    if ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0
        return Any, nothing
    end
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame === false
        # completely new
        lock_mi_inference(interp, mi)
        result = InferenceResult(mi)
        frame = InferenceState(result, #=cached=#true, interp) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            unlock_mi_inference(interp, mi)
            return Any, nothing
        end
        if caller.cached || caller.limited # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(interp, frame)
        update_valid_age!(frame, caller)
        return frame.bestguess, frame.inferred ? mi : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(frame, caller)
    return frame.bestguess, nothing
end
