# just relies on the native tfuncs, maybe there're lots of edge cases
function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    ret = invoke_native(builtin_tfunction, interp, f, argtypes, sv)

    if f === throw
        # TODO: needs a special case here
    elseif ret === Bottom
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(interp, sv))
    end

    return ret
end
