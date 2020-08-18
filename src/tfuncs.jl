# just relies on the native tfuncs, maybe there're lots of edge cases
function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    ret = invoke_native(builtin_tfunction, interp, f, argtypes, sv)

    if ret === Bottom
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(sv))
    end

    return ret
end
