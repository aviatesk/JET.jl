# just relies on the native tfuncs, maybe there're lots of edge cases
function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    ret = invoke_native(builtin_tfunction, interp, f, argtypes, sv)

    # propagate virtual global variable
    if f === getfield && ret == Any && length(argtypes) == 2 && all(a->isa(a, Const), argtypes)
        mod, sym = map(a->a.val, argtypes)

        if mod isa Module && sym isa Symbol
            ret = getvirtualglobalvar(interp, mod, sym)
        end
    end

    if f === throw
        # TODO: needs a special case here
    elseif ret === Bottom
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(interp, sv))
    end

    return ret
end
