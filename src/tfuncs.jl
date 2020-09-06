# just relies on the native tfuncs, maybe there're lots of edge cases
function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    ret = invoke_native(builtin_tfunction, interp, f, argtypes, sv)

    # propagate virtual global variable
    if f === getfield && ret == Any && length(argtypes) == 2 && all(a->isa(a, Const), argtypes)
        mod, sym = map(a->a.val, argtypes)

        if mod isa Module && sym isa Symbol
            ret = get_virtual_globalvar(interp, mod, sym)
        end
    end

    if f === throw
        # NOTE: handled in `finish(me::InferenceState, interp::TPInterpreter)`
        return ret
    elseif ret === Bottom
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(interp, sv))
    end

    return ret
end

# `return_type_tfunc` internally uses `abstract_call` to model `return_type` function; while
# TP will reports it as an error if the `abstract_call` is invalid (e.g. no method matched),
# `return_type` just returns `Union{}` (and thus `return_type_tfunc` returns `Const(Union{})`
# in that case, so we need to special case this so that we just pass `NativeInterpreter` to
# `abstract_call` and just do simple error check for argument numbers
function return_type_tfunc(interp::TPInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    if length(argtypes) !== 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        add_remark!(interp, sv, NoMethodErrorReport(interp, sv, false))
        return Bottom
    else
        # don't recursively pass on `TPInterpreter` via `invoke_native`
        return return_type_tfunc(interp.native, argtypes, sv)
    end
end
