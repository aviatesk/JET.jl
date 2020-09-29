# TODO: maybe we need to add `argtypes_to_type(argtypes)` as key for throw aways by constant propagation

# just relies on the native tfuncs, maybe there're lots of edge cases
function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::Union{InferenceState,Nothing})
    ret = @invoke builtin_tfunction(interp::AbstractInterpreter, f, argtypes::Array{Any,1},
                                    sv::Union{InferenceState,Nothing})

    # propagate virtual global variable
    if isa(ret, VirtualGlobalVariable)
        add_backedge!(val.li, sv)

        ret = val.t
    end

    isnothing(sv) && return ret
    sv = sv::InferenceState

    if f === throw
        # NOTE: handled in `finish(me::InferenceState, interp::TPInterpreter)`
        return ret
    elseif ret === Bottom
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(interp, sv, argtypes))
    end

    return ret
end

# `return_type_tfunc` internally uses `abstract_call` to model `return_type` function and
# here we shouldn't pass `TPInterpreter` to it; otherwise we may get false error reports from
# the  `abstract_call`, which isn't the abstraction of actual execution, thus here we just
# check if the call of `return_type` is valid or not
function return_type_tfunc(interp::TPInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    if length(argtypes) !== 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        add_remark!(interp, sv, NoMethodErrorReport(interp,
                                                    sv,
                                                    false,
                                                    # this is not necessary to be computed correctly, though
                                                    argtypes_to_type(argtypes),
                                                    ))
        return Bottom
    else
        # don't recursively pass on `TPInterpreter` via `@invoke_native`
        return return_type_tfunc(interp.native, argtypes, sv)
    end
end
