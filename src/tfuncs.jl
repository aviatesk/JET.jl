function builtin_tfunction(interp::TPInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                           sv::InferenceState) # `TPInterpreter` isn't overloaded on `return_type`
    ret = @invoke builtin_tfunction(interp::AbstractInterpreter, f, argtypes::Array{Any,1},
                                    sv::Union{InferenceState,Nothing})

    if f === throw
        # uncaught `throw` calls will be reported by `typeinf(interp::TPInterpreter, frame::InferenceState)`
        return ret
    elseif isa(ret, VirtualGlobalVariable)
        # propagate virtual global variable

        add_backedge!(ret.li, sv)
        # this might be `Bottom`, but hopefully the error on this variable is already reported,
        # so we don't check `InvalidBuiltinCallErrorReport` for this pass
        return ret.t
    elseif ret === Bottom
        # XXX: for now, TP just relies on the native tfuncs to report invalid builtin calls,
        # maybe there're lots of false negative/positives
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
