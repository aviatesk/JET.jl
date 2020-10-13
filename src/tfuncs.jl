# TODO: set up our own tfuncs

function CC.builtin_tfunction(interp::JETInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                              sv::InferenceState) # `JETInterpreter` isn't overloaded on `return_type`
    ret = @invoke builtin_tfunction(interp::AbstractInterpreter, f, argtypes::Array{Any,1},
                                    sv::Union{InferenceState,Nothing})

    if f === throw
        # uncaught `throw` calls will be reported by `typeinf(interp::JETInterpreter, frame::InferenceState)`
        return ret
    elseif f === getfield
        # getfield is so common, let's special case it
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const) && ⊑(obj, Module)
                    mod = obj.val
                    if !isdefined(mod, name)
                        # `ret` here should be annotated as `Any` by `getfield_tfunc`, but
                        # we want to be more conservative and change it to `Bottom` and
                        # suppress any further abstract interpretation with this
                        add_remark!(interp, sv, GlobalUndefVarErrorReport(interp, sv, mod, name))
                        return Bottom
                    end
                elseif ret === Bottom
                    # general case when an error is detected by the native `getfield_tfunc`
                    typ = widenconst(obj)
                    add_remark!(interp, sv, NoFieldErrorReport(interp, sv, typ, name))
                    return ret
                end
            end
        end
    end

    if isa(ret, VirtualGlobalVariable)
        # propagate virtual global variable

        add_backedge!(ret.li, sv)
        # this might be `Bottom`, but hopefully the error on this variable is already reported,
        # so we don't check `InvalidBuiltinCallErrorReport` for this pass
        return ret.t
    elseif ret === Bottom
        # XXX: for general case, JET just relies on the (maybe too persmissive) return type
        # from native tfuncs to report invalid builtin calls and probably there're lots of
        # false negatives
        add_remark!(interp, sv, InvalidBuiltinCallErrorReport(interp, sv, argtypes))
    end

    return ret
end

# `return_type_tfunc` internally uses `abstract_call` to model `return_type` function and
# here we shouldn't pass `JETInterpreter` to it; otherwise we may get false error reports from
# the  `abstract_call`, which isn't the abstraction of actual execution, thus here we just
# check if the call of `return_type` is valid or not
function CC.return_type_tfunc(interp::JETInterpreter, argtypes::Vector{Any}, sv::InferenceState)
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
        # don't recursively pass on `JETInterpreter` via `@invoke_native`
        return return_type_tfunc(interp.native, argtypes, sv)
    end
end
