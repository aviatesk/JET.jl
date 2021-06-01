# TODO tfunc implementations in Core.Compiler are really not enough to catch invalid calls
# set up our own checks and enable sound analysis

function CC.builtin_tfunction(interp::JETInterpreter, @nospecialize(f), argtypes::Array{Any,1},
                              sv::InferenceState) # `JETInterpreter` isn't overloaded on `return_type`
    ret = @invoke builtin_tfunction(interp::AbstractInterpreter, f, argtypes::Array{Any,1},
                                    sv::Union{InferenceState,Nothing})

    if f === throw
        # here we only report a selection of "serious" exceptions, i.e. those that should be
        # reported even if they may be caught in actual execution;
        report_pass!(SeriousExceptionReport, interp, sv, argtypes)

        # other general `throw` calls will be reported within `_typeinf(interp::JETInterpreter, frame::InferenceState)`
        # only when they are not caught by control flow, which is judged by whether if the
        # final return type of `sv` is annotated as `Bottom` or not, thus early return now
        return ret
    end

    # report pass for invalid builtin function calls
    # XXX for the meanwhile, we rely on the implementation of native tfuncs thus pass `ret` here as well
    # XXX dynamic dispatch, is this okay in terms of performance ?
    report_pass!(InvalidBuiltinCallErrorReport, interp, sv, f, argtypes, ret)

    if f === getfield && 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const)
                    mod = obj.val
                    if isa(mod, Module)
                        if isdefined(mod, name)
                            if istoplevel_globalref(interp, sv)
                                # when accessing to a global variable in a module
                                # concretized by `interp`, eagerly propagate its type
                                # NOTE logic here should be synced with that of `abstract_eval_special_value(::JETInterpreter, ::Any, ::VarTable, ::InferenceState)`
                                val = getfield(mod, name)
                                return isa(val, AbstractGlobal) ? val.t : Const(val)
                            end
                        end
                    end
                end
            end
        end
    elseif f === fieldtype
        # the valid widest possible return type of `fieldtype_tfunc` is `Union{Type,TypeVar}`
        # because fields of unwrapped `DataType`s can legally be `TypeVar`s,
        # but this will cause lots of false positive `NoMethodErrorReport`s for inference
        # with accessing to abstract fields since most methods don't expect `TypeVar`
        # (e.g. `@report_call readuntil(stdin, 'c')`)
        # JET.jl further widens this case to `Any` and give up further analysis rather than
        # trying hard to do sound and noisy analysis
        # xref: https://github.com/JuliaLang/julia/pull/38148
        if ret === Union{Type, TypeVar}
            return Any
        end
    end

    return ret
end

# check if this frame is for `getproperty(::Module, ::Symbol)`, which accesses to a global
# variable traced by `interp`
function istoplevel_globalref(interp::JETInterpreter, sv::InferenceState)
    def = sv.linfo.def
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = sv.parent
    return !isnothing(parent) && istoplevel(interp, parent)
end

# `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
# and here we shouldn't pass `JETInterpreter` to it; otherwise we may get false error reports
# from the `abstract_call`, which will simulate the call and isn't any abstraction of actual
# execution of it
function CC.return_type_tfunc(interp::JETInterpreter, argtypes::Vector{Any}, sv::InferenceState)
    # report pass for invalid `Core.Compiler.return_type` call
    report_pass!(InvalidReturnTypeCall, interp, sv, argtypes)

    # don't recursively pass on `JETInterpreter` via `@invoke` here, and make sure
    # JET's analysis enter into the simulated call
    return return_type_tfunc(interp.native, argtypes, sv)
end
