# adapted from https://github.com/JuliaLang/julia/blob/1e6e65691254a7fe81f5da8706bb30aa6cb3f8d2/base/compiler/abstractinterpretation.jl

# global ref check
# ----------------

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if isa(e, QuoteNode)
        return CC.AbstractEvalConstant((e::QuoteNode).value)
    elseif isa(e, SSAValue)
        return abstract_eval_ssavalue(e::SSAValue, sv.src)
    elseif isa(e, Slot)
        return vtypes[slot_id(e)].typ
    elseif isa(e, GlobalRef)
        return abstract_eval_global(interp, e.mod, e.name, sv)
    end

    return AbstractEvalConstant(e)
end

function abstract_eval_global(interp::TPInterpreter, M::Module, s::Symbol, sv::InferenceState)
    if !isdefined(M,s)
        add_remark!(interp, sv, "$(s) not defined in module $(string(M))")
        return Bottom
    else
        if isconst(M,s)
            return AbstractEvalConstant(getfield(M,s))
        end
        return Any
    end
end
