get_cur_stmt(frame::InferenceState) = frame.src.code[frame.currpc]

# NOTE:
# below is adapted from https://github.com/JuliaLang/julia/blob/1e6e65691254a7fe81f5da8706bb30aa6cb3f8d2/base/compiler/abstractinterpretation.jl
# and ideally the patching here is better to be upstreamed as much as possible

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

function abstract_eval_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = if isa(e, Expr)
        abstract_eval_value_expr(interp, e, vtypes, sv)
    else
        abstract_eval_special_value(interp, e, vtypes, sv)
    end

    # boolean-context check
    stmt = get_cur_stmt(sv)
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        ⊑(Bool, t) || add_remark!(interp, sv, "non-boolean ($(t)) used in boolean context")
    end

    return ret
end

function abstract_eval_global(interp::TPInterpreter, M::Module, s::Symbol, sv::InferenceState)
    # global ref check
    return if !isdefined(M,s)
        add_remark!(interp, sv, "$(s) not defined in module $(string(M))")
        Bottom
    else
        isconst(M,s) ? AbstractEvalConstant(getfield(M,s)) : Any
    end
end
