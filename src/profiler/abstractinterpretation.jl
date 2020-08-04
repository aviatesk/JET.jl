# HACK:
# call down to `NativeInterpreter`'s abstract call method while passing `TPInterpreter`
function invoke_native(f, interp::TPInterpreter, args...; kwargs...)
    argtypes = to_tuple_type((AbstractInterpreter, typeof.(args)...))
    return invoke(f, argtypes, interp, args...; kwargs...)
end

get_cur_stmt(frame::InferenceState) = frame.src.code[frame.currpc]

function check_global_ref!(interp::TPInterpreter, sv::InferenceState, m::Module, s::Symbol)
    return if !isdefined(m, s)
        add_remark!(interp, sv, "$(s) not defined in module $(string(m))")
        true
    else
        false
    end
end

# NOTE:
# below is adapted from https://github.com/JuliaLang/julia/blob/1e6e65691254a7fe81f5da8706bb30aa6cb3f8d2/base/compiler/abstractinterpretation.jl
# and ideally the patching here is better to be upstreamed as much as possible

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_special_value, interp, e, vtypes, sv)

    # global ref check
    if isa(e, GlobalRef)
        check_global_ref!(interp, sv, e.mod, e.name) && (ret = Bottom) # ret here is annotated by `Any` by `NativeInterpreter`, but here I would like to change it to `Bottom`
    end

    return ret
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
        if !⊑(Bool, Bottom) && !⊑(Bool, t)
            add_remark!(interp, sv, "non-boolean ($(t)) used in boolean context")
            ret = Bottom
        end
    end

    return ret
end

function abstract_eval_statement(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_statement, interp, e, vtypes, sv)

    # global ref check
    if isa(e, Expr)
        if e.head === :isdefined
            sym = e.args[1]
            if isa(sym, Symbol)
                check_global_ref!(interp, sv, sv.mod, sym) && (ret = Bottom)
            elseif isa(sym, GlobalRef)
                check_global_ref!(interp, sv, sym.mod, sym.name) && (ret = Bottom)
            end
        end
    end

    return ret
end
