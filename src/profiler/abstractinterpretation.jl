# TODO: pass more complex data structures into `add_remark!`, and leave string convertion for error printing part

# HACK: call down to `NativeInterpreter`'s abstract call method while passing `TPInterpreter`
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

# returns a call signature string from tt
function tt_to_signature_str(@nospecialize(tt::Type{<:Tuple}))
    fn = ft_to_fname(tt.parameters[1])
    args = join("::" .* string.(tt.parameters[2:end]), ", ")
    return string(fn, '(', args, ')')
end

# returns function name from its type
function ft_to_fname(@nospecialize(ft))
    return if isconstType(ft)
        repr(ft.parameters[1])
    elseif ft isa DataType && isdefined(ft, :instance)
        repr(ft.instance)
    else
        repr(ft)
    end
end

# NOTE:
# below is adapted from https://github.com/JuliaLang/julia/blob/1e6e65691254a7fe81f5da8706bb30aa6cb3f8d2/base/compiler/abstractinterpretation.jl
# and ideally the patching here is better to be upstreamed as much as possible

function abstract_call_gf_by_type(interp::TPInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = invoke_native(abstract_call_gf_by_type, interp, f, argtypes, atype, sv, max_methods)::CallMeta
    info = ret.info

    if isa(info, MethodMatchInfo) && isa(info.results, MethodLookupResult) && isempty(info.results.matches)
        # so no method found
        typeassert(ret.rt, TypeofBottom) # here just for asserting compatibility with native interpreter changes
        add_remark!(interp, sv, string("no matching method found for signature: ", tt_to_signature_str(atype)))
    elseif isa(info, UnionSplitInfo)
        # TODO
    end

    return ret
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_special_value, interp, e, vtypes, sv)

    # global ref check
    if isa(e, GlobalRef)
        check_global_ref!(interp, sv, e.mod, e.name) && (ret = Bottom) # ret here is annotated by `Any` by `NativeInterpreter`, but here I would like to change it to `Bottom`
    end

    return ret
end

function abstract_eval_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_value, interp, e, vtypes, sv)

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
