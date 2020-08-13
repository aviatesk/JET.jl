# HACK:
# calls down to `NativeInterpreter`'s abstract interpretation method while passing `TPInterpreter`
# so that its overloaded methods can be called within the sub/recursive method callls.
function invoke_native(f, interp::TPInterpreter, args...; kwargs...)
    argtypes = to_tuple_type((AbstractInterpreter, typeof.(args)...))
    return invoke(f, argtypes, interp, args...; kwargs...)
end

get_cur_stmt(frame::InferenceState) = frame.src.code[frame.currpc]

# report undef var error
function check_global_ref!(interp::TPInterpreter, sv::InferenceState, m::Module, s::Symbol)
    return if !isdefined(m, s)
        add_remark!(interp, sv, UndefVarErrorReport(sv, m, s))
        true
    else
        false
    end
end

# NOTE:
# below is adapted from https://github.com/JuliaLang/julia/blob/1e6e65691254a7fe81f5da8706bb30aa6cb3f8d2/base/compiler/abstractinterpretation.jl
# and ideally the patching here is better to be upstreamed as much as possible

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.
function abstract_call_gf_by_type(interp::TPInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = invoke_native(abstract_call_gf_by_type, interp, f, argtypes, atype, sv, max_methods)::CallMeta

    # report no method error, notes:
    info = ret.info
    if isa(info, UnionSplitInfo)
        # if `info` is `UnionSplitInfo`, but there won't be a case where `info.matches` is empty
        for info in info.matches
            if isa(info.results, MethodLookupResult) && isempty(info.results.matches)
                # no method match for this union split
                # ret.rt = Bottom # maybe we want to be more strict on error cases ? but such a check will be really against the nature of dynamic typing
                add_remark!(interp, sv, NoMethodErrorReport(sv, atype, true))
            end
        end
    elseif isa(info, MethodMatchInfo) && isa(info.results, MethodLookupResult) && isempty(info.results.matches)
        # really no method found
        typeassert(ret.rt, TypeofBottom) # return type is initialized as `Bottom`, and should never change in these passes
        add_remark!(interp, sv, NoMethodErrorReport(sv, atype, false))
    end

    return ret
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_special_value, interp, e, vtypes, sv)

    # report undef var error
    if isa(e, Slot)
        id = slot_id(e)
        if vtypes[id].undef
            s = sv.src.slotnames[id]
            add_remark!(interp, sv, UndefVarErrorReport(sv, Main, s))
        end
    elseif isa(e, GlobalRef)
        check_global_ref!(interp, sv, e.mod, e.name) && (ret = Bottom) # ret here should annotated as `Any` by `NativeInterpreter`, but here I would like to be more conservative and change it to `Bottom`
    end

    return ret
end

function abstract_eval_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_value, interp, e, vtypes, sv)

    # report non-boolean condition error
    stmt = get_cur_stmt(sv)
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if !⊑(Bool, Bottom) && !⊑(Bool, t)
            add_remark!(interp, sv, NonBooleanCondErrorReport(sv, t))
            ret = Bottom
        end
    end

    return ret
end

function abstract_eval_statement(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_statement, interp, e, vtypes, sv)

    # report undef var error
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
