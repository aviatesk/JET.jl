#=
overloads functions in https://github.com/JuliaLang/julia/blob/fb2e1efd8de5040119be005ec67c66b7e9838156/base/compiler/abstractinterpretation.jl
so that `TPInterpreter` collects possible error points detected during the inference
=#

# HACK:
# calls down to `NativeInterpreter`'s abstract interpretation method while passing `TPInterpreter`
# so that its overloaded methods can be called within the sub/recursive method callls.
function invoke_native(f, interp::TPInterpreter, args...; kwargs...)
    argtypes = to_tuple_type((AbstractInterpreter, typeof.(args)...))
    return invoke(f, argtypes, interp, args...; kwargs...)
end

# report undef var error
function check_global_ref!(interp::TPInterpreter, sv::InferenceState, m::Module, s::Symbol)
    return if !isdefined(m, s)
        add_remark!(interp, sv, UndefVarErrorReport(interp, sv, m, s))
        true
    else
        false
    end
end

# overloads
# ---------

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
                # ret.rt = Bottom # maybe we want to be more strict on error cases ?
                add_remark!(interp, sv, NoMethodErrorReport(interp, sv, true))
            end
        end
    elseif isa(info, MethodMatchInfo) && isa(info.results, MethodLookupResult) && isempty(info.results.matches)
        # really no method found
        typeassert(ret.rt, TypeofBottom) # return type is initialized as `Bottom`, and should never change in these passes
        add_remark!(interp, sv, NoMethodErrorReport(interp, sv, false))
    end

    return ret
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_special_value, interp, e, vtypes, sv)

    # report undef var error
    if isa(e, Slot)
        # id = slot_id(e)
        # s = sv.src.slotnames[id]
        # t = vtypes[id].typ
        # if t === NOT_FOUND || t === Bottom
        #     s = sv.src.slotnames[id]
        #     add_remark!(interp, sv, UndefVarErrorReport(interp, sv, sv.mod, s))
        # end
    elseif isa(e, GlobalRef)
        vgv = getvirtualglobalvar(interp, e.mod, e.name)
        if isnothing(vgv)
            check_global_ref!(interp, sv, e.mod, e.name) && (ret = Bottom) # ret here should annotated as `Any` by `NativeInterpreter`, but here I would like to be more conservative and change it to `Bottom`
        else
            ret = vgv
        end
    end

    return ret
end

function abstract_eval_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_value, interp, e, vtypes, sv)

    # report non-boolean condition error
    stmt = get_cur_stmt(sv)
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom && !⊑(Bool, t)
            add_remark!(interp, sv, NonBooleanCondErrorReport(interp, sv, t))
            ret = Bottom
        end
    end

    return ret
end

# # overload this to profile on e.g. `Expr(:new, ...)`
# function abstract_eval_statement(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
#     ret = invoke_native(abstract_eval_statement, interp, e, vtypes, sv)
#
#     return ret
# end

# works as the native (i.e. "make as much progress on `frame` as possible (without handling
# cycles)", but when `interp` is profiling on virtual toplevel lambda and `frame` is in
# virtual toplevel (i.e. when `isroot(frame) === true`), keep the traced types of  `SlotNumber`s
# (which are originally global variables) in `TPInterpreter.virtualglobalvartable` so that
# they can be referred across profilings on different virtual (toplevel) functions
#
# NOTE:
# virtual global assignments should happen here because `SlotNumber`s can be optimized away
# after the optimization happens
function typeinf_local(interp::TPInterpreter, frame::InferenceState)
    set_current_frame!(interp, frame)

    ret = invoke_native(typeinf_local, interp, frame)

    # assign virtual global variable for toplevel frames
    if istoplevel(interp) && isroot(frame)
        for (pc, stmt) in enumerate(frame.src.code)
            isexpr(stmt, :(=)) && setvirtualglobalvar!(interp, frame, pc, stmt)
        end
    end

    return ret
end

istoplevel(interp::TPInterpreter) = interp.istoplevel

function getvirtualglobalvar(interp, mod, sym)
    haskey(interp.virtualglobalvartable, mod) || return nothing
    return get(interp.virtualglobalvartable[mod], sym, nothing)
end

function setvirtualglobalvar!(interp, frame, pc, stmt)
    mod = frame.mod
    haskey(interp.virtualglobalvartable, mod) || (interp.virtualglobalvartable[mod] = Dict())

    slt = first(stmt.args)::Slot
    lhs = frame.src.slotnames[slt.id]::Symbol
    rhs = frame.src.ssavaluetypes[pc]
    if rhs === NOT_FOUND
        rhs = Bottom
    end

    interp.virtualglobalvartable[mod][lhs] = rhs
end

function typeinf_edge(interp::TPInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    set_current_frame!(interp, caller)

    ret = invoke_native(typeinf_edge, interp, method, atypes, sparams, caller)

    return ret
end

"""
    set_current_frame!(interp::TPInterpreter, frame::InferenceState)
    get_current_frame(interp::TPInterpreter)

The setter and getter of a frame that `interp` is currently profiling.
Current frame is needed when we assemble virtual stack frame from cached error reports.
"""
set_current_frame!(interp::TPInterpreter, frame::InferenceState) = interp.current_frame[] = frame
get_current_frame(interp::TPInterpreter) = interp.current_frame[]

# here we can work on `InferenceState` that inference already ran on it,
# and also maybe optimization has been done
function finish(me::InferenceState, interp::TPInterpreter)
    ret = invoke(finish, Tuple{InferenceState,AbstractInterpreter}, me, interp)

    if get_result(me) === Bottom
        if isroot(me)
            # if return type is `Bottom`-annotated for root frame, this means some error(s)
            # get propagated here, let's report `ExceptionReport` if exist
            for (i, (idx, report)) in enumerate(interp.exceptionreports)
                insert!(interp.reports, idx + i, report)
            end
        else
            # report `throw`s only if there is no circumvent pass, which is represented by
            # `Bottom`-annotated return type inference with non-empty `throw` blocks
            throw_calls = filter(is_throw_call′, me.src.code)
            if !isempty(throw_calls)
                push!(interp.exceptionreports, length(interp.reports) => ExceptionReport(me, interp, throw_calls))
            end
        end
    end

    return ret
end

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)
