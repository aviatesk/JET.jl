#=
overloads functions in https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl
so that `TPInterpreter` collects possible error points detected during the inference
=#

# HACK:
# calls down to `NativeInterpreter`'s abstract interpretation method while passing `TPInterpreter`
# so that its overloaded methods can be called within the sub/recursive method callls.
function invoke_native(f, interp::TPInterpreter, args...; kwargs...)
    argtypes = to_tuple_type((AbstractInterpreter, typeof.(args)...))
    return invoke(f, argtypes, interp, args...; kwargs...)
end

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

# overloads
# ---------

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.
function abstract_call_gf_by_type(interp::TPInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = invoke_native(abstract_call_gf_by_type, interp, f, argtypes, atype, sv, max_methods)::CallMeta

    info = ret.info

    # throw away previously-reported union-split no method errors that are revealed as
    # false positive by constant propagation; constant propagation always happens _after_
     # abstract interpretation with only using types (i.e. `atype`), and so the false positive
      # candidates are supposed to be reported in `interp.reports` at this point
    # watch on: https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    if CC.any(sv.result.overridden_by_const) && isa(info, MethodMatchInfo)
        inds = findall(interp.reports) do report
            return isa(report, NoMethodErrorReport) &&
                report.unionsplit &&
                atype ⊑ report.atype
        end
        if !isempty(inds)
            # false positive reports revealed
            deleteat!(interp.reports, inds)

            # exclude them from cache as well
            prewalk_inf_frame(sv) do frame
                key = hash(frame.linfo)
                if haskey(TPCACHE, key)
                    id, cached_reports = TPCACHE[key]
                    # @assert id === get_id(interp)
                    cached_inds = findall(cached_reports) do cached_report
                        return isa(cached_report, InferenceReportCache{NoMethodErrorReport}) &&
                            first(#= unionsplit =# cached_report.args)::Bool &&
                            atype ⊑ last(#= atype =# cached_report.args)::Type
                    end
                    deleteat!(cached_reports, cached_inds)
                end
            end
        end
    end

    # report no method error
    if isa(info, UnionSplitInfo)
        # if `info` is `UnionSplitInfo`, but there won't be a case where `info.matches` is empty
        for info in info.matches
            if is_empty_match(info)
                # no method match for this union split
                # ret.rt = Bottom # maybe we want to be more strict on error cases ?
                add_remark!(interp, sv, NoMethodErrorReport(interp, sv, true, atype))
            end
        end
    elseif isa(info, MethodMatchInfo) && is_empty_match(info)
        # really no method found, and so the return type should have never changed from its
        # initialization (i.e. `Bottom`)
        # typeassert(ret.rt, TypeofBottom)
        add_remark!(interp, sv, NoMethodErrorReport(interp, sv, false, atype))
    end

    return ret
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = invoke_native(abstract_eval_special_value, interp, e, vtypes, sv)

    # if isa(e, Slot)
    #     id = slot_id(e)
    #     s = sv.src.slotnames[id]
    #     t = vtypes[id].typ
    #     if t === NOT_FOUND || t === Bottom
    #         s = sv.src.slotnames[id]
    #         add_remark!(interp, sv, UndefVarErrorReport(interp, sv, sv.mod, s))
    #     end
    # end

    # report (global) undef var error
    if isa(e, GlobalRef)
        mod, sym = e.mod, e.name
        vgv = get_virtual_globalvar(interp, mod, sym)
        if isnothing(vgv)
            if !isdefined(mod, sym)
                add_remark!(interp, sv, UndefVarErrorReport(interp, sv, mod, sym))
                # typeassert(ret, Any)
                ret = Bottom # ret here should annotated as `Any` by `NativeInterpreter`, but here I would like to be more conservative and change it to `Bottom`
            end
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
# (which are originally global variables) in `TPInterpreter.virtual_globalvar_table` so that
# they can be referred across profilings on different virtual (toplevel) functions
# NOTE:
# virtual global assignments should happen here because `SlotNumber`s can be optimized away
# after the optimization happens
function typeinf_local(interp::TPInterpreter, frame::InferenceState)
    set_current_frame!(interp, frame)

    ret = invoke_native(typeinf_local, interp, frame)

    # assign virtual global variable for toplevel frames
    if istoplevel(interp) && isroot(frame)
        for (pc, stmt) in enumerate(frame.src.code)
            isexpr(stmt, :(=)) && set_virtual_globalvar!(interp, frame, pc, stmt)
        end
    end

    return ret
end

istoplevel(interp::TPInterpreter) = interp.istoplevel

function get_virtual_globalvar(interp, mod, sym)
    haskey(interp.virtual_globalvar_table, mod) || return nothing
    return get(interp.virtual_globalvar_table[mod], sym, nothing)
end

function set_virtual_globalvar!(interp, frame, pc, stmt)
    mod = frame.mod
    haskey(interp.virtual_globalvar_table, mod) || (interp.virtual_globalvar_table[mod] = Dict())

    slt = first(stmt.args)::Slot
    lhs = frame.src.slotnames[slt.id]::Symbol
    rhs = frame.src.ssavaluetypes[pc]
    if rhs === NOT_FOUND
        rhs = Bottom
    end

    interp.virtual_globalvar_table[mod][lhs] = rhs
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

# in this overload we can work on `InferenceState` that inference already ran on,
# and also maybe optimization has been done
function finish(me::InferenceState, interp::TPInterpreter)
    ret = invoke(finish, Tuple{InferenceState,AbstractInterpreter}, me, interp)

    # report `throw` calls "appropriately" by simple inter-frame analysis
    # the basic stance here is really conservative so we don't report them unless they
    # will be inevitably called and won't be caught by `try/catch` in frame at any level
    # NOTE:
    # this is better to happen here (after the optimization) to reduce the chance of false
    # negative reports
    if get_result(me) === Bottom
        # report `throw`s only if there is no circumvent pass, which is represented by
        # `Bottom`-annotated return type inference with non-empty `throw` blocks
        throw_calls = filter(is_throw_call′, me.src.code)
        if !isempty(throw_calls)
            push!(interp.exception_reports, length(interp.reports) => ExceptionReport(interp, me, throw_calls))
        end

        if isroot(me)
            # if return type is `Bottom`-annotated for root frame, this means some error(s)
            # aren't caught by at any level and get propagated here, and so let's report
            # `ExceptionReport` if exist
            for (i, (idx, report)) in enumerate(interp.exception_reports)
                insert!(interp.reports, idx + i, report)
            end
        end
    end

    return ret
end
