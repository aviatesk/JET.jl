"""
    @invoke_native ex

HACK: calls down to `NativeInterpreter`'s abstract interpretation method while passing
  `TPInterpreter` so that subsequent methods that are oveloaded against `TPInterpreter` can
  be called from the native method.

`ex` is supposed to be the function definition signature of the target native method, which
  can be copy-and-pasted from the native compiler code.

e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
ret = @invoke_native abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                              max_methods::Int = InferenceParams(interp).MAX_METHODS)
```
"""
macro invoke_native(ex)
    f = first(ex.args)
    # TODO: maybe we need to handle kwargs too here
    arg2typs = map(ex.args[2:end]) do x
        if isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize")
            x = last(x.args)
        elseif isexpr(x, :kw)
            x = first(x.args)
        end
        return isexpr(x, :(::)) ? (first(x.args), last(x.args)) : (x, Any)
    end
    args = first.(arg2typs)
    typs = last.(arg2typs)
    return esc(:(invoke($(f), Tuple{$(typs...)}, $(args...))))
end

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

# overloads abstractinterpretation.jl
# -----------------------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/abstractinterpretation.jl

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.
function abstract_call_gf_by_type(interp::TPInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = @invoke_native abstract_call_gf_by_type(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)

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
    ret = @invoke_native abstract_eval_special_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)

    # report (global) undef var error
    if isa(e, GlobalRef)
        mod, sym = e.mod, e.name
        vgv = get_virtual_globalvar(interp, mod, sym)
        if isnothing(vgv)
            if !isdefined(mod, sym)
                add_remark!(interp, sv, GlobalUndefVarErrorReport(interp, sv, mod, sym))
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
    ret = @invoke_native abstract_eval_value(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)

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

function abstract_eval_statement(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke_native abstract_eval_statement(interp::AbstractInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)

    # assign virtual global variable
    # NOTE: this can introduce wrong side effects, and should be limited to toplevel frames ?
    stmt = get_cur_stmt(sv)
    if is_global_assign(stmt)
        set_virtual_globalvar!(interp, first((stmt::Expr).args)::GlobalRef, ret)
    end

    return ret
end

is_global_assign(@nospecialize(_)) = false
is_global_assign(ex::Expr)         = isexpr(ex, :(=)) && first(ex.args) isa GlobalRef

function get_virtual_globalvar(interp, mod, sym)
    vgvt4mod = get(interp.virtual_globalvar_table, mod, nothing)
    isnothing(vgvt4mod) && return nothing
    id2vgv = get(vgvt4mod, sym, nothing)
    isnothing(id2vgv) && return nothing
    return last(id2vgv)
end

function set_virtual_globalvar!(interp, gr, @nospecialize(t))
    vgvt4mod = get!(interp.virtual_globalvar_table, gr.mod, Dict())

    sym = gr.name
    id = get_id(interp)
    prev_id, prev_t = get!(vgvt4mod, sym, id => Bottom)

    if t === NOT_FOUND
        t = Bottom
    end

    vgvt4mod[sym] = id => id === prev_id ?
                          tmerge(prev_t, t) :
                          t
end

function typeinf_local(interp::TPInterpreter, frame::InferenceState)
    set_current_frame!(interp, frame)

    ret = @invoke_native typeinf_local(interp::AbstractInterpreter, frame::InferenceState)

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

# overloads typeinfer.jl
# ----------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/typeinfer.jl

# in this overload we can work on `CodeInfo` (and also `InferenceState`) where type inference
# (and maybe optimization) already ran on
function typeinf(interp::TPInterpreter, frame::InferenceState)
    set_current_frame!(interp, frame)

    ret = @invoke_native typeinf(interp::AbstractInterpreter, frame::InferenceState)

    # report (local) undef var error
    # this only works when optimization is enabled, just because `:throw_undef_if_not` and
    # `:(unreachable)` are introduced by `optimize`
    stmts = frame.src.code
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym, _ = stmt.args
            next_idx = idx + 1
            if checkbounds(Bool, stmts, next_idx) && @inbounds is_unreachable(stmts[next_idx])
                # the optimization so far has found this statement is never reachable;
                # TP reports it since it will invoke undef var error at runtime, or will just
                # be dead code otherwise

                add_remark!(interp, frame, LocalUndefVarErrorReport(interp, frame, sym))
            # else
                # by excluding this pass, TP accepts some false negatives (i.e. don't report
                # those that may actually happen on execution)
            end
        end
    end

    # report `throw` calls "appropriately" by simple inter-frame analysis
    # the basic stance here is really conservative so we don't report them unless they
    # will be inevitably called and won't be caught by `try/catch` in frame at any level
    # NOTE:
    # this is better to happen here because constant propagation can reduce the chance of
    # false negative reports by excluding unreachable control flows
    if get_result(frame) === Bottom
        # report `throw`s only if there is no circumvent pass, which is represented by
        # `Bottom`-annotated return type inference with non-empty `throw` blocks
        throw_calls = filter(is_throw_call′, frame.src.code)
        if !isempty(throw_calls)
            push!(interp.exception_reports, length(interp.reports) => ExceptionReport(interp, frame, throw_calls))
        end

        if isroot(frame)
            # if return type is `Bottom`-annotated for root frame, this means some error(s)
            # aren't caught by at any level and get propagated here

            # # just append collected `ExceptionReport`s
            # for (i, (idx, report)) in enumerate(interp.exception_reports)
            #     insert!(interp.reports, idx + i, report)
            # end

            # only report `ExceptionReport`s if there is no other error reported
            # TODO: change behaviour according to severity of collected report, e.g. don't count into `NativeRemark`
            isempty(interp.reports) && append!(interp.reports, last.(interp.exception_reports))
        end
    end

    return ret
end

function typeinf_edge(interp::TPInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    set_current_frame!(interp, caller)

    ret = @invoke_native typeinf_edge(interp::AbstractInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)

    return ret
end
