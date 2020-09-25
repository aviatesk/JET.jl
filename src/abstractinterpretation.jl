"""
    @invoke f(arg::T, ...; kwargs...)

provides a convenient way to call [`invoke`](@ref);
this could be used to call down to `NativeInterpreter`'s abstract interpretation method of
  `f` while passing `TPInterpreter` so that subsequent calls of generic functions overloaded
  against `TPInterpreter` can be called from the native method body of `f`.

e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                 max_methods::Int)
```
"""
macro invoke(ex)
    f = first(ex.args)
    argtypes = []
    args = []
    kwargs = []
    for x in ex.args[2:end]
        if isexpr(x, :parameters)
            append!(kwargs, x.args)
        elseif isexpr(x, :kw)
            push!(kwargs, x)
        else
            arg, argtype = isexpr(x, :(::)) ? (x.args...,) : (x, Any)
            push!(args, arg)
            push!(argtypes, argtype)
        end
    end
    return if isempty(kwargs)
        :(invoke($(f), Tuple{$(argtypes...)}, $(args...))) # might not be necessary
    else
        :(invoke($(f), Tuple{$(argtypes...)}, $(args...); $(kwargs...)))
    end |> esc
end

# overloads abstractinterpretation.jl
# -----------------------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/abstractinterpretation.jl

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.
function abstract_call_gf_by_type(interp::TPInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = (@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                            max_methods::Int))::CallMeta

    info = ret.info

    # report no method error
    if isa(info, UnionSplitInfo)
        # if `info` is `UnionSplitInfo`, but there won't be a case where `info.matches` is empty
        for info in info.matches
            if is_empty_match(info)
                # no method match for this union split
                add_remark!(interp, sv, NoMethodErrorReport(interp, sv, true, atype))
            end
        end
    elseif isa(info, MethodMatchInfo) && is_empty_match(info)
        # really no method found
        # @assert ret.rt === Bottom # the return type should have never changed from its initialization
        add_remark!(interp, sv, NoMethodErrorReport(interp, sv, false, atype))
    end

    return ret
end

is_constant_propagated(frame) = CC.any(frame.result.overridden_by_const)

function is_empty_match(info)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    # for toplevel frame, we need to resolve symbols to global references by ourselves
    if isa(e, Symbol) && istoplevel(sv)
        e = GlobalRef(sv.mod, e)
    end

    ret = @invoke abstract_eval_special_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # resolve global reference to virtual global variable, or report it if undefined
    if !isa(ret, Const) && isa(e, GlobalRef)
        mod, sym = e.mod, e.name
        vgv = get_virtual_globalvar(interp, mod, sym, sv)
        if isnothing(vgv)
            if !isdefined(mod, sym) # we actually don't need this check, but let's add this just for robustness
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
    ret = @invoke abstract_eval_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

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
    ret = @invoke abstract_eval_statement(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # assign virtual global variable
    stmt = get_cur_stmt(sv)
    gr = get_assigned_globalref(sv, stmt)
    if isa(gr, GlobalRef)
        set_virtual_globalvar!(interp, gr.mod, gr.name, ret)
    end

    return ret
end

istoplevel(frame) = isa(frame.linfo.def, Module)

function get_assigned_globalref(frame, stmt)
    isexpr(stmt, :(=)) || return nothing
    lhs = first(stmt.args)
    if istoplevel(frame) && isa(lhs, Symbol)
        return GlobalRef(frame.mod, lhs)
    elseif isa(lhs, GlobalRef)
        return lhs
    end
    return nothing
end

function get_virtual_globalvar(interp, mod, sym, caller = nothing)
    vgvt4mod = get(interp.virtual_globalvar_table, mod, nothing)
    isnothing(vgvt4mod) && return nothing

    x = get(vgvt4mod, sym, nothing)
    isnothing(x) && return nothing

    _, t, _, li = x
    if !isnothing(caller)
        # `caller` might be `nothing` when called in test, don't add backedge for that case
        add_backedge!(li, caller)
    end

    return t
end

function set_virtual_globalvar!(interp, mod, sym, @nospecialize(t))
    vgvt4mod = get!(interp.virtual_globalvar_table, mod, Dict())

    id = get_id(interp)
    prev_id, prev_t, λsym, li = haskey(vgvt4mod, sym) ?
                                vgvt4mod[sym] :
                                (id, Bottom, gen_dummy_backedge(mod)...)

    if t === NOT_FOUND
        t = Bottom
    end

    if id === prev_id
        # if the previous virtual global variable assignment happened in the same inference process
        # TP needs to perform type merge, otherwise TP can "just update" it
        # TODO: add some backedge for this as well ? it should make the inference correct, but maybe slower to converge
        t = tmerge(prev_t, t)
    else
        # invalidate the dummy backedge that was bound to this virtual global variable,
        # so that depending `MethodInstance` will run fresh type inference on the next hit
        li = force_invalidate!(mod, λsym)
    end

    vgvt4mod[sym] = id, t, λsym, li
end

function gen_dummy_backedge(m)
    @gensym λsym
    return λsym, force_invalidate!(m, λsym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(m, λsym)
    λ = Core.eval(m, :($(λsym)() = nothing))
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, Core.svec())
end

# overloads typeinfer.jl
# ----------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/typeinfer.jl

# in this overload we can work on `CodeInfo` (and also `InferenceState`) where type inference
# (and maybe optimization) already ran on
function typeinf(interp::TPInterpreter, frame::InferenceState)
    # throw away previously-collected error reports that have a lineage of this frame if we
    # re-infer this frame with constant propagation, assuming results with constants are
    # always more accurate than those without them (COMBAK: is this really true ?); this can
    # happen only _after_ abstract interpretation without constants (i.e. just using `atype`)
    #
    # xref (maybe coming future change of constant propagation logic):
    # https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    if is_constant_propagated(frame)
        linfo = frame.linfo
        is_lineage′ = Fix1(is_lineage, linfo)

        # throw away previously-collected error reports
        filter!(!is_lineage′, interp.reports)
    end

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    # report (local) undef var error
    # this only works when optimization is enabled, just because `:throw_undef_if_not` and
    # `:(unreachable)` are introduced by `optimize`
    stmts = frame.src.code
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym, _ = stmt.args
            next_idx = idx + 1
            if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
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
    end

    return ret
end

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

# entry
# -----

function profile_frame(interp::TPInterpreter, frame::InferenceState)
    typeinf(interp, frame)

    # if return type is `Bottom`-annotated for this frame, this may mean some `throw`(s)
    # aren't caught by at any level and get propagated here, or there're other critical
    # inference error found
    if get_result(frame) === Bottom
        # let's report report `ExceptionReport`s only if there is no other error reported
        # TODO: change behaviour according to severity of collected report, e.g. don't take
        # into account `NativeRemark`s, etc
        isempty(interp.reports) && append!(interp.reports, last.(interp.exception_reports))

        # # just append collected `ExceptionReport`s
        # for (i, (idx, report)) in enumerate(interp.exception_reports)
        #     insert!(interp.reports, idx + i, report)
        # end
    end

    return interp, frame
end
