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

is_constant_propagated(frame) = CC.any(frame.result.overridden_by_const)

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
    ret = (@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                            max_methods::Int))::CallMeta

    info = ret.info
    linfo = sv.linfo
    iscp = is_constant_propagated(sv)

    # throw away previously-reported `NoMethodErrorReport` if we re-infer this frame with
    # constant propagation; this always happens _after_ abstract interpretation with only
    # using lattice types (i.e. `atype`), and the reports with constants are always more
    # accurate than those without them, so let's just throw away them on this pass
    #
    # NOTE:
    # - we do NOT cache `NoMethodErrorReport`s with constant propagation (i.e. `NoMethodErrorReportConst`)
    #   just because they're actually not bound to this `sv.linfo`
    # - but we exclude them from `TPCACHE`; we want this logic since otherwise inference on
    #   cached `MethodInstance` of `setproperty!` against structs with multiple type fields
    #   will report union-split `NoMethodErrorReport` (that can be excluded by constant propagation)
    #   TODO: this can be wrong in some cases since actual errors that would happen without
    #   the current constants can be threw away as well; hopefully another future
    #   constant propagation "re-reveals" the threw away errors, but of course this doesn't
    #   necessarily hold true always
    #
    # xref (maybe coming future change of constant propagation logic):
    # https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    if iscp
        inds = findall(interp.reports) do report
            return isa(report, NoMethodErrorReport) &&
                linfo == report.linfo # use `sv.linfo` as key
                # ⊑(atype, report.atype) # use `atype` as key
        end
        isempty(inds) || deleteat!(interp.reports, inds)

        # exclude them from cache as well
        prewalk_inf_frame(sv) do frame
            key = frame.linfo
            if haskey(TPCACHE, key)
                id, cached_reports = TPCACHE[key]
                # @assert id === get_id(interp)
                cached_inds = findall(cached_reports) do cached_report
                    return isa(cached_report, InferenceReportCache{NoMethodErrorReport}) &&
                        linfo == last(#= linfo =# cached_report.args)::MethodInstance # use `sv.linfo` as key
                        # ⊑(atype, last(#= atype =# cached_report.args)::Type) # use `atype` as key
                end
                deleteat!(cached_reports, cached_inds)
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
                er = (iscp ? NoMethodErrorReportConst : NoMethodErrorReport)(interp, sv, true, atype, linfo)
                add_remark!(interp, sv, er)
            end
        end
    elseif isa(info, MethodMatchInfo) && is_empty_match(info)
        # really no method found, and so the return type should have never changed from its
        # initialization (i.e. `Bottom`)
        # typeassert(ret.rt, TypeofBottom)
        # add_remark!(interp, sv, NoMethodErrorReport(interp, sv, false, atype))
        er = (iscp ? NoMethodErrorReportConst : NoMethodErrorReport)(interp, sv, false, atype, linfo)
        add_remark!(interp, sv, er)
    end

    return ret
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
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
    if is_global_assign(stmt)
        gr = first((stmt::Expr).args)::GlobalRef
        set_virtual_globalvar!(interp, gr.mod, gr.name, ret)
    end

    return ret
end

is_global_assign(@nospecialize(_)) = false
is_global_assign(ex::Expr)         = isexpr(ex, :(=)) && first(ex.args) isa GlobalRef

function get_virtual_globalvar(interp, mod, sym, sv = nothing)
    vgvt4mod = get(interp.virtual_globalvar_table, mod, nothing)
    isnothing(vgvt4mod) && return

    x = get(vgvt4mod, sym, nothing)
    isnothing(x) && return

    _, t, _, li = x
    if !isnothing(sv)
        # `sv` might be nothing when called in test, don't add backedge for that case
        add_backedge!(li, sv)
    end

    return t
end

function set_virtual_globalvar!(interp, mod, sym, @nospecialize(t))
    vgvt4mod = get!(interp.virtual_globalvar_table, mod, Dict())

    id = get_id(interp)
    prev_id, prev_t, λsym, li = haskey(vgvt4mod, sym) ?
                                vgvt4mod[sym] :
                                (id, Bottom, gen_dummy_backedge!(mod)...)

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

function gen_dummy_backedge!(m)
    @gensym λsym
    return λsym, force_invalidate!(m, λsym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(m, λsym)
    λ = Core.eval(m, :($(λsym)() = nothing))
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, Core.svec())
end

function typeinf_local(interp::TPInterpreter, frame::InferenceState)
    set_current_frame!(interp, frame)

    ret = @invoke typeinf_local(interp::AbstractInterpreter, frame::InferenceState)

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

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

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
    end

    return ret
end

function typeinf_edge(interp::TPInterpreter, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    set_current_frame!(interp, caller)

    ret = @invoke typeinf_edge(interp::AbstractInterpreter, method::Method, atypes, sparams::SimpleVector, caller::InferenceState)

    return ret
end

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
