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

istoplevel(sv) = isa(sv.linfo.def, Module)

mutable struct VirtualGlobalVariable
    # actual profiled type
    t::Any
    # keeps `id` of `TPInterpreter` that defined this
    id::Symbol
    # `Symbol` of a dummy generic function that generates dummy backedge (i.e. `li`)
    edge_sym::Symbol
    # dummy backedge, which will be invalidated on update of `t`
    li::MethodInstance
    # whether this virtual global variable is defined as constant or not
    # TODO: use this field
    isconst::Bool

    function VirtualGlobalVariable(@nospecialize(t),
                                   id::Symbol,
                                   edge_sym::Symbol,
                                   li::MethodInstance,
                                   isconst::Bool = true,
                                   )
        return new(t, id, edge_sym, li, isconst)
    end
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

function is_empty_match(info)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

function abstract_eval_special_value(interp::TPInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if isa(e, Symbol)
            e = GlobalRef(sv.mod, e)
        end
    end

    ret = @invoke abstract_eval_special_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    if isa(ret, Const)
        # unwrap virtual global variable to actual type
        val = ret.val
        if isa(val, VirtualGlobalVariable)
            # add dummy backedge, which will be invalidated on update of this vitual global variable
            add_backedge!(val.li, sv)

            ret = val.t
        end
    elseif isa(e, GlobalRef)
        # report access to undefined global variable
        mod, sym = e.mod, e.name
        if isdefined(mod, sym)
            # FIXME: this might produce wrong result if the current frame is cached and the
            # global variable later be updated (both actually or abstractly, anyway);
            # 1. if we have established a way to convert a already-existing global variable
            #    to virtual global variable we can do that here and add backedge
            # 2. `NativeInterpreter` just returns `Any` for this case, so it might be okay
            #    if TP just follows that and give up profiling on non-constant global variables ...
            val = getfield(mod, sym)
            @debug "propagating non-constant global variable as constant" mod sym val
            ret = Const(val)
        else
            add_remark!(interp, sv, GlobalUndefVarErrorReport(interp, sv, mod, sym))
            # `ret` here should be annotated as `Any` by `NativeInterpreter`, but we want to
            # be more conservative and change it to `Bottom` and suppress any further abstract
            # interpretation with this
            ret = Bottom
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

    # partial actual interpretation
    istoplevel(sv) && interpret!(interp, sv, e)

    # assign virtual global variable
    lhs = get_global_assignment_lhs(sv)
    isa(lhs, GlobalRef) && set_virtual_globalvar!(interp, lhs.mod, lhs.name, ret)

    return ret
end

function get_global_assignment_lhs(sv::InferenceState)
    stmt = get_cur_stmt(sv)

    lhs = get_lhs1(stmt)
    isnothing(lhs) && return nothing
    isa(lhs, GlobalRef) && return lhs
    istoplevel(sv) && isa(lhs, Symbol) && return GlobalRef(sv.mod, lhs)

    return nothing
end

function set_virtual_globalvar!(interp, mod, name, @nospecialize(t))
    local update::Bool = false
    id = get_id(interp)

    prev_t, prev_id, (edge_sym, li) = if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, VirtualGlobalVariable)
            # update previously-defined virtual global variable
            update = true
            val.t, val.id, (val.edge_sym, val.li)
        else
            @warn "TypeProfiler.jl can't trace updates of global variable that already have values"
            return
        end
    else
        # define new virtual global variable
        Bottom, id, gen_dummy_backedge(mod)
    end

    # at this point undefined slots may still be annotated as `NOT_FOUND` (e.g. when there is
    # undefined slot, etc.), which can't be used as abstract value for later profiling,
    # so replace it with Bottom
    if t === NOT_FOUND
        t = Bottom
    end

    if id === prev_id
        # if the previous virtual global variable assignment happened in the same inference process,
        # TP needs to perform type merge, otherwise "just update"s it
        t = tmerge(prev_t, t)
    else
        # invalidate the dummy backedge that is bound to this virtual global variable,
        # so that depending `MethodInstance` will run fresh type inference on the next hit
        li = force_invalidate!(mod, edge_sym)
    end

    return if update
        Core.eval(mod, quote
            local name = $(name)
            name.t = $(t)
            name.id = $(QuoteNode(id))
            name.edge_sym = $(QuoteNode(edge_sym))
            name.li = $(li)
            name
        end)
    else
        vgv = VirtualGlobalVariable(t, id, edge_sym, li)
        Core.eval(mod, :(const $(name) = $(vgv)))
    end::VirtualGlobalVariable
end

function gen_dummy_backedge(mod)
    @gensym edge_sym
    return edge_sym, force_invalidate!(mod, edge_sym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(mod, edge_sym)
    λ = Core.eval(mod, :($(edge_sym)() = return))::Function
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, Core.svec())::MethodInstance
end

# FIXME: remove this monkey-patch
# the only point of this overload is to slip `evaluate_methoddef` into the interpretation of
# `Expr(:method, ...)` statement
push_inithook!() do; Core.eval(Core.Compiler, quote

# make as much progress on `frame` as possible (without handling cycles)
function typeinf_local(interp::$(TPInterpreter), frame::InferenceState)
    @assert !frame.inferred
    frame.dont_work_on_me = true # mark that this function is currently on the stack
    W = frame.ip
    s = frame.stmt_types
    n = frame.nstmts
    while frame.pc´´ <= n
        # make progress on the active ip set
        local pc::Int = frame.pc´´ # current program-counter
        while true # inner loop optimizes the common case where it can run straight from pc to pc + 1
            #print(pc,": ",s[pc],"\n")
            local pc´::Int = pc + 1 # next program-counter (after executing instruction)
            if pc == frame.pc´´
                # need to update pc´´ to point at the new lowest instruction in W
                min_pc = _bits_findnext(W.bits, pc + 1)
                frame.pc´´ = min_pc == -1 ? n + 1 : min_pc
            end
            delete!(W, pc)
            frame.currpc = pc
            frame.cur_hand = frame.handler_at[pc]
            frame.stmt_edges[pc] === nothing || empty!(frame.stmt_edges[pc])
            stmt = frame.src.code[pc]
            changes = s[pc]::VarTable
            t = nothing

            hd = isa(stmt, Expr) ? stmt.head : nothing

            if isa(stmt, NewvarNode)
                sn = slot_id(stmt.slot)
                changes[sn] = VarState(Bottom, true)
            elseif isa(stmt, GotoNode)
                pc´ = (stmt::GotoNode).label
            elseif isa(stmt, GotoIfNot)
                condt = abstract_eval_value(interp, stmt.cond, s[pc], frame)
                if condt === Bottom
                    break
                end
                condval = maybe_extract_const_bool(condt)
                l = stmt.dest::Int
                # constant conditions
                if condval === true
                elseif condval === false
                    pc´ = l
                else
                    # general case
                    frame.handler_at[l] = frame.cur_hand
                    changes_else = changes
                    if isa(condt, Conditional)
                        if condt.elsetype !== Any && condt.elsetype !== changes[slot_id(condt.var)]
                            changes_else = StateUpdate(condt.var, VarState(condt.elsetype, false), changes_else)
                        end
                        if condt.vtype !== Any && condt.vtype !== changes[slot_id(condt.var)]
                            changes = StateUpdate(condt.var, VarState(condt.vtype, false), changes)
                        end
                    end
                    newstate_else = stupdate!(s[l], changes_else)
                    if newstate_else !== false
                        # add else branch to active IP list
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                        s[l] = newstate_else
                    end
                end
            elseif isa(stmt, ReturnNode)
                pc´ = n + 1
                rt = widenconditional(abstract_eval_value(interp, stmt.val, s[pc], frame))
                if !isa(rt, Const) && !isa(rt, Type) && !isa(rt, PartialStruct)
                    # only propagate information we know we can store
                    # and is valid inter-procedurally
                    rt = widenconst(rt)
                end
                if tchanged(rt, frame.bestguess)
                    # new (wider) return type for frame
                    frame.bestguess = tmerge(frame.bestguess, rt)
                    for (caller, caller_pc) in frame.cycle_backedges
                        # notify backedges of updated type information
                        typeassert(caller.stmt_types[caller_pc], VarTable) # we must have visited this statement before
                        if !(caller.src.ssavaluetypes[caller_pc] === Any)
                            # no reason to revisit if that call-site doesn't affect the final result
                            if caller_pc < caller.pc´´
                                caller.pc´´ = caller_pc
                            end
                            push!(caller.ip, caller_pc)
                        end
                    end
                end
            elseif hd === :enter
                l = stmt.args[1]::Int
                frame.cur_hand = Pair{Any,Any}(l, frame.cur_hand)
                # propagate type info to exception handler
                old = s[l]
                new = s[pc]::Array{Any,1}
                newstate_catch = stupdate!(old, new)
                if newstate_catch !== false
                    if l < frame.pc´´
                        frame.pc´´ = l
                    end
                    push!(W, l)
                    s[l] = newstate_catch
                end
                typeassert(s[l], VarTable)
                frame.handler_at[l] = frame.cur_hand
            elseif hd === :leave
                for i = 1:((stmt.args[1])::Int)
                    frame.cur_hand = (frame.cur_hand::Pair{Any,Any}).second
                end
            else
                if hd === :(=)
                    t = abstract_eval_statement(interp, stmt.args[2], changes, frame)
                    t === Bottom && break
                    frame.src.ssavaluetypes[pc] = t
                    lhs = stmt.args[1]
                    if isa(lhs, Slot)
                        changes = StateUpdate(lhs, VarState(t, false), changes)
                    end
                elseif hd === :method
                    fname = stmt.args[1]
                    if isa(fname, Slot)
                        changes = StateUpdate(fname, VarState(Any, false), changes)
                    end
                    #=== TypeProfiler.jl monkey-patch start ===#
                    if $(istoplevel)(frame)
                        interp.ssavalues[$(get_cur_pc)(frame)] = $(evaluate_methoddef)(interp, frame, stmt)
                    end
                    #=== TypeProfiler.jl monkey-patch end ===#
                elseif hd === :inbounds || hd === :meta || hd === :loopinfo || hd === :code_coverage_effect
                    # these do not generate code
                else
                    t = abstract_eval_statement(interp, stmt, changes, frame)
                    t === Bottom && break
                    if !isempty(frame.ssavalue_uses[pc])
                        record_ssa_assign(pc, t, frame)
                    else
                        frame.src.ssavaluetypes[pc] = t
                    end
                end
                if frame.cur_hand !== nothing && isa(changes, StateUpdate)
                    # propagate new type info to exception handler
                    # the handling for Expr(:enter) propagates all changes from before the try/catch
                    # so this only needs to propagate any changes
                    l = frame.cur_hand.first::Int
                    if stupdate1!(s[l]::VarTable, changes::StateUpdate) !== false
                        if l < frame.pc´´
                            frame.pc´´ = l
                        end
                        push!(W, l)
                    end
                end
            end

            if t === nothing
                # mark other reached expressions as `Any` to indicate they don't throw
                frame.src.ssavaluetypes[pc] = Any
            end

            pc´ > n && break # can't proceed with the fast-path fall-through
            frame.handler_at[pc´] = frame.cur_hand
            newstate = stupdate!(s[pc´], changes)
            if isa(stmt, GotoNode) && frame.pc´´ < pc´
                # if we are processing a goto node anyways,
                # (such as a terminator for a loop, if-else, or try block),
                # consider whether we should jump to an older backedge first,
                # to try to traverse the statements in approximate dominator order
                if newstate !== false
                    s[pc´] = newstate
                end
                push!(W, pc´)
                pc = frame.pc´´
            elseif newstate !== false
                s[pc´] = newstate
                pc = pc´
            elseif pc´ in W
                pc = pc´
            else
                break
            end
        end
    end
    frame.dont_work_on_me = false
    nothing
end

end); end # push_inithook!() do; Core.eval(Core.Compiler, quote

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

function profile_frame!(interp::TPInterpreter, frame::InferenceState)
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
