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
        if t !== Bottom && !⊑(Bool, t)
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

# FIXME: this is such an horrible and super fragile copy-and-paste ...
# the whole point is to not let abstract interpretation to halt even after there is an
# obvious error point found (, which is usually represented by `Bottom`-annotated type and
# triggers early-loop-escape in the native implementation), and allow TP to collect as much
# errors as possible; so the only diffs are:
# - the first argument changed to `TPInterpreter`
# - comment out 3 early-loop-escapes

__init__() = Core.eval(Core.Compiler, quote

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
                # if condt === Bottom
                #     break
                # end
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
                elseif isa(rt, Const) && rt.actual
                    rt = Const(rt.val)
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
                    # t === Bottom && break
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
                elseif hd === :inbounds || hd === :meta || hd === :loopinfo || hd === :code_coverage_effect
                    # these do not generate code
                else
                    t = abstract_eval_statement(interp, stmt, changes, frame)
                    # t === Bottom && break
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

end) # __init__() = Core.eval(Core.Compiler, quote
