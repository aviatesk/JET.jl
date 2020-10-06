# defines partial, actual interpretation of Julia's lowered AST, which will be injected into
# the abstract interpretation of toplevel `MethodInstance`

function interpret!(interp, sv, @nospecialize(node))
    if isa(node, Expr)
        head = node.head
        if head === :call
            f = lookup(interp, sv, first(node.args))
            if isa(f, Builtin) || f === Core.Typeof
                args = lookup.(Ref(interp), Ref(sv), node.args[2:end])
                rhs = f(args...)
                do_assignment!(interp, sv, rhs)
            end
        elseif head === :new
            args = lookup.(Ref(interp), Ref(sv), node.args)
            T = popfirst!(args)
            rhs = ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), T, args, length(args))
            do_assignment!(interp, sv, rhs)
        elseif head === :thunk
            # XXX: should flatten this ?
            rhs = Core.eval(sv.mod, node)
            do_assignment!(interp, sv, rhs)
        end
    elseif isa(node, Symbol)
        rhs = getfield(sv.mod, node)
        do_assignment!(interp, sv, rhs)
    else
        # literal
        do_assignment!(interp, sv, node)
    end
end

function lookup(interp, sv, @nospecialize(node))
    return if isa(node, SSAValue)
        @assert isassigned(interp.ssavalues, node.id)
        interp.ssavalues[node.id]
    elseif isa(node, Slot)
        id = node.id::Int
        @assert isassigned(interp.locals, id)
        interp.locals[id]
    elseif isa(node, GlobalRef)
        getfield(node.mod, node.name)
    elseif isa(node, Symbol)
        getfield(sv.mod, node)
    elseif isa(node, QuoteNode)
        node.value
    else
        node # literal
    end
end

function do_assignment!(interp, sv, @nospecialize(rhs))
    pc = get_cur_pc(sv)

    interp.ssavalues[pc] = rhs

    lhs = get_lhs1(sv.src.code[pc])
    if isa(lhs, Slot)
        interp.locals[lhs.id::Int] = rhs
    end
end

get_lhs1(stmt) = isexpr(stmt, :(=)) ? first(stmt.args) : nothing

# evaluate this method definion, in a way it's available for succeeding inference process
function evaluate_methoddef(interp, sv, e)
    f = first(e.args)
    mod = sv.mod
    if isa(f, Symbol)
        f = getfield(mod, f)
    end
    length(e.args) == 1 && return f
    sig = lookup(interp, sv, e.args[2])::SimpleVector
    body = lookup(interp, sv, e.args[3])
    ccall(:jl_method_def, Cvoid, (Any, Any, Any), sig, body, mod)

    # HACK: update world age for this toplevel inference process
    new_world = get_world_counter()
    interp.native = NativeInterpreter(new_world;
                                      inf_params = InferenceParams(interp),
                                      opt_params = OptimizationParams(interp),
                                      )
    sv.world = new_world
    sv.valid_worlds = WorldRange(CC.first(sv.valid_worlds), new_world)
    sv.method_table = CachedMethodTable(method_table(interp))

    return nothing
end
