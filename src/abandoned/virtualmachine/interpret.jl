# below adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/e42045c39f6363aa5035da5c320587b5264ca644/src/interpret.jl

isassign(frame) = isassign(frame, frame.pc)
isassign(frame, pc) = pc in frame.used

# TODO: report undefine variable error
lookup_var_type(frame, val::SSAValue) = typeof′(frame.ssavaluetypes[val.id])
lookup_var_type(frame, ref::GlobalRef) = typeof′(getfield(ref.mod, ref.name))
function lookup_var_type(frame, slot::SlotNumber)
    slottypes = frame.slottypes
    id = slot.id
    if isassigned(slottypes, id)
        return typeof′(slottypes[id])
    else
        throw(UndefVarError(frame.codeinfo.slotnames[id]))
    end
end
lookup_var_type(frame, sym::Symbol) = typeof′(getfield(moduleof(frame), sym))

function lookup_expr_type(frame, e::Expr)
    head = e.head
    # head === :the_exception && return frame.framedata.last_exception[]
    # if head === :static_parameter
    #     arg = e.args[1]::Int
    #     if isassigned(frame.framedata.sparams, arg)
    #         return frame.framedata.sparams[arg]
    #     else
    #         syms = sparam_syms(frame.framecode.scope)
    #         throw(UndefVarError(syms[arg]))
    #     end
    # end
    head === :boundscheck && length(e.args) == 0 && return Bool
    error("invalid lookup expr ", e)
end

"""
    rhs = @lookup_type(frame, node)
    rhs = @lookup_type(mod, frame, node)

This macro is adapted from `JuliaInterpreter.@lookup` but works on "type-level".

This macro looks up previously-computed values referenced as `SSAValues`, `SlotNumbers`,
`GlobalRefs`, `QuoteNode`, sparams or exception reference expression.
It will also lookup symbols in `moduleof(frame)`; this can be supplied ahead-of-time via
the 3-argument version.
If none of the above apply, the value of `node` will be returned.
"""
macro lookup_type(args...)
    length(args) == 2 || length(args) == 3 || error("invalid number of arguments ", length(args))
    havemod = length(args) == 3
    local mod
    if havemod
        mod, frame, node = args
    else
        frame, node = args
    end
    nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
    fallback = if havemod
        quote
            if isa($nodetmp, Symbol)
                getfield($(esc(mod)), $nodetmp)
            else
                $nodetmp
            end |> $(typeof′)
        end
    else
        quote $(typeof′)($nodetmp) end
    end

    return quote let
        $nodetmp = $(esc(node))
        isa($nodetmp, SSAValue) ? lookup_var_type($(esc(frame)), $nodetmp) :
        isa($nodetmp, GlobalRef) ? lookup_var_type($(esc(frame)), $nodetmp) :
        isa($nodetmp, SlotNumber) ? lookup_var_type($(esc(frame)), $nodetmp) :
        isa($nodetmp, Symbol) ? lookup_var_type($(esc(frame)), $nodetmp) :
        isa($nodetmp, Expr) ? lookup_expr_type($(esc(frame)), $nodetmp) :
        isa($nodetmp, QuoteNode) ? $nodetmp.value :
        $fallback
    end end
end

function virtual_assign!(frame, @nospecialize(lhs), @nospecialize(rhs))
    rhs = ProfiledType(rhs)
    if isa(lhs, SSAValue)
        frame.ssavaluetypes[lhs.id] = rhs
    elseif isa(lhs, SlotNumber)
        frame.slottypes[lhs.id] = rhs
    elseif isa(lhs, GlobalRef)
        Core.eval(lhs.mod, :($(lhs.name) = $(QuoteNode(rhs)))) # NOTE: we may need to replace `lhs.mod` to virtual module
    elseif isa(lhs, Symbol)
        Core.eval(moduleof(frame), :($lhs = $(QuoteNode(rhs))))
    end
end

function collect_argtypes(frame, call_expr; isfc = false)
    mod = moduleof(frame)
    return map(enumerate(call_expr.args)) do (i, arg)
        return if isone(i)
            isfc ? #=resolvefc(frame, arg)=# error("foreeigncall unimplemented") : @lookup_type(mod, frame, arg)
        else
            @lookup_type(mod, frame, arg)
        end
    end
end

# function evaluate_call_recurse!(@nospecialize(recurse), frame::Frame, call_expr::Expr; enter_generated::Bool=false)
#     pc = frame.pc
#     ret = bypass_builtins(frame, call_expr, pc)
#     isa(ret, Some{Any}) && return ret.value
#     ret = maybe_evaluate_builtin(frame, call_expr, true)
#     isa(ret, Some{Any}) && return ret.value
#     call_expr = ret
#     fargs = collect_args(frame, call_expr)
#     if fargs[1] === Core.eval
#         return Core.eval(fargs[2], fargs[3])  # not a builtin, but worth treating specially
#     elseif fargs[1] === Base.rethrow
#         err = length(fargs) > 1 ? fargs[2] : frame.framedata.last_exception[]
#         throw(err)
#     end
#     if fargs[1] === Core.invoke # invoke needs special handling
#         f_invoked = which(fargs[2], fargs[3])
#         fargs_pruned = [fargs[2]; fargs[4:end]]
#         sig = Tuple{_Typeof.(fargs_pruned)...}
#         ret = prepare_framecode(f_invoked, sig; enter_generated=enter_generated)
#         isa(ret, Compiled) && invoke(fargs[2:end]...)
#         framecode, lenv = ret
#         lenv === nothing && return framecode  # this was a Builtin
#         fargs = fargs_pruned
#     else
#         framecode, lenv = get_call_framecode(fargs, frame.framecode, frame.pc; enter_generated=enter_generated)
#         if lenv === nothing
#             if isa(framecode, Compiled)
#                 f = popfirst!(fargs)  # now it's really just `args`
#                 return Base.invokelatest(f, fargs...)
#             end
#             return framecode  # this was a Builtin
#         end
#     end
#     newframe = prepare_frame_caller(frame, framecode, fargs, lenv)
#     npc = newframe.pc
#     shouldbreak(newframe, npc) && return BreakpointRef(newframe.framecode, npc)
#     # if the following errors, handle_err will pop the stack and recycle newframe
#     if recurse === finish_and_return!
#         # Optimize this case to avoid dynamic dispatch
#         ret = finish_and_return!(finish_and_return!, newframe, false)
#     else
#         ret = recurse(recurse, newframe, false)
#     end
#     isa(ret, BreakpointRef) && return ret
#     frame.callee = nothing
#     return_from(newframe)
#     return ret
# end

function virtual_call!(frame, call_expr)
    argtypes = collect_argtypes(frame, call_expr)
    ft = first(argtypes)
    if ft <: Builtin
        # TODO: setup a function to profile on builtins in Profiler
        f = ft.instance
        argtypes = argtypes[2:end]
        builtin_tfunction(frame.interp, f, argtypes, #=InferenceState=# nothing)
    else
        tt = to_tuple_type(argtypes)
        _, frame = Profiler.profile_call!(frame.interp, tt)
        TypeProfiler.get_rettyp(frame)
    end
end

# The following come up only when evaluating toplevel code

function evaluate_methoddef(frame, node)
    f = node.args[1]
    if isa(f, Symbol)
        mod = moduleof(frame)
        f = if isdefined(mod, f)
            getfield(mod, f)
        else
            # yet not defined, create an new
            Core.eval(moduleof(frame), Expr(:function, f))
        end
    end
    length(node.args) == 1 && return f
    sig = @lookup_type(frame, node.args[2])::SimpleVector
    body = @lookup_type(frame, node.args[3])
    ccall(:jl_method_def, Cvoid, (Any, Any, Any), sig, body, moduleof(frame))
    return nothing
end

function virtual_eval_rhs(frame, node)
    head = node.head
    if false
    # if head === :new
    #     mod = moduleof(frame)
    #     args = [@lookup_type(mod, frame, arg) for arg in node.args]
    #     T = popfirst!(args)
    #     rhs = ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32), T, args, length(args))
    #     return rhs
    # elseif head === :splatnew # Julia 1.2+
    #     mod = moduleof(frame)
    #     rhs = ccall(:jl_new_structt, Any, (Any, Any), @lookup_type(mod, frame, node.args[1]), @lookup_type(mod, frame, node.args[2]))
    #     return rhs
    elseif head === :isdefined
        return virtual_isdefined(frame, first(node.args))
    elseif head === :call
        return virtual_call!(frame, node)
    # elseif head === :foreigncall || head === :cfunction
    #     return evaluate_foreigncall(frame, node)
    # elseif head === :copyast
    #     val = (node.args[1]::QuoteNode).value
    #     return isa(val, Expr) ? copy(val) : val
    # elseif head === :enter
    #     return length(frame.framedata.exception_frames)
    # elseif head === :boundscheck
    #     return true
    # elseif head === :meta || head === :inbounds || head == (@static VERSION >= v"1.2.0-DEV.462" ? :loopinfo : :simdloop) ||
    #        head === :gc_preserve_begin || head === :gc_preserve_end
    #     return nothing
    elseif head === :method && length(node.args) == 1
        return evaluate_methoddef(frame, node)
    end
    return lookup_expr_type(frame, node)
end

function virtual_isdefined(frame, @nospecialize(node))
    return Bool
    # TODO constant propagation
    # data = frame.framedata
    # if isa(node, SlotNumber)
    #     return data.locals[node.id] !== nothing
    # elseif isexpr(node, :static_parameter)
    #     return isassigned(data.sparams, node.args[1]::Int)
    # elseif isa(node, GlobalRef)
    #     return isdefined(node.mod, node.name)
    # elseif isa(node, Symbol)
    #     return isdefined(moduleof(frame), node)
    # end
    # error("unrecognized isdefined node ", node)
end

"""
    pc = step_expr!(frame::Frame)

Virtually execute and profile the next statement in `frame`.
`pc` is the new program counter, or `nothing` if the virtual execution terminates.

!!! note
    As opposed to `JuliaInterpreter.jl.step_expr!`, this function is supposed to only work
      on toplevel code.
"""
step_expr!(frame::Frame) = step_expr!(frame, pc_expr(frame))
function step_expr!(frame, @nospecialize(node))
    pc = frame.pc
    local rhs
    if isa(node, Expr)
        if node.head === :(=)
            lhs, rhs = node.args
            if isa(rhs, Expr)
                rhs = virtual_eval_rhs(frame, rhs)
            else
                # rhs = istoplevel ? @lookup_type(moduleof(frame), frame, rhs) : @lookup_type(frame, rhs)
                rhs = @lookup_type(moduleof(frame), frame, rhs)
            end
            virtual_assign!(frame, lhs, rhs)
        # elseif node.head === :gotoifnot
        #     arg = @lookup_type(frame, node.args[1])
        #     if !isa(arg, Bool)
        #         throw(TypeError(nameof(frame), "if", Bool, arg))
        #     end
        #     if !arg
        #         return (frame.pc = node.args[2]::Int)
        #     end
        # elseif node.head === :enter
        #     rhs = node.args[1]
        #     push!(data.exception_frames, rhs)
        # elseif node.head === :leave
        #     for _ = 1:node.args[1]
        #         pop!(data.exception_frames)
        #     end
        # elseif node.head === :pop_exception
        #     n = lookup_var_type(frame, node.args[1])
        #     deleteat!(data.exception_frames, n+1:length(data.exception_frames))
        # elseif node.head === :return
        #     return nothing
        # elseif node.head === :method && length(node.args) > 1
        #     evaluate_methoddef(frame, node)
        # elseif node.head === :struct_type
        #     evaluate_structtype(recurse, frame, node)
        # elseif node.head === :abstract_type
        #     evaluate_abstracttype(recurse, frame, node)
        # elseif node.head === :primitive_type
        #     evaluate_primitivetype(recurse, frame, node)
        # elseif node.head === :module
        #     error("this should have been handled by split_expressions")
        # elseif node.head === :using || node.head === :import || node.head === :export
        #     Core.eval(moduleof(frame), node)
        # elseif node.head === :const
        #     g = node.args[1]
        #     if isa(g, GlobalRef)
        #         mod, name = g.module, g.name
        #     else
        #         mod, name = moduleof(frame), g::Symbol
        #     end
        #     if VERSION >= v"1.2.0-DEV.239"  # depends on https://github.com/JuliaLang/julia/pull/30893
        #         Core.eval(mod, Expr(:const, name))
        #     end
        # elseif node.head === :global
        #     Core.eval(moduleof(frame), node)
        elseif node.head === :thunk
            newframe = prepare_thunk(moduleof(frame), node)
            profile_frame!(newframe) # TODO: collect report
        # elseif node.head === :toplevel
        #     mod = moduleof(frame)
        #     modexs, _ = split_expressions(mod, node)
        #     rhs = Core.eval(mod, Expr(:toplevel, :(
        #         for modex in $modexs
        #             newframe = ($prepare_thunk)(modex)
        #             newframe === nothing && continue
        #             while true
        #                 ($through_methoddef_or_done!)($recurse, newframe) === nothing && break
        #             end
        #             $return_from(newframe)
        #         end
        #     )))
        elseif node.head === :error
            error("unexpected error statement ", node)
        elseif node.head === :incomplete
            error("unexpected incomplete statement ", node)
        else
            rhs = virtual_eval_rhs(frame, node)
        end
    # elseif isa(node, GotoNode)
    #     return (frame.pc = node.label)
    # elseif is_GotoIfNot(node)
    #     node = node::Core.GotoIfNot
    #     # arg = @lookup_type(frame, node.cond)
    #     if !isa(arg, Bool)
    #         throw(TypeError(nameof(frame), "if", Bool, arg))
    #     end
    #     if !arg
    #         return (frame.pc = node.dest)
    #     end
    elseif is_ReturnNode(node)
        return nothing
    # elseif isa(node, NewvarNode)
    #     # FIXME: undefine the slot?
    # elseif istoplevel && isa(node, LineNumberNode)
    # elseif istoplevel && isa(node, Symbol)
    #     rhs = getfield(moduleof(frame), node)
    else
        rhs = @lookup_type(frame, node)
    end

    if isassign(frame, pc)
        @isdefined(rhs) || @show frame node
        lhs = SSAValue(pc)
        virtual_assign!(frame, lhs, rhs)
    end
    return frame.pc += 1
end
