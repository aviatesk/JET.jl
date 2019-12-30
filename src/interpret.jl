# lookups
# -------

"""
    rhs = @lookup_type(frame, node)
    rhs = @lookup_type(mod, frame, node)

This macro looks up previously-computed types referenced as SSAValues, SlotNumbers,
GlobalRefs, QuoteNode, sparam or exception reference expression.
It will also lookup symbols in `moduleof(frame)`; this can be supplied ahead-of-time
via the 3-argument version.
If none of the above apply, the value of `node` will be returned.
"""
macro lookup_type(args...)
  length(args) === 2 || length(args) === 3 || error("invalid number of arguments ", length(args))
  havemod = length(args) === 3
  local mod
  if havemod
    mod, frame, node = args
  else
    frame, node = args
  end
  nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
  fallback = havemod ?
    :($nodetmp isa Symbol ? typeof′(getfield($(esc(mod)), $nodetmp)) : typeof′($nodetmp)) :
    :(typeof′($nodetmp))

  quote
    $nodetmp = $(esc(node))
    isa($nodetmp, SSAValue) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, SlotNumber) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, Const) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, TypedSlot) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, QuoteNode) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, Expr) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, GlobalRef) ? lookup_type($(esc(frame)), $nodetmp) :
    isa($nodetmp, Symbol) ? lookup_type($(esc(frame)), $nodetmp) :
    $fallback
  end
end

# TODO: fallback to pre-computed types
lookup_type(frame::Frame, ssav::SSAValue) =
  strip_const(frame.framecode.src.ssavaluetypes[ssav.id])
lookup_type(frame::Frame, slot::SlotNumber) = begin
  return typ = strip_const(frame.framecode.src.slottypes[slot.id])
  # typ !== Any && return typ
  # throw("can't determine slot type: $(frame.framecode.src.slotnames[slot.id])")
end
lookup_type(frame::Frame, c::Const) = strip_const(c)
lookup_type(frame::Frame, tslot::TypedSlot) = strip_const(tslot.typ)
lookup_type(frame::Frame, node::QuoteNode) = typeof′(node.value)
function lookup_type(frame::Frame, e::Expr)
  head = e.head
  if head === :the_exception
    @error "exceptions are not supported"
    return Undefined
  elseif head === :static_parameter
    arg = e.args[1]::Int
    if isassigned(frame.framedata.sparams, arg)
      return Type{frame.framedata.sparams[arg]}
    else
      syms = sparam_syms(frame.framecode.scope)
      throw(UndefVarError(syms[arg]))
    end
  elseif head === :boundscheck
    if length(e.args) === 0
      return Bool
    else
      error("invalid boundscheck at ", e)
    end
  else
    error("invalid lookup expr ", e)
  end
end
lookup_type(frame::Frame, ref::GlobalRef) = typeof′(getfield(ref.mod, ref.name))
function lookup_type(frame::Frame, sym::Symbol)
  mod = moduleof(frame)
  isdefined(mod, sym) || return Undefiend
  return typeof′(getfield(mod, sym))
end

# recursive call
# --------------

step_code!(frame, istoplevel::Bool) = step_code!(frame, pc_expr(frame), istoplevel)
function step_code!(frame, @nospecialize(node), istoplevel::Bool)
  pc, code, data = frame.pc, frame.framecode, frame.framedata
  if !is_leaf(frame)
    show_stackloc(frame)
    @show node
  end
  @assert is_leaf(frame)
  local rhs

  try
    if isa(node, Expr)
      if node.head == :(=)
        lhs, rhs = node.args
        if isa(rhs, Expr)
          rhs = evaluate_or_profile_code!(frame, rhs)
        else
          rhs = if istoplevel
            @lookup_type(moduleof(frame), frame, rhs)
          else
            @lookup_type(frame, rhs)
          end
        end
        do_assignment!(frame, lhs, rhs)
      elseif node.head == :gotoifnot
        # NOTE: just check the branch node type, and ignore jump itself
        arg = @lookup_type(frame, node.args[1])
        if arg !== Bool
          throw(TypeError(nameof(frame), "if", Bool, node.args[1]))
        end
      # TODO: handle exception
      # elseif node.head == :enter
      #   rhs = node.args[1]
      #   push!(data.exception_frames, rhs)
      # elseif node.head == :leave
      #   for _ = 1:node.args[1]
      #     pop!(data.exception_frames)
      #   end
      # elseif node.head == :pop_exception
      #   n = lookup_var(frame, node.args[1])
      #   deleteat!(data.exception_frames, n+1:length(data.exception_frames))
      elseif node.head == :return
        return nothing
      # TODO: toplevel executions
      # elseif istoplevel
      #   if node.head == :method && length(node.args) > 1
      #     evaluate_methoddef(frame, node)
      #   elseif node.head == :struct_type
      #     evaluate_structtype(iot, frame, node)
      #   elseif node.head == :abstract_type
      #     evaluate_abstracttype(iot, frame, node)
      #   elseif node.head == :primitive_type
      #     evaluate_primitivetype(iot, frame, node)
      #   elseif node.head == :module
      #     error("this should have been handled by split_expressions")
      #   elseif node.head == :using ||
      #          node.head == :import || node.head == :export
      #     Core.eval(moduleof(frame), node)
      #   elseif node.head == :const
      #     g = node.args[1]
      #     if isa(g, GlobalRef)
      #       mod, name = g.module, g.name
      #     else
      #       mod, name = moduleof(frame), g::Symbol
      #     end
      #     if VERSION >= v"1.2.0-DEV.239"  # depends on https://github.com/JuliaLang/julia/pull/30893
      #       Core.eval(mod, Expr(:const, name))
      #     end
      #   elseif node.head == :thunk
      #     newframe = prepare_thunk(moduleof(frame), node)
      #     if isa(iot, Compiled)
      #       finish!(iot, newframe, true)
      #     else
      #       newframe.caller = frame
      #       frame.callee = newframe
      #       finish!(iot, newframe, true)
      #       frame.callee = nothing
      #     end
      #     return_from(newframe)
      #   elseif node.head == :global
      #               # error("fixme")
      #   elseif node.head == :toplevel
      #     mod = moduleof(frame)
      #     modexs, _ = split_expressions(mod, node)
      #     Core.eval(
      #       mod,
      #       Expr(
      #         :toplevel,
      #         :(
      #           for modex in $modexs
      #             newframe = ($prepare_thunk)(modex)
      #             newframe === nothing && continue
      #             while true
      #               ($through_methoddef_or_done!)($iot, newframe) ===
      #               nothing && break
      #             end
      #             $return_from(newframe)
      #           end
      #         ),
      #       ),
      #     )
      #   elseif node.head == :error
      #     error("unexpected error statement ", node)
      #   elseif node.head == :incomplete
      #     error("incomplete statement ", node)
      #   else
      #     rhs = eval_rhs(iot, frame, node)
      #   end
      elseif node.head == :thunk || node.head == :toplevel
        error("this frame needs to be run at top level")
      else
        rhs = evaluate_or_profile_code!(frame, node)
      end
    elseif isa(node, Core.GotoNode) # NOTE: ignore GotoNode
    elseif isa(node, Core.NewvarNode)
      # FIXME: undefine the slot?
    elseif istoplevel && isa(node, Core.LineNumberNode)
    elseif istoplevel && isa(node, Symbol)
      # TODO: handle variables that the type profiler creates
      rhs = getfield(moduleof(frame), node)
    else
      rhs = @lookup_type(frame, node)
    end
  catch err
    return handle_err(finish_and_return!, frame, err)
  end

  @isdefined(rhs) && isa(rhs, JuliaInterpreter.BreakpointRef) && return rhs
  if isassign(frame, pc)
    @isdefined(rhs) || error("rhs not defined: $(frame) $(node)")
    lhs = SSAValue(pc)
    do_assignment!(frame, lhs, rhs)
  end
  return (frame.pc = pc + 1)
end

function evaluate_or_profile_code!(frame, node::Expr)
  head = node.head
  # if head == :new
  #   mod = moduleof(frame)
  #   rhs = ccall(:jl_new_struct_uninit, Any, (Any,), @lookup(mod, frame, node.args[1]))
  #   for i = 1:length(node.args)-1
  #     ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), rhs, i - 1, @lookup(mod, frame, node.args[i+1]))
  #   end
  #   return rhs
  # elseif head == :splatnew  # Julia 1.2+
  #   mod = moduleof(frame)
  #   rhs = ccall(:jl_new_structt, Any, (Any, Any), @lookup(mod, frame, node.args[1]), @lookup(mod, frame, node.args[2]))
  #   return rhs
  # elseif head == :isdefined
  #   return check_isdefined(frame, node.args[1])
  # elseif head == :call
  if head === :call
    return profile_call(frame, node)
  # elseif head == :foreigncall || head == :cfunction
  #   return evaluate_foreigncall(frame, node)
  # elseif head == :copyast
  #   val = (node.args[1]::QuoteNode).value
  #   return isa(val, Expr) ? copy(val) : val
  # elseif head == :enter
  #   return length(frame.framedata.exception_frames)
  # elseif head == :boundscheck
  #   return true
  # elseif head == :meta || head == :inbounds || head ==
  #        (@static VERSION >= v"1.2.0-DEV.462" ? :loopinfo : :simdloop) ||
  #        head == :gc_preserve_begin || head == :gc_preserve_end
  #   return nothing
  # elseif head == :method && length(node.args) == 1
  #   return evaluate_methoddef(frame, node)
  # end
  else
    @error "called: $(node)"
    return lookup_type(frame, node)
  end
end

function profile_call(
  frame::Frame, call_expr::Expr;
  enter_generated::Bool = false
)
  # TODO: I may need this ?
  # ret = bypass_builtins(frame, call_expr, pc)
  # isa(ret, Some{Type}) && return ret.value

  ret = maybe_profile_builtin_call(frame, call_expr, true)
  if ret isa SomeType
    rettyp = unwrap_sometype(ret)
    @show call_expr, ret
    return rettyp
  end

  call_expr = ret
  argtypes = collect_argtypes(frame, call_expr)
  f = to_function(argtypes[1])

  if f === Core.eval
    # NOTE: maybe can't handle this
    @warn "TypeProfiler can't profile `Core.eval`"
    return Undefined
  elseif f === Base.rethrow
    @warn "TypeProfiler currently doesn't handle exceptions."
    return Undefined
  elseif f === Core.invoke # invoke needs special handling
    # TODO: handle this
    error("encounter Core.invoke")
    # f_invoked = which(fargs[2], fargs[3])
    # fargs_pruned = [fargs[2]; fargs[4:end]]
    # sig = Tuple{_Typeof.(fargs_pruned)...}
    # ret = prepare_framecode(f_invoked, sig; enter_generated = enter_generated)
    # isa(ret, Compiled) && invoke(fargs[2:end]...)
    # framecode, lenv = ret
    # lenv === nothing && return framecode  # this was a Builtin
    # fargs = fargs_pruned
  else
    framecode, lenv = get_call_framecode(argtypes, frame.framecode, frame.pc; enter_generated = enter_generated)
    if lenv === nothing
      if isa(framecode, Compiled)
        @warn "Hit Compiled method: $(argtypes)"
        return Undefined
      else
        # return framecode  # this was a Builtin
        error("builtin ?")
      end
    end
  end

  newframe = prepare_frame_caller(frame, framecode, argtypes, lenv)
  npc = newframe.pc
  rettyp = evaluate_or_profile!(newframe, false)
  frame.callee = nothing
  return_from(newframe)
  @show rettyp, scopeof(newframe)
  return rettyp
end

# HACK:
# - overload `_Typeof` so that it would "unwrap" `SomeType`
# - wrap in `SomeType` so that `prepare_hoge` works as if with actual values
function collect_argtypes(frame::Frame, call_expr::Expr; isfc::Bool = false)
  args = frame.framedata.callargs
  resize!(args, length(call_expr.args))
  mod = moduleof(frame)
  # TODO: :foreigncall should be handled separatelly
  for (i, arg) in enumerate(call_expr.args)
    args[i] = SomeType(@lookup_type(mod, frame, arg))
  end
  return args
end

_Typeof(t::SomeType) = t.type
to_function(t::SomeType) = to_function(t.type)
to_function(t::Type{<:Function}) = t.instance
to_function(t::Type{T}) where {T} = t
