using Base.Meta: isexpr

# entry point
# -----------

function profile_file(filename::AbstractString; mod::Module = Main)
  isfile(filename) || error("No such file exists: $filename")
  filetext = read(filename, String)
  profile_text(filetext; filename = filename, mod = Main)
end
function profile_text(text::String; filename::AbstractString = "none", mod::Module = Main)
  exs = Base.parse_input_line(text; filename = filename)
  for i = 2:2:length(exs.args)
    frame = prepare_thunk(mod, exs.args[i])
    evaluate_or_profile!(frame, true)
  end
end

function evaluate_or_profile!(frame::Frame, istoplevel::Bool = false)
  # type annotate `frame`
  (s = scopeof(frame)) isa Method && type_annotate_frame!(frame, s)

  # finishes this frame
  while (pc = step_code!(frame, istoplevel)) !== nothing end
  return get_return_type(frame)
end

function get_return_type(frame)
  node = pc_expr(frame)
  if isexpr(node, :return)
    lookup_type(frame, (node::Expr).args[1])
  elseif node isa Const && isexpr(node.val, :return)
    lookup_type(frame, (node.val::Expr).args[1])
  else
    error("expected return statement, got ", node)
  end
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
            lookup_type(frame, rhs)
          else
            lookup_type(frame, rhs)
          end
        end
        do_assignment′!(frame, lhs, rhs)
      elseif node.head == :gotoifnot
        # NOTE: just check the branch node type, and ignore jump itself
        arg = lookup_type(frame, node.args[1])
        if arg !== Bool
          throw(TypeError(nameof(frame), "if", Bool, node.args[1]))
        end
      # TODO: handle exception ?
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
    elseif isa(node, Const)
      return step_code!(frame, node.val, istoplevel)
    elseif isa(node, Core.GotoNode) # NOTE: ignore GotoNode
    elseif isa(node, Core.NewvarNode)
      # FIXME: undefine the slot?
    elseif istoplevel && isa(node, Core.LineNumberNode)
    else
      rhs = lookup_type(frame, node)
    end
  catch err
    return handle_err(finish_and_return!, frame, err)
  end

  @isdefined(rhs) && isa(rhs, JuliaInterpreter.BreakpointRef) && return rhs
  if isassign(frame, pc)
    @isdefined(rhs) || error("rhs not defined: $(frame) $(node)")
    lhs = SSAValue(pc)
    do_assignment′!(frame, lhs, rhs)
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
  elseif head == :meta || head == :inbounds || head ==
         (@static VERSION >= v"1.2.0-DEV.462" ? :loopinfo : :simdloop) ||
         head == :gc_preserve_begin || head == :gc_preserve_end
    return Nothing
  # elseif head == :method && length(node.args) == 1
  #   return evaluate_methoddef(frame, node)
  # end
  else
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
  if ret isa ProfiledType
    call_arg_types = collect_call_arg_types(frame, call_expr)
    rettyp = unwrap_ProfiledType(ret)
    @show call_arg_types, rettyp
    return rettyp
  end

  call_arg_types = ret
  f_type = call_arg_types[1]
  arg_types = call_arg_types[2:end]
  f = to_function(f_type) # non-builtin function *can* be identified from its type
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
    return Undefined
    # f_invoked = which(fargs[2], fargs[3])
    # fargs_pruned = [fargs[2]; fargs[4:end]]
    # sig = Tuple{_Typeof.(fargs_pruned)...}
    # ret = prepare_framecode(f_invoked, sig; enter_generated = enter_generated)
    # isa(ret, Compiled) && invoke(fargs[2:end]...)
    # framecode, lenv = ret
    # lenv === nothing && return framecode  # this was a Builtin
    # fargs = fargs_pruned
  end

  # HACK: wrap in `ProfiledType` so that `prepare_hoge` works as if with actual values
  call_arg_types_wrapped = Any[ProfiledType(t) for t in call_arg_types]
  framecode, lenv = get_call_framecode(
    call_arg_types_wrapped,
    frame.framecode,
    frame.pc;
    enter_generated = enter_generated
  )
  if lenv === nothing
    if isa(framecode, Compiled)
      @warn "Hit Compiled method: $(arg_types)"
      return Undefined
    else
      error("builtin ?")  # this was a Builtin
      return Undefined
    end
  end

  newframe = prepare_frame_caller(frame, framecode, call_arg_types_wrapped, lenv)
  rettyp = evaluate_or_profile!(newframe, false)
  frame.callee = nothing
  return_from(newframe)

  @show scopeof(newframe), rettyp
  return rettyp
end

# HACK:
# - overload `_Typeof` so that it would "unwrap" `ProfiledType`
# - overload `to_function` so that it would identify a function from its type
_Typeof(t::ProfiledType) = t.type
to_function(t::ProfiledType) = to_function(t.type)
to_function(t::Type{<:Function}) = t.instance
to_function(t::Type{Core.IntrinsicFunction}) =
  error("to_function can't identify intrinsic functions. Use IntrinsicFunctionType wrapper type instead.")
to_function(t::IntrinsicFunctionType) = t.f
to_function(t::Type{T}) where {T} = T
to_function(t::Type{Type{T}}) where {T} = t.parameters[1]

do_assignment′!(frame::Frame, ssav::SSAValue, @nospecialize(rhs)) = frame.framedata.ssavalues[ssav.id] = rhs
function do_assignment′!(frame::Frame, slot::SlotNumber, @nospecialize(rhs))
  frame.framedata.locals[slot.id] = rhs
  frame.framedata.last_reference[slot.id] = (frame.assignment_counter += 1)
end
do_assignment′!(frame::Frame, gr::GlobalRef, @nospecialize(rhs)) =
  Core.eval(gr.mod, :($(gr.name) = $(ProfiledType(rhs))))
do_assignment′!(frame::Frame, sym::Symbol, @nospecialize(rhs)) =
  Core.eval(moduleof(frame), :($sym = $(ProfiledType(rhs))))
