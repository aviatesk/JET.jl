function type_annotate_frame!(frame::Frame, s::Method)
  tt = signature_type(frame)
  typedsrc = typed_src(s, tt)

  # update to typed ssa statements (basically introduces `TypedSlot`s)
  replace_coretypes!(typedsrc)
  frame.framecode.src.code = typedsrc.code

  # update to typed ssavalues
  frame.framecode.src.ssavaluetypes = replaced_coretype.(typedsrc.ssavaluetypes)
  empty!(frame.framedata.ssavalues)
  for t in replaced_coretype.(typedsrc.ssavaluetypes)
    push!(frame.framedata.ssavalues, t)
  end

  # update to typed slots
  frame.framecode.src.slottypes = replaced_coretype.(typedsrc.slottypes)
end

# NOTE: maybe too fragile, make this robust
function typed_src(
  m::Method, @nospecialize(tt);
  world = Base.get_world_counter(), params = Core.Compiler.Params(world)
)::Core.CodeInfo
  xs = filter(x -> m == x[3], Base._methods_by_ftype(tt, -1, world))
  isempty(xs) && error("no method found for $(m) with $(types)")
  length(xs) !== 1 && error("multiple method found for: $m with $types")

  x = xs[1]
  m = Base.func_for_method_checked(x[3], Tuple{tt.parameters[2:end]...}, x[2])
  typedsrc, rettyp = Core.Compiler.typeinf_code(m, x[1], x[2], false, params)

  return typedsrc
end
