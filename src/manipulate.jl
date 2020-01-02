function type_annotate_frame!(frame::Frame, s::Method)
  tt = signature_type(frame, s)
  typedsrc = typed_src(s, tt)

  # update to typed ssa statements
  frame.framecode.src.code = typedsrc.code

  # update to typed ssavalues
  ssavaluetypes::Vector{Any} = typedsrc.ssavaluetypes
  frame.framecode.src.ssavaluetypes = ssavaluetypes

  # update to typed slots
  frame.framecode.src.slottypes = typedsrc.slottypes
end

# extract call arg types from `FrameData.locals` (wrapped in `Some`)
# TODO: handle kwargs
function signature_type(frame::Frame, s::Method)
  sig = s.sig
  while sig isa UnionAll; sig = sig.body; end
  call_args = filter(!isnothing, frame.framedata.locals)
  call_arg_types = map(zip(sig.parameters, call_args)) do (s, call_arg)
    if s isa UnionAll && s.body isa DataType && s.body.name.name === :Vararg
      tpl = typeof′(call_arg isa Some ? call_arg.value : call_arg)::Type{<:Tuple}
      return Vararg{typejoin(tpl.types...), length(tpl.parameters)}
    else
      return typeof′(call_arg isa Some ? call_arg.value : call_arg)
    end
  end
  return Tuple{call_arg_types...}
end

# NOTE: maybe too fragile, make this robust
function typed_src(
  m::Method, @nospecialize(tt);
  world = Base.get_world_counter(), params = Core.Compiler.Params(world)
)::Core.CodeInfo
  xs = filter(x -> m == x[3], Base._methods_by_ftype(tt, -1, world))
  isempty(xs) && error("no method found for $(m) with $(tt)")
  length(xs) !== 1 && error("multiple method found for: $m with $types")

  x = xs[1]
  m = Base.func_for_method_checked(x[3], Tuple{tt.parameters[2:end]...}, x[2])
  typedsrc, rettyp = Core.Compiler.typeinf_code(m, x[1], x[2], false, params)

  return typedsrc
end
