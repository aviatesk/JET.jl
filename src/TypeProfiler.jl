module TypeProfiler

# import all the bindings in JuliaInterpreter
using JuliaInterpreter
for n in names(JuliaInterpreter,; all = true)
  s = string(n)
  (startswith(s, '#') || s in ("eval", "include", "SSAValue", "SlotNumber")) && continue
  Core.eval(@__MODULE__, :(import JuliaInterpreter: $n))
end

using Core: SSAValue, SlotNumber, TypedSlot
using Core.Compiler: Const

include("types.jl")
include("utils.jl")
include("manipulate.jl")
include("interpret.jl")
include("builtins.jl")

function evaluate_or_profile!(frame::Frame, istoplevel::Bool = false)
  # type annotate `frame`
  if (s = scopeof(frame)) isa Method
    type_annotate_frame!(frame, s)
  else
    # TODO
    error("should handle toplevel execution")
  end

  # finishes this frame
  while (pc = step_code!(frame, istoplevel)) !== nothing end
  return get_return_type(frame)
end

using Base.Meta: isexpr

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

end
