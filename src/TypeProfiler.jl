module TypeProfiler

# import all the bindings in JuliaInterpreter
using JuliaInterpreter
for n in names(JuliaInterpreter,; all = true)
  s = string(n)
  (startswith(s, '#') || s in ("eval", "include")) && continue
  Core.eval(@__MODULE__, :(import JuliaInterpreter: $n))
end

include("types.jl")
include("utils.jl")
include("manipulate.jl")
include("interpret.jl")
include("builtins.jl")

function __init__()
  Core.eval(JuliaInterpreter, :(function replace_coretypes_list!(list; rev::Bool = false)
    for (i, stmt) in enumerate(list)
      if stmt isa Expr
        replace_coretypes!(stmt; rev = rev)
      else
        list[i] = rev ? $replaced_coretype_rev(stmt) : $replaced_coretype(stmt)
      end
    end
  end))
end

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
  return get_return(frame)
end

end
