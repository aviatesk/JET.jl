module TypeProfiler

using JuliaInterpreter

include("types.jl")
include("utils.jl")
include("typeannotate.jl")
include("interpreter.jl")
include("builtins.jl")

function evaluate_or_profile!(frame::Frame, istoplevel::Bool = false)
  # type annotate `frame.framcode.src` !
  (m = scopeof(frame)) isa Method && type_annotate_framecode!(frame, m)
  while (pc = step_code!(frame, istoplevel)) !== nothing end
  return get_return(frame)
end

end
