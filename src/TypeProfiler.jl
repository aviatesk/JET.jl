module TypeProfiler

export profile_file, profile_text

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

end
