module TypeProfiler

# export profile_file, profile_text

using Core: SimpleVector, svec, MethodInstance, CodeInfo, LineInfoNode,
            GotoNode, PiNode, PhiNode, SlotNumber
using Core.Compiler: SSAValue, tmerge
using Base: is_expr, to_tuple_type

include("types.jl")
include("utils.jl")
include("interpret.jl")
include("profile.jl")
include("builtin.jl")

# for development
# ---------------

function method_instance(f, args...)
  typs = Tuple{typeof.(args)...}
  m = collect(methods(f, typs))[1]
  # TODO: sparmas
  ftyps = Tuple{typeof′(f), typs.parameters...}
  return Core.Compiler.specialize_method(m, ftyps, svec())
end
typeinf_mi(mi::MethodInstance) = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())

mi = method_instance(sum, "julia")
frame = Frame(mi)
step_code!(frame)
function init(f, args...)
  (@__MODULE__).eval(quote
    mi = method_instance($f, $(args...))
    frame = Frame(mi)
  end)
  nothing
end
init() = init(sum, "julia")

end
