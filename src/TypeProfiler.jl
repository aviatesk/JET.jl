module TypeProfiler

# export profile_file, profile_text

using Core: SimpleVector, svec, MethodInstance, CodeInfo, LineInfoNode,
            GotoNode, PiNode, PhiNode, SlotNumber
using Core.Compiler: SSAValue, tmerge, specialize_method, typeinf_ext
using Base: is_expr, to_tuple_type, unwrap_unionall, rewrap_unionall

include("types.jl")
include("utils.jl")
include("interpret.jl")
include("profile.jl")
include("builtin.jl")
include("print.jl")

# for development
# ---------------

function method_instance(f, args...)
  tt = Tuple{typeof(f), typeof.(args)...}
  mms = matching_methods(tt)
  @assert (n = length(mms)) === 1 "$(n === 0 ? "no" : "multiple") methods found: $tt"

  tt, sparams::SimpleVector, m::Method = mms[1]
  return specialize_method(m, tt, sparams)
end
typeinf_mi(mi::MethodInstance) = typeinf_ext(mi, Base.get_world_counter())

function init(f, args...)
  (@__MODULE__).eval(quote
    mi = method_instance($f, $(args)...)
    frame = Frame(mi, [typeof($f), typeof.($args)...])
  end)
end
(init() = init(sum, "julia"))()
function summer(A::AbstractVector{T}) where {T}
  s = zero(T)
  for a in A s += a end
  return s
end

end
