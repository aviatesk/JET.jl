module TypeProfiler

# export profile_file, profile_text
export @profile_call

using Core: SimpleVector, svec, MethodInstance, CodeInfo, LineInfoNode,
            GotoNode, PiNode, PhiNode, SlotNumber
using Core.Compiler: SSAValue, tmerge, specialize_method, typeinf_ext
using Base: unwrap_unionall, rewrap_unionall
using Base.Meta: isexpr

const to_tt = Base.to_tuple_type

include("types.jl")
include("utils.jl")
include("construct.jl")
include("interpret.jl")
include("builtin.jl")
include("profile.jl")
include("print.jl")

macro profile_call(ex)
  @assert isexpr(ex, :call) "function call expression should be given"
  f = ex.args[1]
  args = ex.args[2:end]
  quote
    let
      frame = prepare_frame($f, $(args...))
      evaluate_or_profile!(frame)
      print_report(frame)
    end
  end
end

end
