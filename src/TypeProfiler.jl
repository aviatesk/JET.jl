module TypeProfiler

# export profile_file, profile_text
export @profile_call

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

macro profile_call(ex)
  @assert is_expr(ex, :call) "function call expression should be given"
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

# TODO: keyword args
function prepare_frame(f, args...)
  slottyps = Type[typeof(f), typeof.(args)...]
  tt = to_tuple_type(slottyps)
  mms = matching_methods(tt)
  @assert (n = length(mms)) === 1 "$(n === 0 ? "no" : "multiple") methods found: $tt"
  tt, sparams::SimpleVector, m::Method = mms[1]
  mi = specialize_method(m, tt, sparams)
  return Frame(mi, slottyps)
end

end
