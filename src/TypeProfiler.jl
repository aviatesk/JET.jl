module TypeProfiler

export
  # profile_file, profile_text,
  @profile_call

# TODO: using once gets stable
import Core:
  SimpleVector, svec,
  MethodInstance, CodeInfo, LineInfoNode,
  GotoNode, PiNode, PhiNode, PhiCNode, UpsilonNode, SlotNumber
import Core.Compiler:
  specialize_method, typeinf_ext,
  tmerge,
  SSAValue
import Base:
  unwrap_unionall, rewrap_unionall
import Base.Meta: isexpr

const to_tt = Base.to_tuple_type

include("types.jl")
include("utils.jl")
include("construct.jl")
include("interpret.jl")
include("builtin.jl")
include("profile.jl")
include("print.jl")

# for debugging
# -------------

# this method is basically only for testing
# TODO: keyword args
function prepare_frame(f, args...)
  slottyps = Type[typeof′(f), typeof′.(args)...]
  tt = to_tt(slottyps)
  mms = matching_methods(tt)
  @assert (n = length(mms)) === 1 "$(n === 0 ? "no" : "multiple") methods found: $tt"
  tt, sparams::SimpleVector, m::Method = mms[1]
  mi = specialize_method(m, tt, sparams)
  return prepare_frame(mi, slottyps)
end

macro profile_call(ex, kwargs...)
  @assert isexpr(ex, :call) "function call expression should be given"
  f = ex.args[1]
  args = ex.args[2:end]
  return quote let
    maybe_newframe = prepare_frame($(esc(f)), $(map(esc, args)...))
    !isa(maybe_newframe, Frame) && return maybe_newframe
    frame = maybe_newframe::Frame
    try
      evaluate_or_profile!(frame)
      print_report(frame; $(map(esc, kwargs)...))
    catch err
      rethrow(err)
    finally
      global $(esc(:frame)) = frame # HACK: assign the erroneous frame into global `frame` variable for debugging
    end
  end end
end

end
