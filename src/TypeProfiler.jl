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

# for deverlopment
macro profile_call(ex, kwargs...)
  @assert isexpr(ex, :call) "function call expression should be given"
  f = ex.args[1]
  args = ex.args[2:end]
  quote
    let
      maybe_newframe = prepare_frame($(esc(f)), $(map(esc, args)...))
      !isa(maybe_newframe, Frame) && return maybe_newframe
      frame = maybe_newframe::Frame
      try
        evaluate_or_profile!(frame)
        print_report(frame; $(map(esc, kwargs)...))
        @show rettyp(frame)
      catch err
        rethrow(err) # rely on fancy err printing by some frontend
      finally
        global $(esc(:frame)) = frame # HACK: assing the result into `frame`
      end
    end
  end
end

end
