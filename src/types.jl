# Profilng
# --------

# There are mainly two kinds of types that TypeProfiler interprets:
# - T is usual Julia types
# - Unknown is introduced when TypeProfiler finds something _errorable_,
#   or can't evaluate its type (mostly because of unimplemented features)

"""
    const Unknown = Union{}

A special type that TypeProfiler introduces when it finds something _errorable_ or
  can't determine its type (mostly because of unimplmented features).
After introduing this type, TypeProfiler still continues profiling, but any futher
  profiling for things including this type will be skipped.

!!! note
    For consistency with Julia's internal, this type is just a type alias for `Union{}`,
      which is the singleton instance of [`Core.TypeofBottom`](@ref) type.
"""
const Unknown = Union{}

"""
    struct ProfiledType
        type::Type
    end

Wrapper type for "type-profiled" (top-level) variables. The actual profiled type
  will be kept in `type` field.
"""
struct ProfiledType
  type::Type
end

# Frame
# -----

# NOTE:
# once https://github.com/JuliaLang/julia/issues/269 has been resolved, we can
# remove this weird parametric type and just use mutally circular type

struct _FrameChain{T}
  lin::LineInfoNode
  frame::T
end

mutable struct Frame
  #= frame info =#
  scope::Union{Method,Module}
  # sparams::Vector{Any}
  src::CodeInfo
  nstmts::Int
  generator::Bool # TODO
  istoplevel::Bool # TODO
  #= profiling state =#
  pc::Int
  ssavaluetypes::Vector{Type}
  # slottypes::Vector{Type}
  rettyp::Union{Nothing,Type} # initialized with nothing
  #= frame chain =#
  caller::Union{_FrameChain{Frame},Nothing}
  callee::Union{_FrameChain{Frame},Nothing}
end

const FrameChain = _FrameChain{Frame}

function Frame(scope, src::CodeInfo, caller = nothing)
  ssavaluetypes = Vector{Type}(undef, length(src.ssavaluetypes))
  nstmts = length(ssavaluetypes)
  return Frame(scope, src, nstmts, false, false, 1, ssavaluetypes, nothing, caller, nothing)
end
function Frame(mi::MethodInstance)
  scope = mi.def::Method
  src = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())
  return Frame(scope, src)
end
function Frame(parentframe::Frame, mi::MethodInstance)
  scope = mi.def::Method
  src = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())
  lin = lineinfonode(parentframe)
  caller = FrameChain(lin, parentframe)
  return Frame(scope, src, caller)
end

# Report
# ------

abstract type ErrorReport end

function (reporttyp::Type{<:ErrorReport})(frame::Frame, args...)
  lin = lineinfonode(frame)
  reporttyp(frame, lin, args...)
end

struct ConditionErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  profiled::Type
end

struct ArgumentNumberErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  f::Function
  expected::Int
  profiled::Int
end

struct ArgumentTypeErrorReport <: ErrorReport
  frame::Frame
  lin::LineNumberNode
  f::Function
  expected::Type
  profiled::Type
end

function Base.show(io::IO, report::T) where {T<:ErrorReport}
  fs = filter(n -> n ∉ (:frame, :lin), fieldnames(T))
  cs = [getfield(report, f) for f in fs]
  c = join(string.(cs), ' ')
  s = string(typeof(report).name.name, '(', c, ')',  " in ", scopeof(report.frame))
  println(io, s)
end
