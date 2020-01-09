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

# NOTE: need to be here so that Frame can refer to this type
abstract type ErrorReport end

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
  #= reports =#
  reports::Vector{ErrorReport} # will be referenced from leaf frames
  #= frame info =#
  scope::Union{Method,Module}
  src::CodeInfo
  slottypes::Vector{Any} # MethodInstance.specTypes seems to be not always true
  sparams::Vector{Any}
  nstmts::Int
  generator::Bool # TODO
  istoplevel::Bool # TODO
  #= profiling state =#
  pc::Int
  ssavaluetypes::Vector{Type}
  rettyp::Union{Nothing,Type} # initialized with nothing
  #= frame chain =#
  caller::Union{_FrameChain{Frame},Nothing}
  callee::Union{_FrameChain{Frame},Nothing}
end

const FrameChain = _FrameChain{Frame}

function Frame(
  scope::Union{Method,Module}, src::CodeInfo, slottypes::Vector, sparams::Vector,
  caller::Union{Nothing,FrameChain} = nothing;
  generator::Bool = false, istoplevel::Bool = false,
)
  reports = caller !== nothing ? caller.frame.reports : ErrorReport[]
  ssavaluetypes = Vector{Type}(undef, length(src.ssavaluetypes))
  nstmts = length(ssavaluetypes)
  return Frame(
    reports,
    scope, src, slottypes, sparams, nstmts, generator, istoplevel,
    1, ssavaluetypes, nothing,
    caller, nothing,
  )
end

function Frame(mi::MethodInstance, slottypes::Vector, parentframe::Union{Nothing,Frame} = nothing)
  scope = mi.def::Method
  src = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())
  sparams = collect(mi.sparam_vals)
  caller = parentframe === nothing ? nothing : begin
    lin = lineinfonode(parentframe)
    FrameChain(lin, parentframe)
  end
  return Frame(scope, src, slottypes, sparams, caller)
end

# Report
# ------

"""
    abstract type ErrorReport end

Abstract type of all the error reports that TypeProfiler states.
Each concrete report type should have the two mandatory fields below:

1. `frame::Frame`: a current frame in which this error report is profiled
2. `lin::LineInfoNode`:
    - specifies _where_ this error report can occur
    - should be obtained with `lineinfonode(frame::Frame)`

`frame`, `lin` fields should be the 1st and 2nd field, respectively.

!!! note
    There is an utility macro [`@report!`](@ref) for adding an error report to
      its frame.

See also: [`lineinfonode`](@ref), [`@report!`](@ref)
"""
ErrorReport

@nospecialize

struct MethodErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  args::Type
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
  lin::LineInfoNode
  f::Function
  expected::Type
  profiled::Type
end

struct ConditionErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  profiled::Type
end

@specialize

"""
    @report!(frame, reporttyp, args...)
    @report!(frame, reportcall)

Adds a report to `frame.reports` and then `return`s `Unknown` type.
The reports is supposed to be specified with the following way:

- `@report!(frame, reporttyp, args...)`
- `@report!(frame, reporttyp(args...))`

where `reporttyp` specifies the [`ErrorReport`](@ref) type itself and `args...`
  are its _third_ and subsequent fields.
"""
macro report!(frame, exs...)
  if length(exs) === 1
    call_ex = exs[1]
    @assert is_expr(call_ex, :call) && begin
      reporttyp = call_ex.args[1]
      endswith(string(reporttyp), "ErrorReport")
    end "invalid call: $call_ex"
    args = call_ex.args[2:end]
  else
    reporttyp = exs[1]
    @assert endswith(string(reporttyp), "ErrorReport") "invalid argument form: $args"
    args = exs[2:end]
  end

  return quote
    lin = lineinfonode($(esc(frame)))
    report = $reporttyp($(esc(frame)), lin, $(esc.(args)...))
    report!($(esc(frame)), report)
    return Unknown
  end
end
report!(frame::Frame, report::ErrorReport) = push!(frame.reports, report)

function Base.show(io::IO, report::T) where {T<:ErrorReport}
  fs = filter(n -> n ∉ (:frame, :lin), fieldnames(T))
  cs = [getfield(report, f) for f in fs]
  c = join(string.(cs), ", ")
  s = string(typeof(report).name.name, '(', c, ')',  " in ", scopeof(report.frame))
  println(io, s)
end
