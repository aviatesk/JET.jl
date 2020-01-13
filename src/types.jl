# Profilng
# --------

# There are mainly two kinds of types that TypeProfiler interprets:
# - T is usual Julia types
# - Unknown is introduced when TypeProfiler finds something _errorable_,
#   or can't evaluate its type (mostly because of unimplemented features)

"""
    struct Unknown end

A special type that TypeProfiler introduces when it finds something _errorable_ or
  can't determine its type (mostly because of unimplmented features).
After introduing this type, TypeProfiler still continues profiling, but any futher
  profiling for things including this type will be skipped.

!!! note
    This type plays an equivalent role to [`Union{}`](@ref) in the Julia's internal,
      but also indicates TypeProfiler itself really founds errros.
"""
struct Unknown end

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
  #= history =#
  profiled::Dict{UInt64,Type} # for avoiding to enter recursive calls
  #= frame info =#
  scope::Union{MethodInstance,Module}
  src::CodeInfo
  slottypes::Vector{Any} # MethodInstance.specTypes seems to be not always true
  sparams::Vector{Any}
  nstmts::Int
  generator::Bool # TODO
  istoplevel::Bool # TODO
  #= profiling state =#
  pc::Int
  ssavaluetypes::Vector{Type}
  rettyp::Type # intialized with Union{} as in the inference
  #= frame chain =#
  caller::Union{_FrameChain{Frame},Nothing}
  callee::Union{_FrameChain{Frame},Nothing}
end

const FrameChain = _FrameChain{Frame}

function Frame(
  scope::Union{MethodInstance,Module}, src::CodeInfo, slottypes::Vector, sparams::Vector,
  caller::Union{Nothing,FrameChain} = nothing;
  generator::Bool = false, istoplevel::Bool = false,
)
  reports = caller !== nothing ? caller.frame.reports : ErrorReport[]
  profiled = if caller !== nothing
    caller.frame.profiled
  else
    if scope isa Module
      Dict{UInt64,Type}()
    else
      Dict{UInt64,Type}(hash(scope::MethodInstance) => src.rettype)
    end
  end
  ssavaluetypes = Vector{Type}(undef, length(src.ssavaluetypes))
  nstmts = length(ssavaluetypes)
  return Frame(
    reports,
    profiled,
    scope, src, slottypes, sparams, nstmts, generator, istoplevel,
    1, ssavaluetypes, Union{},
    caller, nothing,
  )
end

function Base.show(io::IO, frame::Frame)
  scope = scopeof(frame)
  if scope isa MethodInstance
    printstyled(io, "Frame of ", scope; bold = true)
    m = scope.def::Method
    print(io, " in ", m.module, " at ", m.file, ':', m.line)
  else
    printstyled(io, "Frame in ", scope; bold = true)
  end
end
function Base.show(io::IO, ::MIME"text/plain", frame::Frame)
  hascaller = frame.caller !== nothing
  hascallee = frame.callee !== nothing

  show(io, frame); println(io)
  println(io, "├─ pc: ", frame.pc, "/", frame.nstmts)
  println(io, "├─ code:")
  s = max(1,frame.pc-2)
  e = min(length(frame.src.code),frame.pc+2)
  for i in s:e
    stmt = frame.src.code[i]
    i === e ? print(io, "│  └─ ") : print(io, "│  ├─ ")
    if i === frame.pc
      printstyled(io, stmt; bold = true)
    else
      print(io, stmt)
      if isassigned(frame.ssavaluetypes, i)
        printstyled(io, "::", frame.ssavaluetypes[i]; color = :cyan)
      else
        printstyled(io, "::", "Unprofiled"; color = :light_black)
      end
    end
    println(io)
  end
  if (hascaller || hascallee)
    print(io, "├─ ret"); printstyled(io, "::", frame.rettyp, '\n'; color = :cyan)
  else
    print(io, "└─ ret"); printstyled(io, "::", frame.rettyp, '\n'; color = :cyan)
  end

  if hascaller
    if hascallee
      println(io, "├─ caller: ", frame.caller.frame)
    else
      println(io, "└─ caller: ", frame.caller.frame)
    end
  end
  if hascallee
    println(io, "└─ callee: ", frame.callee.frame)
  end

  if (c = length(frame.reports)) > 0
    println(io, "Reports: ", c)
    for (i, r) in enumerate(frame.reports)
      i === c ? print(io, "└─ ") : print(io, "├─ ")
      println(io, r)
    end
  end
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

struct UndefVarErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  mod::Union{Module,Method}
  name::Symbol
  maybe::Bool
end

struct InvalidBuiltinCallErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  tt::Type
end

struct NoMethodErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  tt::Type
end

struct ConditionErrorReport <: ErrorReport
  frame::Frame
  lin::LineInfoNode
  t::Type
end

@specialize

function Base.show(io::IO, report::T) where {T<:ErrorReport}
  fs = filter(n -> n ∉ (:frame, :lin), fieldnames(T))
  cs = [getfield(report, f) for f in fs]
  c = join(string.(cs), ", ")
  s = string(typeof(report).name.name, '(', c, ')')
  print(io, s, " [", report.frame, ']')
end

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
    @assert isexpr(call_ex, :call) && begin
      reporttyp = call_ex.args[1]
      endswith(string(reporttyp), "ErrorReport")
    end "invalid call form: $call_ex"
    args = call_ex.args[2:end]
  else
    reporttyp = exs[1]
    @assert endswith(string(reporttyp), "ErrorReport") "invalid argument form: $args"
    args = exs[2:end]
  end

  return quote
    lin = lineinfonode($(esc(frame)))
    report = $reporttyp($(esc(frame)), lin, $(map(esc, args)...))
    report!($(esc(frame)), report)
    return Unknown
  end
end
report!(frame::Frame, report::ErrorReport) = push!(frame.reports, report)
