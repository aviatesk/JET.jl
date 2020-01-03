"""
    struct ProfiledType
        type::Type
    end
    const PT = ProfiledType

The wrapper type for "type-profiled" variables. An actual type will be kept in `type` field.
"""
struct ProfiledType
  type::Type
end
const PT = ProfiledType

"""
    struct Undefined end

A Singleton type that represents when the type profiler failed to track a type.
It should be reported when and where this is introduced, and then any further
  profiling for things including this type won't be executed.
"""
struct Undefined end

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
  ssavaluetypes::Vector{PT}
  # slottypes::Vector{PT}
  rettyp::Union{Nothing,PT} # initialized with nothing
  #= frame chain =#
  caller::Union{_FrameChain{Frame},Nothing}
  callee::Union{_FrameChain{Frame},Nothing}
end

const FrameChain = _FrameChain{Frame}

function Frame(scope, src::CodeInfo, caller = nothing)
  ssavaluetypes = Vector{PT}(undef, length(src.ssavaluetypes))
  nstmts = length(ssavaluetypes)
  return Frame(scope, src, nstmts, false, false, 1, ssavaluetypes, nothing, caller, nothing)
end
function Frame(mi::MethodInstance)
  scope = mi.def::Method
  src = typed_code(mi)
  return Frame(scope, src)
end
function Frame(parentframe::Frame, mi::MethodInstance)
  scope = mi.def::Method
  src = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())
  lin = lineinfonode(parentframe)
  caller = FrameChain(lin, parentframe)
  return Frame(scope, src, caller)
end

typed_code(mi::MethodInstance) = Core.Compiler.typeinf_ext(mi, Base.get_world_counter())

# Profiling
# ---------

abstract type Report end

struct ConditionErrorReport <: Report
  frame::Frame
  lin::LineInfoNode
  typ::Type
end
