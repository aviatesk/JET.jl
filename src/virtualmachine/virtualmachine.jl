# TODO:
# - macro expansion:
#   TP will try to expand any macro, but since macros can access to any global objects
#   and so any macro expansion including access to global values will fail as far as TP
#   avoids evaluation of them but just profiles/preserves their types.
module VirtualMachine

import Base:
    Meta, Meta.isexpr, to_tuple_type

import Core:
    CodeInfo, SimpleVector, LineInfoNode, GotoNode, Slot, GeneratedFunctionStub,
    MethodInstance, NewvarNode, TypeName,
    # JuliaInterpreter.jl defines their own replacements of these types, and TP may also
    # need equivalents
    SSAValue, SlotNumber,
    # TP-specific
    Builtin

import Core.Compiler:
    builtin_tfunction

using ..TypeProfiler
using ..TypeProfiler.Profiler

# # VirtualProcess -> VirtualFrame
# struct VirtualProcess
#
# end
#
# # virtual toplevel frame
# struct VirtualFrame
#     mod::Module
#     src::CodeInfo
#     reports::Vector{ErrorReport}
# end
#
# abstract type ToplevelErrorReport <: ErrorReport end

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
  will be kept in `profiled` field.
"""
struct ProfiledType
    profiled::Type
    ProfiledType(@nospecialize(t::Type)) = new(t)
end

typeof′(@nospecialize(v)) = typeof(v)
typeof′(@nospecialize(x::Type{T})) where {T} = Type{T}
typeof′(pt::ProfiledType) = pt.profiled

# TODO: remove this
struct Frame
    frame::Dict{Symbol,Any}
end
function Base.getproperty(frame::Frame, sym::Symbol)
    sym === :frame && return invoke(getproperty, Tuple{Any,Symbol}, frame, sym)
    return frame.frame[sym]
end
function Base.setproperty!(frame::Frame, sym::Symbol, @nospecialize(v))
    sym === :frame && return invoke(setproperty!, Tuple{Any,Symbol,Any}, frame, sym, v)
    frame.frame[sym] = v
end
Base.propertynames(frame::Frame) = keys(frame.frame)

# NOTE:
# files below were originally adapted from JuliaInterpreter.jl (https://github.com/JuliaDebug/JuliaInterpreter.jl/tree/e42045c39f6363aa5035da5c320587b5264ca644/src),
# but there are notable changes:
# - works only on type-level, i.e. most executions would be done virtually
# - supposed to work only on toplevel, since any profiling on a specific virtual method call will be done by `TypepProfiler.Profiler` module, which borrows Julia's native type inference logic
# - any parts related to breakpoints are removed
# - any JuliaInterpreter-specific optimizations are removed

include("utils.jl")
include("construct.jl")
include("interpret.jl")

function profile_frame!(frame::Frame)
    nstmts = length(frame.codeinfo.code)
    while true
        pc = step_expr!(frame)
        (isnothing(pc) || pc > nstmts) && break
    end
end

# function profile_expr!(mod::Module, expr::Expr)
#     modexs, _ = split_expressions(mod, expr)
#     rhs = Core.eval(mod, Expr(:toplevel, :(
#         for modex in $(modexs)
#             frame = $(prepare_thunk)(modex)
#             frame === nothing && continue
#             while true
#                 $(through_methoddef_or_done!)($recurse, frame) === nothing && break
#             end
#             $(return_from)(frame)
#         end
#     )))
# end

# export
#     profile_code,
#     profile_text,
#     profile_file,
#     profile_module

end  # module VirtualMachine
