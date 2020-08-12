@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module TypeProfiler

using Core:
    MethodInstance

using Core.Compiler:
    InferenceState, isconstType

using Base:
    Meta.isexpr, to_tuple_type

include("errorreport.jl")
include("profiler/profiler.jl")
include("virtualmachine/virtualmachine.jl")
include("print.jl")

using ..Profiler

macro profile_call(ex, kwargs...)
    @assert isexpr(ex, :call) "function call expression should be given"
    f = ex.args[1]
    args = ex.args[2:end]

    quote let
        interp, frame = $(profile_call)($(esc(f)), $(map(esc, args)...))
        $(print_reports)(interp; $(map(esc, kwargs)...))
        $(get_rettyp)(frame)
    end end
end

@nospecialize

function profile_call(f, args...)
    tt = to_tuple_type(typeof′.([f, args...]))
    return profile_call_gf(tt)
end

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

get_rettyp(frame::InferenceState) = frame.result.result

export
    @profile_call

end
