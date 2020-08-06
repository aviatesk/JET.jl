module TypeProfiler

include("profiler/profiler.jl")

using Base:
    Meta.isexpr

macro profile_call(ex)
    @assert isexpr(ex, :call) "function call expression should be given"
    f = ex.args[1]
    args = ex.args[2:end]

    quote let
        interp, frame = Profiler.profile_call($(esc(f)), $(map(esc, args)...))
        Profiler.print_reports(interp, frame)
        frame.result.result
    end end
end

export
    @profile_call

end
