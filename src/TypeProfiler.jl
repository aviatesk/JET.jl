@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module TypeProfiler

include("common.jl")
include("profiler/profiler.jl")
include("virtualprocess/virtualprocess.jl")

using .Profiler, .VirtualProcess

export
    profile_file, profile_text, @profile_call

end
