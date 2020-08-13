@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module TypeProfiler

include("common.jl")
include("profiler/profiler.jl")
include("virtualmachine/virtualmachine.jl")

using ..Profiler, ..VirtualMachine

export
    profile_file, profile_text, @profile_call

end
