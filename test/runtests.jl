# test utilities are extracted into a separate file for easier interactive testing from REPL
# i.e. julia-dev -i test/interactive_utils.jl
include("interactive_utils.jl")

# for benchmarking or debugging JET analysis on CI
if get(ENV, "CI", nothing) == "true"
    include("debug.jl")
end

@testset "JET.jl" begin
    @testset "virtualprocess.jl" begin
        include("test_virtualprocess.jl")
    end

    @testset "abstractinterpretation.jl" begin
        include("test_abstractinterpretation.jl")
    end

    @testset "tfuncs.jl" begin
        include("test_tfuncs.jl")
    end

    @testset "jetcache.jl" begin
        include("test_jetcache.jl")
    end

    # tests with Windows-paths is just an hell
    @static Sys.iswindows() || @testset "print.jl" begin
        include("test_print.jl")
    end
end
