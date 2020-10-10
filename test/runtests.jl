# test utilities are extracted into a separate file for easier testing from REPL
# i.e. julia-dev -i test/setuptest.jl
include("setuptest.jl")

@testset "TypeProfiler.jl" begin
    @testset "virtualprocess.jl" begin
        include("test_virtualprocess.jl")
    end

    @testset "abstractinterpretation.jl" begin
        include("test_abstractinterpretation.jl")
    end

    @testset "tfuncs.jl" begin
        include("test_tfuncs.jl")
    end

    @testset "tpcache.jl" begin
        include("test_tpcache.jl")
    end

    # tests with Windows-paths is just an hell
    @static Sys.iswindows() || @testset "print.jl" begin
        include("test_print.jl")
    end
end
