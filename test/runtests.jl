include("setup.jl")

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

    @testset "reports.jl" begin
        include("test_reports.jl")
    end

    # tests with Windows-paths is just an hell
    @static Sys.iswindows() || @testset "print.jl" begin
        include("test_print.jl")
    end

    @testset "misc" begin
        include("test_misc.jl")
    end

    @testset "performance" begin
        include("test_performance.jl")
    end

    @testset "interface.jl" begin
        include("test_interfaces.jl")
    end

    @testset "Test.jl integration" begin
        include("test_test.jl")
    end

    @testset "analyzers" begin
        @testset "PerformanceAnalyzer" begin
            include("analyzers/test_perfanalyzer.jl")
        end
    end
end
