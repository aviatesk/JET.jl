include("setup.jl")

@testset "JET.jl" begin
    @testset "abstractinterpret" begin
        @testset "inferenceerrorreport.jl" begin
            include("abstractinterpret/test_inferenceerrorreport.jl")
        end

        @testset "typeinfer.jl" begin
            include("abstractinterpret/test_typeinfer.jl")
        end
    end

    @testset "toplevel" begin
        @testset "virtualprocess.jl" begin
            include("toplevel/test_virtualprocess.jl")
        end
    end

    @testset "ui" begin
        # tests with Windows-paths is just an hell
        @static Sys.iswindows() || @testset "print.jl" begin
            include("ui/test_print.jl")
        end
    end

    @testset "misc" begin
        include("test_misc.jl")
    end

    @testset "Test.jl integration" begin
        include("test_Test.jl")
    end

    @testset "JETInterface" begin
        include("test_JETInterface.jl")
    end

    @testset "analyzers" begin
        @testset "JETAnalyzer" begin
            include("analyzers/test_jetanalyzer.jl")
        end

        @testset "OptAnalyzer" begin
            include("analyzers/test_optanalyzer.jl")
        end
    end

    # run simple performance benchmark
    # and also run OptAnalyzer on JET
    @testset "performance" begin
        include("performance.jl")
    end
end
