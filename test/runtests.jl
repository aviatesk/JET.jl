# test utilities are extracted into a separate file for easier interactive testing from REPL
# i.e. julia-dev -i test/interactive_utils.jl
include("interactive_utils.jl")

# for benchmarking or debugging JET analysis
# TODO: setup proper benchmarks, separate them from the test suite
include("benchmark.jl")

# stuff used across tests
using Test

const FIXTURE_DIR = normpath(@__DIR__, "fixtures")

const ERROR_REPORTS_FROM_SUM_OVER_STRING = let
    interp, frame = profile_call(sum, (String,))
    @test !isempty(interp.reports)
    interp.reports
end

function test_sum_over_string(ers)
    @test !isempty(ers)
    for target in ERROR_REPORTS_FROM_SUM_OVER_STRING
        @test any(ers) do er
            return er.msg == target.msg && er.sig == target.sig
        end
    end
end
test_sum_over_string(res::JET.VirtualProcessResult) =
    test_sum_over_string(res.inference_error_reports)
test_sum_over_string(interp::JETInterpreter) = test_sum_over_string(interp.reports)

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

# # test self profiling
# using JET
# interp, frame = let
#     m = first(methods(JET.profile_file))
#     interp = JET.JETInterpreter()
#     JET.profile_method!(interp, m)
#
#     # JET.print_reports(stdout::IO, interp.reports)
# end
