using Test

const outmsg, errmsg = let
    local outmsg::String, errmsg::String
    mktemp() do outpath, stdout
        mktemp() do errpath, stderr
            redirect_stdio(; stdout, stderr) do
                @eval using JET
            end
            flush(stderr)
            errmsg = read(errpath, String)
        end
        flush(stdout)
        outmsg = read(outpath, String)
    end
    outmsg, errmsg
end

isempty(outmsg) || println(outmsg)
isempty(errmsg) || println(errmsg)

@info "JET setup information:" JET.JET_DEV_MODE

@testset "JET.jl" begin
    @static if JET.JET_LOADABLE
        @testset "abstractinterpret" begin
            @testset "inferenceerrorreport.jl" include("abstractinterpret/test_inferenceerrorreport.jl")

            @testset "typeinfer.jl" include("abstractinterpret/test_typeinfer.jl")
        end

        @testset "toplevel" begin
            @testset "virtualprocess.jl" include("toplevel/test_virtualprocess.jl")
        end

        @testset "ui" begin
            # tests with Windows-paths is just an hell
            @static Sys.iswindows() || @testset "print.jl" include("ui/test_print.jl")

            @testset "vscode.jl" include("ui/test_vscode.jl")
        end

        @testset "misc" include("test_misc.jl")

        @testset "Test.jl integration" include("test_Test.jl")

        @testset "JETInterface" include("test_JETInterface.jl")

        @testset "analyzers" begin
            @testset "JETAnalyzer" include("analyzers/test_jetanalyzer.jl")

            @testset "OptAnalyzer" include("analyzers/test_optanalyzer.jl")
        end

        @testset "performance" include("performance.jl")

        @testset "sanity check" include("sanity_check.jl")

        # @testset "self check" include("self_check.jl")

        # @testset "extensions" begin
        #     include("ext/test_cthulhu.jl")
        # end
    else
        @info "JET was not loaded fully, switching to run the tests for JETEmpty.jl."
        @testset "test_JETEmpty.jl" include("test_JETEmpty.jl")
    end
end
