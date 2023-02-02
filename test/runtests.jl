using Test

include("setup.jl")

@info "JET setup information:" JET.JET_DEV_MODE

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

        @testset "vscode.jl" begin
            include("ui/test_vscode.jl")
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

    @testset "self check !!!" begin
        target_modules = (JET,)
        JETAnalyzerT   = typeof(JET.JETAnalyzer())
        OptAnalyzerT   = typeof(JET.OptAnalyzer())
        InferenceState = Core.Compiler.InferenceState
        annotate_types = true

        # error analysis
        # ==============

        # JETAnalyzer
        test_call(JET.analyze_frame!, (JETAnalyzerT, InferenceState);
                  target_modules, annotate_types)
        # OptAnalyzer
        test_call(JET.analyze_frame!, (OptAnalyzerT, InferenceState);
                  target_modules, annotate_types)
        # top-level
        test_call(JET.virtual_process, (String, String, Nothing, JETAnalyzerT, JET.ToplevelConfig{Vector{Expr}});
                  target_modules, annotate_types)
        test_call(JET.virtual_process, (String, String, Base.PkgId, JETAnalyzerT, JET.ToplevelConfig{Vector{Expr}});
                  target_modules, annotate_types)
        # entries
        test_call(JET.report_file, (String,);
                  target_modules, annotate_types)
        test_call(JET.report_package, (Union{String,Module,Nothing},);
                  target_modules, annotate_types)

        # optimization analysis
        # =====================

        # ignore some dynamically-designed functions
        # TODO implement `signature_filter` and limit the ignorance scope
        function function_filter(@nospecialize ft)
            if ft === typeof(JET.widenconst) ||
               ft === typeof(JET.ignorelimited) ||
               ft === typeof(JET.print) ||
               ft === typeof(Base.CoreLogging.handle_message) ||
               ft === typeof(get) ||
               ft === typeof(sprint) ||
               ft === typeof(string) ||
               ft === typeof(zero) ||
               ft === typeof(JET.copy_report) ||
               ft === typeof(JET.handle_sig!) ||
               ft === typeof(JET._which) ||
               (@static VERSION < v"1.9.0-DEV.283" && ft === typeof(JET.rewrap_unionall)) || # requires https://github.com/JuliaLang/julia/pull/44512
               false
                return false
            end
            return true
        end
        # JETAnalyzer
        test_opt(JET.analyze_frame!, (JETAnalyzerT, InferenceState);
                 target_modules,
                 function_filter, annotate_types)
        # OptAnalyzer
        test_opt(JET.analyze_frame!, (OptAnalyzerT, InferenceState);
                 target_modules,
                 function_filter, annotate_types)
    end
end
