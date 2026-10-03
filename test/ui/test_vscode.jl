module test_vscode

include("../setup.jl")

import JET.VSCode:
    get_reports,
    PostProcessor,
    vscode_diagnostics

@testset "sources" begin
    let res = @report_call sum("julia")
        @test res.source == "BasicJETAnalyzer: sum(::String)"
    end
    let res = @report_call sum(1:100)
        @test res.source == "BasicJETAnalyzer: sum(::$(typeof(1:100)))"
    end
    let res = @report_opt sum("julia")
        @test res.source == "OptAnalyzer: sum(::String)"
    end
    let
        local filename
        res = mktemp() do path, io
            filename = path
            report_file2(filename)
        end
        @test occursin(filename, res.source)
    end
end

@testset "diagnostics" begin
    hasfield′(obj::T, sym) where T = hasfield(T, sym)

    function check_basic_integration(diagnostics, reports)
        @test hasfield′(diagnostics, :source)
        @test hasfield′(diagnostics, :items)
        @test length(diagnostics.items) == length(reports)
    end
    function check_inference_integration(item, report)
        @test hasfield′(item, :msg)
        @test hasfield′(item, :path)
        @test hasfield′(item, :line)
        @test hasfield′(item, :severity)
        @test hasfield′(item, :relatedInformation)
        @test length(item.relatedInformation) == length(report.vst)
        @test !isempty(item.relatedInformation)
        ri = first(item.relatedInformation)
        @test hasfield′(ri, :msg)
        @test hasfield′(ri, :path)
        @test hasfield′(ri, :line)
    end
    function check_toplevel_integration(item, report)
        @test hasfield′(item, :msg)
        @test hasfield′(item, :path)
        @test hasfield′(item, :line)
        @test hasfield′(item, :severity)
    end

    # basic case
    let res = @report_call sum("julia")
        reports = get_reports_with_test(res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source)
        check_basic_integration(diagnostics, reports)
        @test !isempty(diagnostics.items) && !isempty(reports)
        item = first(diagnostics.items)
        report = first(reports)
        check_inference_integration(item, report)
    end

    # no error case
    let res = @report_call sum(1:100)
        reports = get_reports_with_test(res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source)
        check_basic_integration(diagnostics, reports)
        @test isempty(diagnostics.items) && isempty(reports)
    end

    # top-level integration (from top-level error)
    let res = mktemp() do path, io
            s = quote
                macro foo()
                    throw("foo")
                end
                @foo
            end |> string
            write(path, s)
            report_file2(path)
        end
        reports = get_reports_with_test(res)
        postprocessor = PostProcessor(res.res.actual2virtual)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source;
                                         postprocessor)
        check_basic_integration(diagnostics, reports)
        @test !isempty(diagnostics.items) && !isempty(reports)
        item = first(diagnostics.items)
        report = first(reports)
        check_toplevel_integration(item, report)
    end

    # top-level integration (from inference error)
    let res = mktemp() do path, io
            s = quote
                undefvar
            end |> string
            write(path, s)
            report_file2(path)
        end
        reports = get_reports_with_test(res)
        postprocessor = PostProcessor(res.res.actual2virtual)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source;
                                         postprocessor)
        check_basic_integration(diagnostics, reports)
        @test !isempty(diagnostics.items) && !isempty(reports)
        item = first(diagnostics.items)
        report = first(reports)
        check_inference_integration(item, report)
    end
end

@testset "toplevel warning diagnostics" begin
    let res = @test_logs report_text("x = 1e-1000\n", "Untitled-warning")
        reports = get_reports(res)
        @test only(reports) isa JET.ParseWarningReport
        for warnings in (reports, res.res.toplevel_warning_reports)
            diagnostics = vscode_diagnostics(res.analyzer, warnings, res.source)
            @test diagnostics.source == res.source
            item = only(diagnostics.items)
            @test item.severity == 1
            @test item.path == "Untitled-warning"
            @test item.line == 1
            @test item.msg == sprint(JET.print_report, only(reports))
            @test !hasproperty(item, :relatedInformation)
        end
        io = IOBuffer()
        @test show(io, MIME"application/vnd.julia-vscode.diagnostics"(), res) ==
              vscode_diagnostics(res.analyzer, reports, res.source)
    end
    let res = report_text("x = 1e-1000\nundefined_warning_test", @__FILE__)
        reports = get_reports(res)
        @test length(reports) == 2
        postprocessor = PostProcessor(res.res.actual2virtual)
        diagnostics = vscode_diagnostics(res.analyzer, reports, res.source; postprocessor)
        @test diagnostics.source == res.source
        @test diagnostics.items == vcat(
            vscode_diagnostics(res.analyzer, res.res.toplevel_warning_reports,
                               res.source; postprocessor).items,
            vscode_diagnostics(res.analyzer, res.res.inference_error_reports,
                               res.source; postprocessor).items)
        warning, inference = diagnostics.items
        @test warning.severity == inference.severity == 1
        @test warning.path == @__FILE__
        @test warning.line == 1
        @test inference.line == 2
        @test !isempty(inference.relatedInformation)
        reversed = vscode_diagnostics(res.analyzer, reverse(reports), res.source; postprocessor)
        @test reversed.items == reverse(diagnostics.items)
    end
    let res = report_text("")
        report = JET.UnsupportedFeatureReport("$(@__MODULE__).unsupported", @__FILE__, 7)
        postprocessor = PostProcessor(Main => @__MODULE__)
        diagnostics = vscode_diagnostics(res.analyzer, [report], "warning source"; postprocessor)
        @test diagnostics.source == "warning source"
        @test only(diagnostics.items) ==
              (; msg="unsupported", path=@__FILE__, line=7, severity=1)
    end
    let res = report_text("""
            x = 1e-1000
            @eval error("fatal warning test")
            """)
        @test !isempty(res.res.toplevel_warning_reports)
        reports = get_reports(res)
        @test only(reports) isa ToplevelErrorReport
        diagnostics = vscode_diagnostics(res.analyzer, reports, res.source)
        @test only(diagnostics.items).severity == 0
        @test occursin("fatal warning test", only(diagnostics.items).msg)
    end
end

end # module test_vscode
