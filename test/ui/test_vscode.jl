import JET.VSCode:
    get_critical_reports,
    gen_postprocess,
    vscode_diagnostics

@testset "sources" begin
    let res = @report_call sum("julia")
        @test res.source == "@report_call sum(::String)"
    end
    let res = @report_call sum(1:100)
        @test res.source == "@report_call sum(::$(typeof(1:100)))"
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
        reports = get_reports(res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source,
                                         )
        check_basic_integration(diagnostics, reports)
        @test !isempty(diagnostics.items) && !isempty(reports)
        item = first(diagnostics.items)
        report = first(reports)
        check_inference_integration(item, report)
    end

    # no error case
    let res = @report_call sum(1:100)
        reports = get_reports(res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source,
                                         )
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
        reports = get_critical_reports(res.res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source,
                                         gen_postprocess(res.res.actual2virtual),
                                         )
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
        reports = get_critical_reports(res.res)
        diagnostics = vscode_diagnostics(res.analyzer,
                                         reports,
                                         res.source,
                                         gen_postprocess(res.res.actual2virtual),
                                         )
        check_basic_integration(diagnostics, reports)
        @test !isempty(diagnostics.items) && !isempty(reports)
        item = first(diagnostics.items)
        report = first(reports)
        check_inference_integration(item, report)
    end
end
