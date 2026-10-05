module test_print

include("../setup.jl")

function result_string(result)
    buf = IOBuffer()
    show(buf, result)
    return String(take!(buf))
end

@testset "print toplevel errors" begin
    for (src, msg) in (("""
            a = begin
                b =
            end
            """, "invalid identifier"),
            ("begin\n    a = 1\n", "Expected `end`"))
        for filename in (@__FILE__, "foo")
            io = IOBuffer()
            res = report_text(src, filename)
            print_reports(io, ToplevelErrorReport[res.res.toplevel_error_report])
            s = String(take!(io))
            @test occursin("Top-level analysis failed", s)
            @test occursin("@ $filename:$(res.res.toplevel_error_report.line)", s)
            @test occursin(msg, s)
        end
    end
end

@testset "fatal analysis status" begin
    for report in (
            ActualErrorWrapped(ErrorException("execution failed"),
                Base.StackTraces.StackFrame[], "example.jl", 7),
            MacroExpansionErrorReport(ErrorException("expansion failed"),
                Base.StackTraces.StackFrame[], "example.jl", 7),
            LoweringErrorReport("lowering failed", "example.jl", 7),
            JET.ConcretizationTimeoutErrorReport(0.1,
                Base.StackTraces.StackFrame[], "example.jl", 7))
        io = IOBuffer()
        @test print_reports(io, ToplevelErrorReport[report]) == 1
        s = String(take!(io))
        @test startswith(s, """
            ═════ Top-level analysis failed ═════
            JET stopped before completing the analysis.
            Fix the error below and rerun the analysis.
            ┌ @ ./example.jl:7""")
        lines = split(s, '\n'; keepempty=false)
        @test textwidth(last(lines)) == maximum(textwidth, lines[5:end-1])
        @test !occursin("1 toplevel error found", s)
        @test !occursin("No errors detected", s)
    end
    @test isempty(sprint(print_reports, ToplevelErrorReport[]))
end

@testset "print toplevel warnings" begin
    @test isempty(sprint(print_reports, JET.ToplevelWarningReport[]))
    @test isempty(@test_deprecated r"print_toplevel_success" sprint(io ->
        print_reports(io, JET.ToplevelWarningReport[]; print_toplevel_success=true)))
    let report = JET.UnsupportedFeatureReport("unsupported test feature", @__FILE__, 7)
        reports = JET.ToplevelWarningReport[report]
        for sourceinfo in (:default, :full, :compact, :minimal, :none)
            io = IOBuffer()
            @test print_reports(io, reports; sourceinfo) == 1
            s = String(take!(io))
            filename = sourceinfo in (:compact, :minimal, :none) ? basename(@__FILE__) : @__FILE__
            @test occursin("1 toplevel warning found", s)
            @test occursin("@ $filename:7", s)
            @test occursin("unsupported test feature", s)
            @test !occursin("error", s)
        end
        s = sprint(print_reports, reports; context=:color=>true)
        rail = sprint(io -> printstyled(io, "│ "; color=:yellow); context=:color=>true)
        @test occursin(rail, s)
        s = sprint(print_reports, JET.ToplevelWarningReport[report, report])
        @test occursin("2 toplevel warnings found", s)
        lines = split(s, '\n'; keepempty=false)
        bottoms = findall(startswith("└"), lines)
        @test length(bottoms) == 2
        @test all(textwidth(lines[bottom]) == maximum(textwidth, lines[top+1:bottom-1])
                  for (top, bottom) in zip(findall(startswith("┌"), lines), bottoms))
    end
    # control sequences, such as hyperlinks in colored parser diagnostics, take no width
    let res = @test_logs report_text("x = 1e-1000\n", @__FILE__)
        s = sprint(show, res; context=:color=>true)
        @test occursin("\e]8;;", s)
        lines = split(replace(s, JET.CONTROL_SEQUENCE => ""), '\n'; keepempty=false)
        @test textwidth(last(lines)) == maximum(textwidth, lines[3:end-1])
    end
    let res = @test_logs report_text("x = 1e-1000\n", @__FILE__)
        reports = get_reports(res)
        @test only(reports) isa JET.ParseWarningReport
        s = result_string(res)
        @test occursin("1 toplevel warning found", s)
        @test occursin("@ $(@__FILE__):1", s)
        @test !occursin("No errors", s)
        @test !occursin("possible error", s)
        @test !occursin("Top-level analysis failed", s)
        @test !occursin("rerun the analysis", s)
        @test s == sprint(print_reports, res.res.toplevel_warning_reports)
    end
    let res = @test_logs report_text("x = 1e-1000\nundefined_warning_test", @__FILE__)
        reports = get_reports(res)
        @test length(reports) == 2
        @test first(reports) isa JET.ParseWarningReport
        @test last(reports) isa InferenceErrorReport
        postprocessor = JET.PostProcessor(res.res.actual2virtual)
        s = result_string(res)
        @test occursin("1 toplevel warning found", s)
        @test occursin("1 possible error found", s)
        @test occursin("undefined_warning_test", s)
        @test !occursin("No errors", s)
        @test s == sprint(print_reports, res.res.toplevel_warning_reports, postprocessor) *
                   sprint(print_reports, res.res.inference_error_reports, postprocessor)
        io = IOBuffer()
        @test print_reports(io, reports, postprocessor) == 2
    end
    let report = JET.UnsupportedFeatureReport("$(@__MODULE__).unsupported", "warning.jl", 2)
        postprocessor = JET.PostProcessor(Main => @__MODULE__)
        reports = Union{JET.ToplevelWarningReport,InferenceErrorReport}[report]
        s = sprint(print_reports, reports, postprocessor)
        @test occursin("│ unsupported", s)
        @test !occursin(string(@__MODULE__), s)
        @test !occursin("No errors", s)
    end
    let res = report_text("""
            x = 1e-1000
            @eval error("fatal warning test")
            """)
        @test !isempty(res.res.toplevel_warning_reports)
        @test only(get_reports(res)) isa ToplevelErrorReport
        s = result_string(res)
        @test occursin("Top-level analysis failed", s)
        @test occursin("fatal warning test", s)
        @test !occursin("toplevel warning", s)
    end
end

@testset "actual error stacktrace rendering" begin
    @testset "with stacktrace" for err in (
            ErrorException("execution failed"),
            LoadError("example.jl", 1, ErrorException("execution failed")),
            InitError(:Example, ErrorException("execution failed")))
        st = [Base.StackTraces.StackFrame(:example, Symbol("example.jl"), 1)]
        report = ActualErrorWrapped(err, st, "example.jl", 1)
        for color in (false, true)
            # the first line of the error message joins the summary
            firstline, rest = split(sprint(showerror, err, st; context=:color=>color), '\n'; limit=2)
            @test endswith(firstline, "execution failed")
            @test startswith(rest, "Stacktrace:")
            summary = "JET could not execute this top-level code: $firstline\n\n"
            @test sprint(JET.print_report, report; context=:color=>color) == summary * rest
            @test sprint(JET.print_report, report; context=(:color=>color, :markdown_rendering=>false)) == summary * rest
            msg_md = sprint(JET.print_report, report; context=(:color=>color, :markdown_rendering=>true))
            @test msg_md == summary * "```\n" * rest * "\n```\n"
        end
    end
    @testset "without stacktrace" begin
        err = ErrorException("execution failed")
        report = ActualErrorWrapped(err, Base.StackTraces.StackFrame[], "example.jl", 1)
        msg = "JET could not execute this top-level code: execution failed"
        @test sprint(JET.print_report, report) == msg
        @test sprint(JET.print_report, report; context=:markdown_rendering=>true) == msg
        @test !occursin("```", msg)
    end
end

@testset "concretization timeout guidance" for with_stacktrace in (false, true),
                                               markdown_rendering in (false, true)
    st = with_stacktrace ?
        Base.StackTraces.StackFrame[
            Base.StackTraces.StackFrame(:example, Symbol("example.jl"), 1)
        ] : Base.StackTraces.StackFrame[]
    report = JET.ConcretizationTimeoutErrorReport(0.1, st, "example.jl", 1)
    msg = report_message(report; context=:markdown_rendering=>markdown_rendering)
    @test occursin("possibly due to interpretation overhead", msg)
    @test occursin("raise `concretization_timeout`", msg)
    for guidance in (
            "If the stacktrace shows code that normally finishes quickly",
            "Add a `concretization_patterns` entry",
            "matching the enclosing top-level block",
            "run its function calls natively",
            "entire matching block, including any side effects",
            "`concretization_timeout` cannot interrupt those native calls")
        @test occursin(guidance, msg) == with_stacktrace
    end
    @test occursin("Stacktrace:", msg) == with_stacktrace
    @test occursin("```", msg) == (with_stacktrace && markdown_rendering)
end

@testset "markdown rendering of preformatted text" begin
    @testset "macro expansion and lowering errors" begin
        st = [Base.StackTraces.StackFrame(:example, Symbol("example.jl"), 1)]
        err = ErrorException("transformation failed")
        for (report, context) in (
                (MacroExpansionErrorReport(err, st, "example.jl", 1),
                 "JET could not expand a macro in this code"),
                (LoweringErrorReport(err, "example.jl", 1, st), "JET could not lower this code"))
            msg = sprint(JET.print_report, report)
            @test startswith(msg, "$context: transformation failed\n\nStacktrace:")
            msg_md = sprint(JET.print_report, report; context=:markdown_rendering=>true)
            @test msg_md == replace(msg, "\n\nStacktrace:"=>"\n\n```\nStacktrace:"; count=1) * "\n```\n"
        end
        let report = LoweringErrorReport("invalid assignment location", "example.jl", 1)
            @test sprint(JET.print_report, report) == "Syntax error: invalid assignment location"
        end
    end
    @testset "syntax diagnostics" begin
        error_report = report_text("x = (1 +\n", "example.jl").res.toplevel_error_report
        @test error_report isa ParseErrorReport
        warning_report = only(report_text("x = 1e-1000\n", "example.jl").res.toplevel_warning_reports)
        @test warning_report isa JET.ParseWarningReport
        for report in (error_report, warning_report)
            diagnostic = sprint(JS.show_diagnostic, report.diagnostic, report.source)
            @test startswith(diagnostic, "# ") # a Markdown heading outside of code blocks
            summary, body = split(sprint(JET.print_report, report), "\n\n"; limit=2)
            level = report isa ParseErrorReport ? "error" : "warning"
            @test summary == "Syntax $level: $(report.diagnostic.message)"
            @test body == diagnostic
            @test sprint(JET.print_report, report; context=:markdown_rendering=>true) ==
                "$summary\n\n```\n$diagnostic\n```\n"
        end
    end
    @testset "backticks in code blocks" begin
        @test sprint(JET.print_markdown_codeblock, "`a`") == "```\n`a`\n```\n"
        @test sprint(JET.print_markdown_codeblock, "a\n```\nb") == "````\na\n```\nb\n````\n"
    end
end

@testset "recursive include rendering" begin
    report = RecursiveIncludeErrorReport("/dir/a.jl", ["/dir/a.jl", "/dir/b.jl"], "/dir/b.jl", 1)
    for context in (:color=>true, :markdown_rendering=>true)
        @test sprint(JET.print_report, report; context) == """
            Recursive `include` detected: `/dir/a.jl` is already being included.

            Include chain:
            - `/dir/a.jl`
            - `/dir/b.jl`
            - `/dir/a.jl`"""
    end
end

@testset "message wrapping" begin
    let text = join(fill("word", 40), ' ') * " `a code span that is not broken`"
        s = sprint(io -> JET.println_wrapped(io, text; prefix="- "))
        lines = split(chomp(s), '\n')
        @test length(lines) > 1
        @test all(line -> textwidth(line) ≤ JET.MESSAGE_WIDTH, lines)
        @test all(line -> startswith(line, "  "), lines[2:end])
        @test occursin("`a code span that is not broken`", s)
        @test join(split(s), ' ') == "- " * text
    end
    let code = "`" * 'x'^JET.MESSAGE_WIDTH * "`"
        @test sprint(JET.print_wrapped, "a $code b") == "a\n$code\nb"
    end
    @testset "literal whitespace in code spans" begin
        @test JET.message_words("  path  `/dir/あ  b/file.jl`  is missing  ") ==
            ["path", "`/dir/あ  b/file.jl`", "is", "missing"]
        @test JET.message_words("use `  a   b  `, then `c  d`.") ==
            ["use", "`  a   b  `,", "then", "`c  d`."]
        file = "/dir/あ  b/file.jl"
        report = RecursiveIncludeErrorReport(file, [file], "example.jl", 1)
        for cols in (40, 120), summary_line in (false, true)
            msg = sprint(JET.print_report, report;
                         context=(:displaysize=>(24, cols), :summary_line=>summary_line))
            summary, body = split(msg, "\n\n"; limit=2)
            @test occursin("`$file`", summary)
            @test body == "Include chain:\n- `$file`\n- `$file`"
        end
    end
    # "aaa bb cc ddddd" at width 6: greedy wrapping would leave "cc" alone on a line
    @test JET.wrap_lines([3, 2, 2, 5], 6) == [1:1, 2:3, 4:4]
    @test isempty(JET.wrap_lines(Int[], 6))
    @test sprint(io -> JET.print_wrapped(io, ""; prefix="- ")) == "- "
    # the wrapping adapts to interpolated values such as paths
    let file = "/" * join(fill("long_directory_name", 3), '/') * "/config.jl"
        assignment = JET.ToplevelAssignment(nothing, file, 3)
        report = MissingConcretizationErrorReport(false, GlobalRef(Main, :RandomType),
            assignment, "example.jl", 7)
        lines = split(sprint(JET.print_report, report), '\n')
        @test all(line -> textwidth(line) ≤ JET.MESSAGE_WIDTH, lines)
    end
    let text = join(fill("word", 40), ' ') # 199 columns
        wrapped(cols) =
            split(sprint(JET.print_wrapped, text; context=:displaysize=>(24, cols)), '\n')
        @test maximum(textwidth, wrapped(60)) ≤ 60
        # wider displays do not get lines wider than `MESSAGE_WIDTH`
        @test wrapped(200) == wrapped(JET.MESSAGE_WIDTH) == split(sprint(JET.print_wrapped, text), '\n')
        @test 10 < maximum(textwidth, wrapped(10)) ≤ 40
    end
    # the report box passes the display width without its rail to the message
    let report = JET.ConcretizationTimeoutErrorReport(0.1, Base.StackTraces.StackFrame[],
                                                      "example.jl", 7)
        function paragraph_width(cols)
            s = sprint(print_reports, ToplevelErrorReport[report]; context=:displaysize=>(24, cols))
            lines = split(s, '\n'; keepempty=false)
            paragraph = findfirst(startswith("│ JET executes"), lines)::Int
            return maximum(textwidth, lines[paragraph:end-1]) # excluding the box bottom
        end
        @test paragraph_width(60) ≤ 60
        @test paragraph_width(150) ≤ JET.MESSAGE_WIDTH + 2
    end
    let report = JET.UnsupportedFeatureReport(join(fill("word", 40), ' '), "example.jl", 7)
        lines = split(sprint(JET.print_report, report; context=:displaysize=>(24, 50)), '\n')
        @test length(lines) > 5
        @test all(line -> textwidth(line) ≤ 50, lines)
    end
    let report = JET.DependencyError("MyPkg", "SomeDep", "example.jl", 7)
        msg = sprint(JET.print_report, report; context=:displaysize=>(24, 50))
        _, body = split(msg, "\n\n"; limit=2)
        @test all(line -> textwidth(line) ≤ 50, split(msg, '\n'))
        @test all(line -> startswith(line, r"- |  \S"), split(body, '\n'))
        # the wording is synced with `Base.require`
        @test report_message(report) ==
            "Package MyPkg does not have SomeDep in its dependencies. - You may have a " *
            "partially installed environment. Try `Pkg.instantiate()` to ensure all " *
            "packages in the environment are installed. - Or, if you have MyPkg checked " *
            "out for development and have added SomeDep as a dependency but haven't " *
            "updated your primary environment's manifest file, try `Pkg.resolve()`. - " *
            "Otherwise you may need to report an issue with MyPkg"
    end
end

@testset "report summaries" begin
    st = [Base.StackTraces.StackFrame(:example, Symbol("example.jl"), 1)]
    err = ErrorException("execution failed")
    reports = Any[
        report_text("x = (1 +\n", "example.jl").res.toplevel_error_report,
        only(report_text("x = 1e-1000\n", "example.jl").res.toplevel_warning_reports),
        JET.UnsupportedFeatureReport("unsupported test feature", "example.jl", 1),
        MacroExpansionErrorReport(err, st, "example.jl", 1),
        LoweringErrorReport(err, "example.jl", 1, st),
        LoweringErrorReport("lowering failed", "example.jl", 1),
        ActualErrorWrapped(err, st, "example.jl", 1),
        DependencyError("MyPkg", "SomeDep", "example.jl", 1),
        RecursiveIncludeErrorReport("/dir/a.jl", ["/dir/a.jl", "/dir/b.jl"], "/dir/b.jl", 1),
        JET.ConcretizationTimeoutErrorReport(0.1, st, "example.jl", 1),
        MissingConcretizationErrorReport(false, GlobalRef(Main, :RandomType), nothing,
                                         "example.jl", 1)]
    for report in reports
        wrapped = sprint(JET.print_report, report; context=:displaysize=>(24, 40))
        oneline = sprint(JET.print_report, report;
                         context=(:displaysize=>(24, 40), :summary_line=>true))
        # the summary is followed by a blank line and the body if any, and only the
        # wrapping of the summary differs
        summary, body... = split(oneline, "\n\n"; limit=2)
        wrapped_summary, wrapped_body... = split(wrapped, "\n\n"; limit=2)
        @test !occursin('\n', summary)
        @test join(split(wrapped_summary), ' ') == summary
        @test all(line -> textwidth(line) ≤ 40, split(wrapped_summary, '\n'))
        @test wrapped_body == body
    end
    # an error message that does not fit is put on its own line instead of being wrapped
    let err = ErrorException(join(fill("word", 20), ' ')) # 99 columns
        report = ActualErrorWrapped(err, Base.StackTraces.StackFrame[], "example.jl", 1)
        @test sprint(JET.print_report, report; context=:displaysize=>(24, 60)) ==
            "JET could not execute this top-level code:\n" * err.msg
        @test sprint(JET.print_report, report;
                     context=(:displaysize=>(24, 60), :summary_line=>true)) ==
            "JET could not execute this top-level code: " * err.msg
        @test sprint(JET.print_report, report; context=:displaysize=>(24, 150)) ==
            "JET could not execute this top-level code:\n" * err.msg
    end
    @testset "literal whitespace" for err in (
            ErrorException("expected \"a  b\""),
            ArgumentError("invalid value: \"a  b\""))
        st = Base.StackTraces.StackFrame[]
        report = ActualErrorWrapped(err, st, "example.jl", 1)
        message = sprint(showerror, err, st)
        context = "JET could not execute this top-level code:"
        for cols in (50, 120), summary_line in (false, true)
            separator = summary_line || cols == 120 ? " " : "\n"
            @test sprint(JET.print_report, report;
                         context=(:displaysize=>(24, cols), :summary_line=>summary_line)) ==
                context * separator * message
        end
    end
end

@testset "print inference errors" begin
    mktemp() do filename, io
        res = report_text("""
            global s::String = "julia"
            sum(s)
        """, filename)
        io = IOBuffer()
        @test !iszero(print_reports(io, res.res.inference_error_reports))
        let s = String(take!(io))
            @test occursin("2 possible errors found", s)
            @test occursin("$(escape_string(filename)):2", s) # toplevel call site
        end
    end

    mktemp() do filename, io
        res = report_text("""
            foo(args...) = args_typo # typo
            foo(rand(Char, 1000000000)...)
        """, filename)
        io = IOBuffer()
        @test !iszero(print_reports(io, res.res.inference_error_reports, JET.PostProcessor(res.res.actual2virtual)))
        let s = String(take!(io))
            @test occursin("1 possible error found", s)
            @test occursin("$(escape_string(filename)):1", s) # toplevel call site
        end
    end

    # each line closing report boxes is as wide as the error message above it, and the last
    # one closes all remaining boxes
    let lines = split(sprint(show, report_call(sum, (String,))), '\n'; keepempty=false)
        closings = findall(l -> occursin(r"^│*└┴*─+$", l), lines)
        @test any(i -> startswith(lines[i], "│") && occursin('┴', lines[i]), closings)
        @test all(i -> textwidth(lines[i]) == textwidth(lines[i-1]), closings)
        @test startswith(last(lines), "└┴")
    end
end

@testset "deprecated success configurations" begin
    @test isempty(@test_deprecated r"print_toplevel_success" sprint(io ->
        print_reports(io, ToplevelErrorReport[]; print_toplevel_success=true)))
    @test isempty(@test_deprecated r"print_inference_success" sprint(io ->
        print_reports(io, InferenceErrorReport[]; print_inference_success=false)))
    @test (@test_deprecated r"print_inference_success" sprint(io ->
        print_reports(io, InferenceErrorReport[]; print_inference_success=true))) ==
        "No errors detected\n"
    @test sprint(print_reports, InferenceErrorReport[]) == "No errors detected\n"
end

@testset "invalid constant declaration messages" begin
    let res = @analyze_toplevel begin
            x = 1
            const x = 2
        end
        report = only(res.res.inference_error_reports)
        @test report isa InvalidConstantDeclarationReport
        @test occursin("cannot declare `$(report.var.mod).x` constant; it was already declared global", get_msg(report))
    end
    let res = @analyze_toplevel begin
            module Exporter
                const x = 1
            end
            import .Exporter: x
            const x = 2
        end
        report = only(res.res.inference_error_reports)
        @test report isa InvalidConstantDeclarationReport
        @test occursin("cannot declare `$(report.var.mod).x` constant; it was already declared as an import", get_msg(report))
    end
end

@testset "repr" begin
    let result = report_call((Regex,)) do r
            getfield(r, :nonexist)
        end
        @test occursin(":nonexist", result_string(result))
    end

    let result = report_call() do
            sin("julia")
        end
        @test occursin("sin(\"julia\")", result_string(result))
    end
end

test_print_callf(f, a) = f(a)
@testset "simplified global references" begin
    # exported names should not be canonicalized
    let result = @report_call sum("julia")
        s = result_string(result)
        @test occursin("+", s)
        @test !occursin("Base.:+", s)
        @test occursin("zero", s)
        @test !occursin("Base.zero", s)
    end

    # `Main.`-prefix should be omitted
    let result = report_call() do
            sin("42")
        end
        s = result_string(result)
        @test occursin("sin", s)
        @test !occursin(r"(Main|Base)\.sin", s)
    end
    let result = report_call() do
            test_print_callf(sin, "42")
        end
        s = result_string(result)
        @test occursin("test_print_callf", s)
        @test !occursin(r"(Main|Base)\.test_print_callf", s)
    end
end

struct StackTraceTypeLimited{g}
    num::g
end;
function func_stacktrace_types_limit(x::StackTraceTypeLimited)
    if x.num isa StackTraceTypeLimited
        return func_stacktrace_types_limit(x.num)
    end
    return x.num + 1
end

@testset "Depth-limited type printing" begin
    Typ = Any
    for i = 1:10
        Typ = StackTraceTypeLimited{Typ}
    end
    STTL_str = repr(StackTraceTypeLimited)

    result = report_opt() do x::Typ
        func_stacktrace_types_limit(x) # runtime dispatch!
    end
    @test occursin("$STTL_str{…}", result_string(result))
    @test !occursin(repr(Typ), result_string(result))

    result = report_opt(;stacktrace_types_limit=0) do x::Typ
        func_stacktrace_types_limit(x) # runtime dispatch!
    end
    @test !occursin("$STTL_str{…}", result_string(result))
    @test occursin(repr(Typ), result_string(result))

    result = report_opt(;stacktrace_types_limit=1000) do x::Typ
        func_stacktrace_types_limit(x) # runtime dispatch!
    end
    @test !occursin("$STTL_str{…}", result_string(result))
    @test occursin(repr(Typ), result_string(result))

    result = report_opt(;stacktrace_types_limit=3) do x::Typ
        func_stacktrace_types_limit(x) # runtime dispatch!
    end
    @test occursin("$STTL_str{$STTL_str{$STTL_str{…}}}", result_string(result))
    @test !occursin(repr(Typ), result_string(result))
end

@testset "sourceinfo" begin
    let result = report_call(()->sum("abc"); sourceinfo=:full)
        s = result_string(result)
        @test occursin(r"@ Base /.*reduce\.jl:\d+", s)
    end

    let result = report_call(()->sum("abc"); sourceinfo=:default)
        s = result_string(result)
        @test occursin(r"@ Base \./reduce\.jl:\d+", s)
    end

    let result = report_call(()->sum("abc"); sourceinfo=:compact)
        s = result_string(result)
        @test occursin(r"@ Base reduce\.jl:\d+", s)
        @test !occursin(r"@ Base /.*reduce\.jl:\d+", s)
    end

    let result = report_call(()->sum("abc"); sourceinfo=:minimal)
        s = result_string(result)
        @test occursin(r"@ Base\n", s)
        @test !occursin(r"reduce\.jl", s)
    end

    let result = report_call(()->sum("abc"); sourceinfo=:none)
        s = result_string(result)
        @test !occursin(r"@ Base", s)
        @test !occursin(r"reduce\.jl", s)
    end

    let result = report_call(()->sum("abc"); sourceinfo=:invalid)
        @test_throws ArgumentError result_string(result)
    end
end

end # module test_print
