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

            """)
        @test occursin("@ ./example.jl:7", s)
        @test !occursin("1 toplevel error found", s)
        @test !occursin("No errors detected", s)
    end
    @test isempty(sprint(print_reports, ToplevelErrorReport[]))
    @test sprint(io -> print_reports(io, ToplevelErrorReport[];
        print_toplevel_success=true)) == "No toplevel errors detected\n"
end

@testset "print toplevel warnings" begin
    @test isempty(sprint(print_reports, JET.ToplevelWarningReport[]))
    @test sprint(io -> print_reports(io, JET.ToplevelWarningReport[];
        print_toplevel_success=true)) == "No toplevel warnings detected\n"
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
        @test occursin("2 toplevel warnings found",
            sprint(print_reports, JET.ToplevelWarningReport[report, report]))
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
            msg = sprint(showerror, err, st; context=:color=>color)
            @test sprint(JET.print_report, report; context=:color=>color) == msg
            @test sprint(JET.print_report, report; context=(:color=>color, :markdown_rendering=>false)) == msg
            msg_md = sprint(JET.print_report, report; context=(:color=>color, :markdown_rendering=>true))
            @test occursin("execution failed\n\n```\nStacktrace:", msg_md)
            @test endswith(msg_md, "\n```\n")
            @test msg_md == replace(msg, "\nStacktrace:"=>"\n\n```\nStacktrace:"; count=1) * "\n```\n"
        end
    end
    @testset "without stacktrace" begin
        err = ErrorException("execution failed")
        report = ActualErrorWrapped(err, Base.StackTraces.StackFrame[], "example.jl", 1)
        msg = sprint(showerror, err, report.st)
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
    msg = sprint(JET.print_report, report; context=:markdown_rendering=>markdown_rendering)
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
