module test_print

include("../setup.jl")

function result_string(result)
    buf = IOBuffer()
    show(buf, result)
    return String(take!(buf))
end

@testset "print toplevel errors" begin
    let io = IOBuffer()
        src = """
            a = begin
                b =
            end
            """

        res = report_text(src, @__FILE__)
        print_reports(io, res.res.toplevel_error_reports)
        let s = String(take!(io))
            @test occursin("2 toplevel errors found", s)
            @test occursin(Regex("@ $(@__FILE__):\\d"), s)
            @test occursin("invalid identifier", s)
            @test occursin("Expected `end`", s)
        end

        res = report_text(src, "foo")
        print_reports(io, res.res.toplevel_error_reports)
        let s = String(take!(io))
            @test occursin("2 toplevel errors found", s)
            @test occursin(r"@ foo:\d", s)
            @test occursin("invalid identifier", s)
            @test occursin("Expected `end`", s)
        end
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
