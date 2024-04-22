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
            @test occursin("1 toplevel error found", s)
            @test occursin(Regex("@ $(@__FILE__):\\d"), s)
            @static if JET.JULIA_SYNTAX_ENABLED
                @test occursin("invalid identifier", s) || occursin("Expected `end`", s)
            else
                @test occursin("syntax: unexpected \"end\"", s)
            end
        end

        res = report_text(src, "foo")
        print_reports(io, res.res.toplevel_error_reports)
        let s = String(take!(io))
            @test occursin("1 toplevel error found", s)
            @test occursin(r"@ foo:\d", s)
            @static if JET.JULIA_SYNTAX_ENABLED
                @test occursin("invalid identifier", s) || occursin("Expected `end`", s)
            else
                @test occursin("syntax: unexpected \"end\"", s)
            end
        end
    end
end

@testset "print inference errors" begin
    let #=== LINE SENSITIVITY START ===#
        res = @analyze_toplevel begin
            s = "julia"
            sum(s)
        end

        io = IOBuffer()
        @test !iszero(print_reports(io, res.res.inference_error_reports))
        let s = String(take!(io))
            @test occursin("2 possible errors found", s)
            @test occursin("$(escape_string(@__FILE__)):$((@__LINE__)-7)", s) # toplevel call site
        end
    end #=== LINE SENSITIVITY END ===#

    let #=== LINE SENSITIVITY START ===#
        res = @analyze_toplevel begin
            foo(args...) = args_typo # typo
            foo(rand(Char, 1000000000)...)
        end

        io = IOBuffer()
        @test !iszero(print_reports(io, res.res.inference_error_reports, JET.gen_postprocess(res.res.actual2virtual)))
        let s = String(take!(io))
            @test occursin("1 possible error found", s)
            @test occursin("$(escape_string(@__FILE__)):$((@__LINE__)-8)", s) # toplevel call site
        end
    end #=== LINE SENSITIVITY END ===#
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

end # module test_print
