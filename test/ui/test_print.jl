module test_print

include("../setup.jl")

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
        buf = IOBuffer()
        show(buf, result)
        s = String(take!(buf))
        @test occursin(":nonexist", s)
    end

    let result = report_call() do
            sin("julia")
        end
        buf = IOBuffer()
        show(buf, result)
        s = String(take!(buf))
        @test occursin("sin(\"julia\")", s)
    end
end

test_print_callf(f, a) = f(a)
@testset "simplified global references" begin
    # exported names should not be canonicalized
    let result = @report_call sum("julia")
        buf = IOBuffer()
        show(buf, result)
        s = String(take!(buf))
        @test occursin("+", s)
        @test !occursin("Base.:+", s)
        @test occursin("zero", s)
        @test !occursin("Base.zero", s)
    end

    # `Main.`-prefix should be omitted
    let result = report_call() do
            sin("42")
        end
        buf = IOBuffer()
        show(buf, result)
        s = String(take!(buf))
        @test occursin("sin", s)
        @test !occursin(r"(Main|Base)\.sin", s)
    end
    let result = report_call() do
            test_print_callf(sin, "42")
        end
        buf = IOBuffer()
        show(buf, result)
        s = String(take!(buf))
        @test occursin("test_print_callf", s)
        @test !occursin(r"(Main|Base)\.test_print_callf", s)
    end
end

struct F49231{a,b,c,d,e,f,g}
    num::g
end;
bar(x) = rand() > 0.5 ? x : Any[0][1]
mysum(x) = sum(y-> bar(x.num), 1:5; init=0)

@testset "Depth-limited type printing" begin
    f = F49231{Float64,Float32,Int,String,AbstractString,6,Float64}(1)
    Ftype = Tuple{Vector{typeof(f)}}
    result = JET.report_opt(Ftype; stacktrace_types_limited=true) do a
        mysum(a[1]) # runtime dispatch !
    end
    buf = IOBuffer()
    show(buf, result)
    s = String(take!(buf))
    @test occursin("F49231{…}", s)
end

@testset "Depth-limited type printing - show types with large depth" begin
    f = F49231{Float64,Float32,Int,String,AbstractString,6,Float64}(1)
    Ftype = Tuple{Vector{typeof(f)}}
    result = JET.report_opt(Ftype; stacktrace_types_limited=false) do a
        mysum(a[1]) # runtime dispatch !
    end
    buf = IOBuffer()
    show(buf, result)
    s = String(take!(buf))
    @test !occursin("F49231{…}", s)
end

end # module test_print
