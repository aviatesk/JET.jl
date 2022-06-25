# NOTE:
# tests in this file may not be so robust against the future changes in JET.jl or
# even those in julia itself

@testset "print toplevel errors" begin
    let
        io = IOBuffer()
        s = """
            a = begin
                b =
            end
            """

        res = report_text(s, @__FILE__).res
        print_reports(io, res.toplevel_error_reports)
        let s = String(take!(io))
            @test occursin("1 toplevel error found", s)
            @test occursin(Regex("@ $(@__FILE__):\\d"), s)
            @test occursin("syntax: unexpected \"end\"", s)
        end

        res = report_text(s, "foo").res
        print_reports(io, res.toplevel_error_reports)
        let s = String(take!(io))
            @test occursin("1 toplevel error found", s)
            @test occursin(r"@ foo:\d", s)
            @test occursin("syntax: unexpected \"end\"", s)
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
        @test !iszero(print_reports(io, res.inference_error_reports))
        let s = String(take!(io))
            @test occursin("2 possible errors found", s)
            @test occursin(Regex("@ $(escape_string(@__FILE__)):$((@__LINE__)-7)"), s) # toplevel call signature
        end
    end #=== LINE SENSITIVITY END ===#

    @testset "special case splat call signature" begin
        let #=== LINE SENSITIVITY START ===#
            res = @analyze_toplevel begin
                foo(args...) = args_typo # typo
                foo(rand(Char, 1000000000)...)
            end

            io = IOBuffer()
            @test !iszero(print_reports(io, res.inference_error_reports, JET.gen_postprocess(res.actual2virtual)))
            let s = String(take!(io))
                @test occursin("1 possible error found", s)
                @test occursin(Regex("@ $(escape_string(@__FILE__)):$((@__LINE__)-8)"), s) # toplevel call signature
                @test occursin("foo(rand(Char, 1000000000)...)", s)
            end
        end #=== LINE SENSITIVITY END ===#
    end
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
