function profile!(s, virtualmod;
                  filename = "top-level",
                  actualmodsym = :Main,
                  interp = TPInterpreter(),
                  )
    return virtual_process!(s, filename, actualmodsym, virtualmod, interp)
end

const ERROR_REPORTS_FOR_SUM_OVER_STRING = let
    res = profile!("sum(\"julia\")", Main)
    @test !isempty(res.inference_error_reports)
    res.inference_error_reports
end

function test_sum_over_string(res)
    @test !isempty(res.inference_error_reports)
    for target in ERROR_REPORTS_FOR_SUM_OVER_STRING
        @test any(res.inference_error_reports) do er
            return er.msg == target.msg && er.sig == target.sig
        end
    end
end

@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "\"toplevel definitions\"" begin
    let
        s = """
        # function
        foo() = nothing

        # local function, shouldn't be evaluated
        let
            c = rand(Bool)
            foo′(a) = a || c
        end

        # macro
        macro foo(ex) ex end

        # abstract type
        abstract type Foo end

        # struct
        struct Foo1 <: Foo
            val::Int
        end
        mutable struct Foo2 <: Foo
            val::Int
        end

        # primitive type
        primitive type Foo3 32 end

        # import, using
        using Base: Fix1
        import Base: getproperty

        function getproperty(foo::Foo, sym::Symbol)
            return if sym === :val
                getfield(foo, sym)::Int
            else
                getfield(foo, sym)
            end
        end
        """
        virtualmod = gen_mod()

        profile!(s, virtualmod)

        @test isdefined(virtualmod, :foo)
        @test !isdefined(virtualmod, :foo′)
        @test isdefined(virtualmod, Symbol("@foo"))
        @test isdefined(virtualmod, :Foo)
        @test isdefined(virtualmod, :Foo1)
        @test isdefined(virtualmod, :Foo2)
        @test isdefined(virtualmod, :Foo3)
        @test isdefined(virtualmod, :Fix1)
        @test !isempty(methodswith(getfield(virtualmod, :Foo), getproperty))
    end

    # "toplevel definitions" with access to global objects
    let
        s = """
        foo = rand(Bool)

        struct Foo
            b::Bool
            Foo(b = foo) = new(b)
        end

        Foo()
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test !isdefined(virtualmod, :foo) # global variables aren't evaluated
        @test isdefined(virtualmod, :Foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end
end

@testset "hoisting \"toplevel definitions\"" begin
    let
        s = """
        let
            struct Foo end
            foo(::Foo) = :Foo
            foo(Foo.instance)
        end
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isdefined(virtualmod, :Foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # "toplevel definitions" can't resolve access to local objects
    let
        s = """
        let
            i = 1
            struct Foo
                val::Int
                Foo(val = i) = new(val)
            end
            foo = Foo()
        end
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test !isdefined(virtualmod, :i)
        @test isempty(res.toplevel_error_reports)
        @test_broken isempty(res.inference_error_reports)
    end
end

@testset "macro expansions" begin
    let
        s = """
        @inline foo(a) = identity(a)
        foo(10)
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isdefined(virtualmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        s = """
        macro foo(ex)
            @assert Meta.isexpr(ex, :call)
            push!(ex.args, 1)
            return ex
        end

        @foo sin()
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isdefined(virtualmod, Symbol("@foo"))
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # macro expansions with access to global variables will fail
    let
        s = """
        const arg = 1

        macro foo(ex)
            @assert Meta.isexpr(ex, :call)
            push!(ex.args, arg)
            return ex
        end

        @foo sin()
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isdefined(virtualmod, Symbol("@foo"))
        @test_broken isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end
end

@testset "remove `const`" begin
    let
        s = """
        const s = "julia"
        sum(s)
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        s = """
        let
            const s = "julia"
            sum(s)
        end
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURE_DIR, "include1.jl")
        f2 = normpath(FIXTURE_DIR, "include1.jl")
        s = read(f1, String)

        virtualmod = gen_mod()

        res = profile!(s, virtualmod; filename = f1)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test isdefined(virtualmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        s = read(f, String)

        virtualmod = gen_mod()

        res = profile!(s, virtualmod; filename = f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa UndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        s = read(f, String)

        virtualmod = gen_mod()

        res = profile!(s, virtualmod; filename = f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        s = read(f1, String)

        virtualmod = gen_mod()

        res = profile!(s, virtualmod; filename = f1)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test !isempty(res.toplevel_error_reports)
        let
            report = first(res.toplevel_error_reports)
            @test report isa RecursiveIncludeErrorReport
            @test report.duplicated_file == f1
            @test f1 in report.files
            @test f2 in report.files
            @test report.file == f2
            @test report.line == 1
        end
    end
end

@testset "module definition" for s in ("""
                                       module foo end
                                       """,
                                       """
                                       baremodule foo end
                                       """,
                                       """
                                       module foo
                                       module bar end
                                       end
                                       """
                                       )
    virtualmod = gen_mod()
    res = profile!(s, virtualmod)
    @test isempty(res.toplevel_error_reports)
end

@testset "module usage" begin
    # using
    let
        s = """
        module foo

        using Base.Meta: isexpr

        isexpr(:(foo(bar)), :call)
        isexpr2(:(foo(bar)), :call)

        end
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa UndefVarErrorReport
        @test occursin("isexpr2", first(res.inference_error_reports).msg)
    end

    # sequential usage
    let
        s = """
        module foo

        bar(s) = sum(s)

        module baz

        using ..foo

        bar("julia") # -> UndefVarErrorReport

        end # module bar

        end # module foo
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa UndefVarErrorReport
    end

    # usage of global objects
    let
        s = """
        module foo

        bar(s) = sum(s)

        module baz

        using ..foo

        foo.bar("julia") # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        s = """
        module foo

        bar(s) = sum(s)

        module baz

        using ..foo: bar

        bar("julia") # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # usage of global variables
    let
        s = """
        module foo

        const bar = sum

        module baz

        using ..foo

        foo.bar("julia") # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        @test_broken !isempty(res.inference_error_reports)
        # test_sum_over_string(res) # TODO: propagate `getfield` correctly
    end

    # usage of global variables
    let
        s = """
        module foo

        const bar = sum

        module baz

        using ..foo: bar

        bar("julia") # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        # TODO: fix usage of virtual global variables
        @test_broken isempty(res.toplevel_error_reports)
        # test_sum_over_string(res)
    end

    # export
    let
        s = """
        module foo

        bar(s) = sum(s)

        export bar

        end

        using .foo

        bar("julia") # -> NoMethodErrorReports
        """

        virtualmod = gen_mod()

        res = profile!(s, virtualmod)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "sequential" begin
    collect_report_errors(s; actualmod = Main, filename = "top-level", kwargs...) =
        return report_errors(actualmod, s, filename; kwargs...)[2]

    let
        s = """
        foo(1:1000)

        foo(a) = length(a)
        """

        rps = collect_report_errors(s)
        @test length(rps) === 1
        @test first(rps) isa UndefVarErrorReport
    end

    let
        s = """
        foo(a) = length(a)
        foo("julia") # should not error

        foo(a) = sum(a)
        foo(1:1000)  # should not error too
        """

        rps = collect_report_errors(s)
        @test isempty(rps)
    end
end
