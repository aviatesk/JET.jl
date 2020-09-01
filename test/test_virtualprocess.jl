@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test !isempty(ret.toplevel_error_reports)
        @test first(ret.toplevel_error_reports) isa SyntaxErrorReport
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

        virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

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

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test !isdefined(virtualmod, :foo) # global variables aren't evaluated
        @test isdefined(virtualmod, :Foo)
        @test isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
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

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test isdefined(virtualmod, :Foo)
        @test isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
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

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test !isdefined(virtualmod, :i)
        @test isempty(ret.toplevel_error_reports)
        @test_broken isempty(ret.inference_error_reports)
    end
end

@testset "macro expansions" begin
    let
        s = """
        @inline foo(a) = identity(a)
        foo(10)
        """

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test isdefined(virtualmod, :foo)
        @test isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
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

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test isdefined(virtualmod, Symbol("@foo"))
        @test isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
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

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test isdefined(virtualmod, Symbol("@foo"))
        @test_broken isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
    end
end

@testset "remove `const`" begin
    let
        s = """
        const s = "julia"
        sum(s)
        """

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test isempty(ret.toplevel_error_reports)
        @test !isempty(ret.inference_error_reports)
    end

    let
        s = """
        let
            const s = "julia"
            sum(s)
        end
        """

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, "top-level")

        @test !isempty(ret.toplevel_error_reports)
        @test first(ret.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURE_DIR, "include1.jl")
        f2 = normpath(FIXTURE_DIR, "include1.jl")
        s = read(f1, String)

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, f1)

        @test f1 in ret.included_files
        @test f2 in ret.included_files
        @test isdefined(virtualmod, :foo)
        @test isempty(ret.toplevel_error_reports)
        @test isempty(ret.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        s = read(f, String)

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, f)

        @test f in ret.included_files
        @test !isempty(ret.toplevel_error_reports)
        @test first(ret.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(ret.inference_error_reports)
        @test first(ret.inference_error_reports) isa UndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        s = read(f, String)

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, f)

        @test f in ret.included_files
        @test !isempty(ret.toplevel_error_reports)
        @test first(ret.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        s = read(f1, String)

        virtualmod = gen_mod()

        ret = virtual_process!(TPInterpreter(), Main, virtualmod, s, f1)

        @test f1 in ret.included_files
        @test f2 in ret.included_files
        @test !isempty(ret.toplevel_error_reports)
        let
            report = first(ret.toplevel_error_reports)
            @test report isa RecursiveIncludeErrorReport
            @test report.duplicated_file == f1
            @test f1 in report.files
            @test f2 in report.files
            @test report.file == f2
            @test report.line == 1
        end
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
