@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        res, interp = profile_toplevel!(s)

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
        vmod = gen_virtualmod()
        profile_toplevel!(s, vmod)

        @test isdefined(vmod, :foo)
        @test !isdefined(vmod, :foo′)
        @test isdefined(vmod, Symbol("@foo"))
        @test isdefined(vmod, :Foo)
        @test isdefined(vmod, :Foo1)
        @test isdefined(vmod, :Foo2)
        @test isdefined(vmod, :Foo3)
        @test isdefined(vmod, :Fix1)
        @test !isempty(methodswith(getfield(vmod, :Foo), getproperty))
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

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        # global variables aren't evaluated but kept in `interp` instead
        @test !isdefined(vmod, :foo)
        @test !isnothing(get_virtual_globalvar(interp, vmod, :foo))
        @test isdefined(vmod, :Foo)
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

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test isdefined(vmod, :Foo)
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

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test !isdefined(vmod, :i)
        @test isnothing(get_virtual_globalvar(interp, vmod, :i))
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

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test isdefined(vmod, :foo)
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

        @foo sin() # otherwise NoMethodError
        """

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test isdefined(vmod, Symbol("@foo"))
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

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test isdefined(vmod, Symbol("@foo"))
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

        res, interp = profile_toplevel!(s)

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

        res, interp = profile_toplevel!(s)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURE_DIR, "include1.jl")
        f2 = normpath(FIXTURE_DIR, "include1.jl")
        s = read(f1, String)

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod; filename = f1)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test isdefined(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        res, interp = profile_file!(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        res, interp = profile_file!(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        res, interp = profile_file!(f1)

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
    res, interp = profile_toplevel!(s)
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

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
        @test occursin("isexpr2", first(res.inference_error_reports).msg)
    end

    # sequential usage
    let
        s = """
        module foo

        bar(s) = sum(s)

        module baz

        using ..foo

        bar("julia") # -> GlobalUndefVarErrorReport

        end # module bar

        end # module foo
        """

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
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

        res, interp = profile_toplevel!(s)

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

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # usage of global variables
    let
        s = """
        module foo

        const bar = "julia"

        module baz

        using ..foo

        sum(foo.bar) # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
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

        res, interp = profile_toplevel!(s)

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

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "sequential" begin
    let
        s = """
        foo(1:1000)

        foo(a) = length(a)
        """

        res, interp = profile_toplevel!(s)
        @test length(res.inference_error_reports) === 1
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        s = """
        foo(a) = length(a)
        foo("julia") # should not error

        foo(a) = sum(a)
        foo(1:1000)  # should not error too
        """

        res, interp = profile_toplevel!(s)
        @test isempty(res.inference_error_reports)
    end
end

@testset "virtual global variables" begin
    let
        s = """
        var = rand(Bool)
        const constvar = rand(Bool)
        """

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)
        @test !isnothing(get_virtual_globalvar(interp, vmod, :var))
        @test !isnothing(get_virtual_globalvar(interp, vmod, :constvar))
    end

    # local variables shouldn't leak into global
    @testset "no global leak" for s in ("""
                                        let
                                            localvar = rand(Bool)
                                        end
                                        """,
                                        """
                                        for i = 1:100
                                            localvar = i
                                        end
                                        """,
                                        """
                                        while true
                                            localvar = rand(Bool)
                                        end
                                        """
                                        )

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)
        @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
    end
end
