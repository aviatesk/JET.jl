@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        res, interp = profile_toplevel(s)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "virtual module self-reference" begin
    let
        res, interp = @profile_toplevel begin
            Main.sum("julia") # `Main.sum` should be resolved as constant
        end
        test_sum_over_string(res)
    end
end

@testset "\"toplevel definitions\"" begin
    let
        vmod = gen_virtualmod()
        @profile_toplevel vmod begin
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
        end

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
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            foo = rand(Bool)

            struct Foo
                b::Bool
                Foo(b = foo) = new(b)
            end

            Foo()
        end

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
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            let
                struct Foo end
                foo(::Foo) = :Foo
                foo(Foo.instance)
            end
        end

        @test isdefined(vmod, :Foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # "toplevel definitions" can't resolve access to local objects
    let
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            let
                i = 1
                struct Foo
                    val::Int
                    Foo(val = i) = new(val)
                end
                foo = Foo()
            end
        end

        @test !isdefined(vmod, :i)
        @test isnothing(get_virtual_globalvar(interp, vmod, :i))
        @test isempty(res.toplevel_error_reports)
        @test_broken isempty(res.inference_error_reports)
    end
end

@testset "macro expansions" begin
    let
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            @inline foo(a) = identity(a)
            foo(10)
        end

        @test isdefined(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                push!(ex.args, 1)
                return ex
            end

            @foo sin() # otherwise NoMethodError
        end

        @test isdefined(vmod, Symbol("@foo"))
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # macro expansions with access to global variables will fail
    let
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            const arg = 1

            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                push!(ex.args, arg)
                return ex
            end

            @foo sin()
        end

        @test isdefined(vmod, Symbol("@foo"))
        @test_broken isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end
end

@testset "remove `const`" begin
    let
        res, interp = @profile_toplevel begin
            const s = "julia"
            sum(s)
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        res, interp = @profile_toplevel begin
            let
                const s = "julia"
                sum(s)
            end
        end

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURE_DIR, "include1.jl")
        f2 = normpath(FIXTURE_DIR, "include1.jl")

        vmod = gen_virtualmod()
        res, interp = profile_file′(f1, vmod)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test isdefined(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        res, interp = profile_file′(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        res, interp = profile_file′(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        res, interp = profile_file′(f1)

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
    res, interp = profile_toplevel(s)
    @test isempty(res.toplevel_error_reports)
end

@testset "module usage" begin
    # using
    let
        res, interp = @profile_toplevel begin
            module foo

            using Base.Meta: isexpr

            isexpr(:(foo(bar)), :call)
            isexpr2(:(foo(bar)), :call)

            end
        end

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
        @test occursin("isexpr2", first(res.inference_error_reports).msg)
    end

    # sequential usage
    let
        res, interp = @profile_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo

            bar("julia") # -> GlobalUndefVarErrorReport

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    # usage of global objects
    let
        res, interp = @profile_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo

            foo.bar("julia") # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        res, interp = @profile_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo: bar

            bar("julia") # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # usage of global variables
    let
        res, interp = @profile_toplevel begin
            module foo

            const bar = "julia"

            module baz

            using ..foo

            sum(foo.bar) # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # usage of global variables
    let
        res, interp = @profile_toplevel begin
            module foo

            const bar = sum

            module baz

            using ..foo: bar

            bar("julia") # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        # TODO: fix usage of virtual global variables
        @test_broken isempty(res.toplevel_error_reports)
        # test_sum_over_string(res)
    end

    # export
    let
        res, interp = @profile_toplevel begin
            module foo

            bar(s) = sum(s)

            export bar

            end

            using .foo

            bar("julia") # -> NoMethodErrorReports
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "sequential" begin
    let
        res, interp = @profile_toplevel begin
            foo(1:1000)

            foo(a) = length(a)
        end
        @test length(res.inference_error_reports) === 1
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        res, interp = @profile_toplevel begin
            foo(a) = length(a)
            foo("julia") # should not error

            foo(a) = sum(a)
            foo(1:1000)  # should not error too
        end
        @test isempty(res.inference_error_reports)
    end
end

@testset "virtual global variables" begin
    let
        vmod = gen_virtualmod()
        res, interp = @profile_toplevel vmod begin
            var = rand(Bool)
            const constvar = rand(Bool)
        end
        @test !isnothing(get_virtual_globalvar(interp, vmod, :var))
        @test !isnothing(get_virtual_globalvar(interp, vmod, :constvar))
    end

    @testset "scope" begin
        # function
        # --------

        let
            vmod = gen_virtualmod()
            interp, frame = Core.eval(vmod, :($(profile_call)() do
                localvar = rand(Bool)
                global globalvar = localvar
            end))
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end

        # blocks
        # ------

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                begin
                    globalvar = rand(Bool)
                end
            end
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                begin
                    local localvar = rand(Bool)
                    globalvar = localvar
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end

        let
            vmod = gen_virtualmod()
            @test_broken try
                res, interp = @profile_toplevel vmod begin
                    begin
                        local localvar
                        localvar = rand(Bool) # this shouldn't be annotated as `global`
                        globalvar = localvar
                    end
                end
                @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
                @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
            catch
                false
            end
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                globalvar2 = begin
                    local localvar = rand(Bool)
                    globalvar1 = localvar
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :locbalvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar1))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar2))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                let
                    localvar = rand(Bool)
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                globalvar = let
                    localvar = rand(Bool)
                    localvar
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end

        # loops
        # -----

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                for i = 1:100
                    localvar = i
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                for i in 1:10
                    localvar = rand(i)
                    global globalvar = localvar
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                while true
                    localvar = rand(Bool)
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
        end

        let
            vmod = gen_virtualmod()
            res, interp = @profile_toplevel vmod begin
                while true
                    localvar = rand()
                    global globalvar = localvar
                end
            end
            @test isnothing(get_virtual_globalvar(interp, vmod, :localvar))
            @test !isnothing(get_virtual_globalvar(interp, vmod, :globalvar))
        end
    end
end
