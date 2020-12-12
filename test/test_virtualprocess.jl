@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        res, interp = profile_text′(s)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "fix self-reference of virtual module" begin
    let
        res, interp = @profile_toplevel begin
            const foo = sum
            Main.foo("julia") # `Main.sum` should be resolved as constant
        end
        test_sum_over_string(res)
    end

    let
        res, interp = @profile_toplevel begin
            let
                Main = "julia" # local `Main` should not be resolved to virtual module
                sum(Main)
            end
        end
        test_sum_over_string(res)
    end
end

@testset "fix toplevel global `Symbol`" begin
    # this case otherwise will throw an error in `CC.typ_for_val` in optimization
    let
        res, interp = @profile_toplevel begin
            v = '1'
            v = if rand(Bool)
                rand(Int)
            else
                v
            end
            sin(v)
        end
        @test true
    end

    # `c` wrapped in `GotoIfNot` node should be transformed into `GlobalRef(vmod, :c)`,
    # otherwise `NonBooleanCondErrorReport` (and `UncaughtExceptionReport`) will be reported
    let
        res, interp = @profile_toplevel begin
            c = false
            if c
                throw("should be ignored")
            end
        end
        @test isempty(res.inference_error_reports)
    end
end

@testset "\"toplevel definitions\"" begin
    let
        vmod = gen_virtual_module()
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

        @test is_concrete(vmod, :foo)
        @test !is_concrete(vmod, :foo′)
        @test is_concrete(vmod, Symbol("@foo"))
        @test is_concrete(vmod, :Foo)
        @test is_concrete(vmod, :Foo1)
        @test is_concrete(vmod, :Foo2)
        @test is_concrete(vmod, :Foo3)
        @test is_concrete(vmod, :Fix1)
        @test !isempty(methodswith(getfield(vmod, :Foo), getproperty))
    end

    # basic profiling with user-defined types
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            gb = rand(Bool)

            struct Foo
                b::Bool
                Foo(b = gb) = new(b)
            end

            foo = Foo(gb)
        end

        # global variables aren't evaluated but kept in `interp` instead
        @test is_abstract(vmod, :gb)
        @test isa_abstract(vmod.gb, Bool)
        @test is_concrete(vmod, :Foo)
        @test is_abstract(vmod, :foo)
        @test isa_abstract(vmod.foo, vmod.Foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # definitions using type aliases
    let
        vmod = gen_virtual_module()
        @profile_toplevel vmod begin
            const BT = Bool

            struct Foo
                b::BT
            end

            const c = Foo(rand(Bool))
            uc = Foo(rand(Bool))
        end
        @test is_concrete(vmod, :BT)
        @test is_concrete(vmod, :Foo)
        @test is_abstract(vmod, :c)
        @test is_abstract(vmod, :uc)
    end

    # a toplevel definition within a block
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            begin
                struct Foo
                    bar
                end

                foo = Foo(:bar)
                println(foo)
            end
        end
        @test is_concrete(vmod, :Foo)
        @test isa_abstract(vmod.foo, vmod.Foo)
    end

    # toplevel definitions within a block
    # somewhat related upstream issue: https://github.com/JuliaDebug/LoweredCodeUtils.jl/issues/47
    # well, the actual error here is world age error ...
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            begin
                abstract type Foo end
                struct Foo1 <: Foo
                    foo
                end

                foo = Foo1(:foo)
                println(foo)
            end
        end
        @test isempty(res.toplevel_error_reports)
        @test is_abstract(vmod, :foo)
    end

    @testset "toplevel definitions by `eval` calls" begin
        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                # these definitions shouldn't be abstracted away
                for fname in (:foo, :bar, :baz)
                    @eval begin
                        @inline ($(Symbol("is", fname)))(a) = a === $(QuoteNode(fname))
                    end
                end

                # these should be abstracted away (i.e. shouldn't throw)
                isfoo(:foo) && throw("foo")                   # should be reported
                isbar(:foo) && isbaz(:baz) && throw("foobaz") # shouldn't be reported
                isbar(:bar) && isbaz(:baz) && throw("barbaz") # should be reported
            end

            @test is_concrete(vmod, :isfoo)
            @test is_concrete(vmod, :isbar)
            @test is_concrete(vmod, :isbaz)
            @test length(res.inference_error_reports) == 2
            @test all(er->isa(er, UncaughtExceptionReport), res.inference_error_reports)
        end
    end
end

@testset "macro expansions" begin
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            @inline foo(a) = identity(a)
            foo(10)
        end

        @test is_concrete(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                push!(ex.args, 1)
                return ex
            end

            @foo sin() # otherwise NoMethodError
        end

        @test is_concrete(vmod, Symbol("@foo"))
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    # macro expansions with access to global variables will fail
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            const arg = rand(Bool)

            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                arg && push!(ex.args, 1)
                return ex
            end

            @foo sin()
        end

        @test is_concrete(vmod, Symbol("@foo"))
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

@testset "expression flattening" begin
    let
        @test (begin
            # internal error shouldn't occur
            res, interp = @profile_toplevel begin
                r = rand(); s = sin(a); c = cos(b); tan(c)
            end
        end; true)
    end
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURE_DIR, "include1.jl")
        f2 = normpath(FIXTURE_DIR, "include1.jl")

        vmod = gen_virtual_module()
        res, interp = profile_file′(f1, vmod)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test is_concrete(vmod, :foo)
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
    res, interp = profile_text′(s)
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

    # error handling for module usages
    let
        res, interp = @profile_toplevel begin
            using Base: foo
        end

        @test !isempty(res.toplevel_error_reports)
        er = first(res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err == UndefVarError(:foo)
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 7
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

    # module usage within a block
    let
        res, interp = @profile_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            begin
                using ..foo: bar
                bar("julia") # -> NoMethodErrorReports
            end

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    @testset "module usage of virtual global variable" begin
        let
            res, interp = @profile_toplevel begin
                module foo

                const bar = sum

                module baz

                using ..foo: bar

                bar("julia")

                end # module bar

                end # module foo
            end

            @test isempty(res.toplevel_error_reports)
            test_sum_over_string(res)
        end

        let
            res, interp = @profile_toplevel begin
                module foo

                const bar = "julia"

                module baz

                using ..foo: bar

                sum(bar)

                end # module bar

                end # module foo
            end

            @test isempty(res.toplevel_error_reports)
            test_sum_over_string(res)
        end

        let
            res, interp = @profile_toplevel begin
                module foo

                const bar = "julia"

                export bar

                end # module foo

                using .foo
                sum(bar)
            end

            @test isempty(res.toplevel_error_reports)
            test_sum_over_string(res)
        end
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
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            var = rand(Bool)
            const constvar = rand(Bool)
        end

        @test is_abstract(vmod, :var)
        @test isa_abstract(vmod.var, Bool)
        @test is_abstract(vmod, :constvar)
        @test isa_abstract(vmod.constvar, Bool)
    end

    @testset "scope" begin
        # function
        # --------

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    local localvar = rand(Bool)
                    global globalvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        # blocks
        # ------

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    globalvar = rand(Bool)
                end
            end

            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    local localvar = rand(Bool)
                    globalvar = localvar
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    local localvar
                    localvar = rand(Bool) # this shouldn't be annotated as `global`
                    globalvar = localvar
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                globalvar2 = begin
                    local localvar = rand(Bool)
                    globalvar1 = localvar
                end
            end

            @test !isdefined(vmod, :locbalvar)
            @test is_abstract(vmod, :globalvar1)
            @test isa_abstract(vmod.globalvar1, Bool)
            @test is_abstract(vmod, :globalvar2)
            @test isa_abstract(vmod.globalvar2, Bool)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                let
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                globalvar = let
                    localvar = rand(Bool)
                    localvar
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        # loops
        # -----

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                for i = 1:100
                    localvar = i
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                for i in 1:10
                    localvar = rand(Bool)
                    global globalvar = localvar
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                while true
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                while true
                    localvar = rand(Bool)
                    global globalvar = localvar
                end
            end

            @test !isdefined(vmod, :localvar)
            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end
    end

    @testset "multiple declaration/assignment" begin
        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                s, c = sincos(1)
            end

            @test is_abstract(vmod, :s)
            @test isa_abstract(vmod.s, Float64)
            @test is_abstract(vmod, :c)
            @test isa_abstract(vmod.c, Float64)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    local s, c
                    s, c = sincos(1)
                end
            end

            @test !isdefined(vmod, :s)
            @test !isdefined(vmod, :c)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                let
                    global s, c
                    s, c = sincos(1)
                end
            end

            @test is_abstract(vmod, :s)
            @test isa_abstract(vmod.s, Float64)
            @test is_abstract(vmod, :c)
            @test isa_abstract(vmod.c, Float64)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                so, co = let
                    si, ci = sincos(1)
                    si, ci
                end
            end

            @test !isdefined(vmod, :si)
            @test !isdefined(vmod, :ci)
            @test is_abstract(vmod, :so)
            @test isa_abstract(vmod.so, Float64)
            @test is_abstract(vmod, :co)
            @test isa_abstract(vmod.co, Float64)
        end

        let
            vmod = gen_virtual_module()
            res, interp = @profile_toplevel vmod begin
                begin
                    local l
                    l, g = sincos(1)
                end
            end
            @test !isdefined(vmod, :l)
            @test is_abstract(vmod, :g)
            @test isa_abstract(vmod.g, Float64)
        end
    end
end

@testset "toplevel throw" begin
    res, interp = @profile_toplevel begin
        throw("throw me")
    end
    @test length(res.inference_error_reports) == 1
    @test first(res.inference_error_reports) isa UncaughtExceptionReport
end

@testset "error handling within ConcreteInterpreter" begin
    let
        res, interp = @profile_toplevel begin
            struct A <: B end # UndefVarError(:B) should be handled into `res.toplevel_error_reports`
        end

        @test !isempty(res.toplevel_error_reports)
        er = first(res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err == UndefVarError(:B)
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 7
    end

    # stacktrace cropping
    let
        res, interp = @profile_toplevel begin
            foo() = throw("don't call me, pal")
            struct A <: foo() end
        end

        @test !isempty(res.toplevel_error_reports)
        er = first(res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err == "don't call me, pal"
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 7
        @test length(er.st) == 1
        sf = first(er.st)
        @test sf.file === Symbol(@__FILE__) && sf.line == (@__LINE__) - 11
    end
end

@testset "invalid constant redefinition" begin
    # for virtual global assignment
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)

            const foo = fib(1000000000000) # ::Int

            foo = fib(1000000000000.) # ::Float64
        end

        @test is_abstract(vmod, :foo)
        @test length(res.inference_error_reports) == 1
        er = first(res.inference_error_reports)
        @test er isa InvalidConstantRedefinition
        @test er.name === :foo
    end

    # for concretized constants
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            const T = typeof(fib(1000000000000)) # never terminates, yes

            T = Nothing
        end

        @test is_concrete(vmod, :T) # wao, this is concretized
        @test length(res.inference_error_reports) == 1
        er = first(res.inference_error_reports)
        @test er isa InvalidConstantRedefinition
        @test er.name === :T
    end
end

@testset "control flows in toplevel frames" begin
    let
        vmod = gen_virtual_module()
        res, interp = @profile_toplevel vmod begin
            v = rand(Int)

            if rand(Bool)
                v = rand(Char)
            end

            sin(v) # union-split no method error should be reported
        end

        @test is_abstract(vmod, :v)
        @test length(res.inference_error_reports) === 1
        er = first(res.inference_error_reports)
        @test er isa NoMethodErrorReport
        @test er.unionsplit
    end
end
