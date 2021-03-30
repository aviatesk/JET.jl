@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        res = analyze_text(s)

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "fix self-reference of virtual module" begin
    let
        res = @analyze_toplevel begin
            const foo = sum
            Main.foo("julia") # `Main.sum` should be resolved as constant
        end
        test_sum_over_string(res)
    end

    let
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        @analyze_toplevel vmod begin
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
        res = @analyze_toplevel vmod begin
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
        @analyze_toplevel vmod begin
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
        res = @analyze_toplevel vmod begin
            begin
                struct Foo
                    bar
                end

                foo = Foo(:bar)
                println(foo)
            end
        end
        @test is_concrete(vmod, :Foo)
        @test is_analyzed(vmod, :foo)
        @test isa_analyzed(vmod.foo, vmod.Foo)
    end

    # toplevel definitions within a block
    # somewhat related upstream issue: https://github.com/JuliaDebug/LoweredCodeUtils.jl/issues/47
    # well, the actual error here is world age error ...
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
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
        @test is_analyzed(vmod, :foo)
        @test isa_analyzed(vmod.foo, vmod.Foo1)
    end

    @testset "toplevel definitions by `eval` calls" begin
        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
        res = @analyze_toplevel vmod begin
            @inline foo(a) = identity(a)
            foo(10)
        end

        @test is_concrete(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
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
        res = @analyze_toplevel vmod begin
            const arg = rand(Bool)

            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                arg && push!(ex.args, 1)
                return ex
            end

            @foo sin()
        end

        @test is_concrete(vmod, Symbol("@foo"))
        @test length(res.toplevel_error_reports) == 1
        let r = first(res.toplevel_error_reports)
            @test isa(r, MissingConcretization) # this error should be considered as missing concretization
        end
        @test isempty(res.inference_error_reports)
    end

    # macros that can general :module or :toplevel expressions
    let
        # if we don't expand macros before we check `:toplevel` or `:module` expressions,
        # we may pass `:toplevel` or `:module` expressions to `partially_interpret!` and
        # eventually we will fail to concretize them and their toplevel definitions
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
            macro wrap_in_mod(blk)
                ex = Expr(:module, true, esc(:foo), esc(blk))
                return Expr(:toplevel, ex)
            end

            @wrap_in_mod begin
                bar() = nothing
            end
        end
        @test isdefined(vmod, :foo) && isdefined(vmod.foo, :bar)

        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
            """
                foo

            Docstring for module `foo`
            """
            module foo
            bar() = nothing
            end
        end
        @test isdefined(vmod, :foo) && isdefined(vmod.foo, :bar)
    end
end

@testset "remove `const`" begin
    let
        res = @analyze_toplevel begin
            const s = "julia"
            sum(s)
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        res = @analyze_toplevel begin
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
            res = @analyze_toplevel begin
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
        res = analyze_file(f1, vmod)

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test is_concrete(vmod, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        res = analyze_file(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        res = analyze_file(f)

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        res = analyze_file(f1)

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
    res = analyze_text(s)
    @test isempty(res.toplevel_error_reports)
end

@testset "module usage" begin
    # using
    let
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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

    @testset "module usage of abstract global variable" begin
        let
            res = @analyze_toplevel begin
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
            res = @analyze_toplevel begin
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
            res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
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
        res = @analyze_toplevel begin
            foo(1:1000)

            foo(a) = length(a)
        end
        @test length(res.inference_error_reports) === 1
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        res = @analyze_toplevel begin
            foo(a) = length(a)
            foo("julia") # should not error

            foo(a) = sum(a)
            foo(1:1000)  # should not error too
        end
        @test isempty(res.inference_error_reports)
    end
end

@testset "abstract global variables" begin
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
                begin
                    globalvar = rand(Bool)
                end
            end

            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
                let
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
                for i = 1:100
                    localvar = i
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
                while true
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
                s, c = sincos(1)
            end

            @test is_abstract(vmod, :s)
            @test isa_abstract(vmod.s, Float64)
            @test is_abstract(vmod, :c)
            @test isa_abstract(vmod.c, Float64)
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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
            res = @analyze_toplevel vmod begin
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

    @testset "concretize statically constant variables" begin
        let
            m = gen_virtual_module()
            @analyze_toplevel m begin
                const a = 0
            end
            @test is_concrete(m, :a) && m.a == 0
        end

        # try to concretize even if it's not declared as constant
        let
            m = gen_virtual_module()
            @analyze_toplevel m begin
                a = 0
            end
            @test is_concrete(m, :a) && m.a == 0
        end

        let
            m = gen_virtual_module()
            @analyze_toplevel m begin
                const a = :jetzero # should be quoted, otherwise undef var error
            end
            @test is_concrete(m, :a) && m.a === :jetzero
        end

        # sequential
        let
            m = gen_virtual_module()
            @analyze_toplevel m begin
                a = rand(Int)
                a = 0
            end
            @test is_concrete(m, :a)
            @test m.a == 0
        end
        let
            m = gen_virtual_module()
            @analyze_toplevel m begin
                a = 0
                a = rand(Int)
            end
            @test is_abstract(m, :a)
            @test isa_abstract(m.a, Int)
        end
    end

    @testset "https://github.com/aviatesk/JET.jl/issues/142" begin
        res = @analyze_toplevel begin
            Circle = @NamedTuple begin
                radius::Float64
            end

            function area(c::Circle)
                pi * c.radius^2
            end

            area(Circle(2))
        end

        @test isempty(res.toplevel_error_reports)
    end
end

@testset "toplevel throw" begin
    res = @analyze_toplevel begin
        throw("throw me")
    end
    @test length(res.inference_error_reports) == 1
    @test first(res.inference_error_reports) isa UncaughtExceptionReport
end

@testset "error handling within ConcreteInterpreter" begin
    let
        res = @analyze_toplevel begin
            struct A <: B end # UndefVarError(:B) should be handled into `res.toplevel_error_reports`
        end

        @test !isempty(res.toplevel_error_reports)
        er = first(res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err == UndefVarError(:B)
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 7
    end

    @testset "stacktrace scrubbing" begin
        # NOTE some of the tests below are line-number-sensitive

        # scrub internal frames until (errored) user macro
        let
            res = @analyze_toplevel begin
                macro badmacro(s) throw(s) end # L1
                @badmacro "hi"                 # L2
            end

            @test !isempty(res.toplevel_error_reports)
            er = first(res.toplevel_error_reports)
            @test er isa ActualErrorWrapped
            @test er.file == (@__FILE__) && er.line == (@__LINE__) - 6 # L2
            @test length(er.st) == 1
            sf = first(er.st)
            @test sf.file === Symbol(@__FILE__) && sf.line == (@__LINE__) - 10 # L1
            @test sf.func === Symbol("@badmacro")
        end

        # TODO add test for `eval_with_err_handling` and `lower_with_err_handling`

        # scrub all the internal frame when errors happens in `maybe_evaluate_builtin`
        let
            res = @analyze_toplevel begin
                struct A
                    fld::UndefinedType
                end
            end

            @test !isempty(res.toplevel_error_reports)
            er = first(res.toplevel_error_reports)
            @test er isa ActualErrorWrapped
            @test er.err == UndefVarError(:UndefinedType)
            @test er.file == (@__FILE__) && er.line == (@__LINE__) - 9
            @test isempty(er.st)
        end

        # errors from user functions (i.e. those from `@invokelatest f(fargs...)` in the overload
        # `JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)`)
        let
            res = @analyze_toplevel begin
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
end

@testset "invalid constant redefinition/declaration" begin
    # for abstract global assignment
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel vmod begin
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
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel vmod begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            foo = fib(1000000000000.) # ::Float64
            const foo = fib(1000000000000) # ::Int
        end

        @test is_abstract(vmod, :foo)
        @test length(res.inference_error_reports) == 1
        er = first(res.inference_error_reports)
        @test er isa InvalidConstantDeclaration
        @test er.name === :foo
    end

    # for concretized constants
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel vmod begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            const T = typeof(fib(1000000000000)) # never terminates
            T = Nothing
        end

        @test is_concrete(vmod, :T) # wao, this is concretized
        @test length(res.inference_error_reports) == 1
        er = first(res.inference_error_reports)
        @test er isa InvalidConstantRedefinition
        @test er.name === :T
    end
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel vmod begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            T = Nothing
            const T = typeof(fib(1000000000000)) # never terminates
        end

        @test is_concrete(vmod, :T) # wao, this is concretized
        @test length(res.inference_error_reports) == 1
        er = first(res.inference_error_reports)
        @test er isa InvalidConstantDeclaration
        @test er.name === :T
    end

    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel vmod begin
            a = 0
            const a = 1
        end
        @test length(res.inference_error_reports) == 1
        let er = first(res.inference_error_reports)
            @test er isa InvalidConstantDeclaration
            @test er.name === :a
        end
    end
end

@testset "control flows in toplevel frames" begin
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
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

@testset "docstrings" begin
    # can find errors within docstring generation
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
            """
                foo

            This is a invalid docstring for `$(foo)`,
            becuase `foo` is undefined at this point.
            """
            :(foo)
        end
        @test any(res.inference_error_reports) do r
            r isa GlobalUndefVarErrorReport &&
            r.name === :foo
        end
    end
end

@testset "world age" begin
    # https://github.com/aviatesk/JET.jl/issues/104
    res = @analyze_toplevel begin
        using Test
        @testset begin
            a = @inferred(ones(Int,ntuple(d->1,1)), ntuple(x->x+1,1))
        end
    end
    @test true

    res = @analyze_toplevel begin
        using Test

        @test_warn "foo" println(stderr, "foo")
    end
    @test isempty(res.toplevel_error_reports)
end

@testset "avoid too much bail out from `virtual_process!`" begin
    let
        res = @analyze_toplevel begin
            sin′
        end;
        @test !isempty(res.inference_error_reports)
    end

    let
        res = @analyze_toplevel begin
            s = nothing
            sin′
        end;
        @test !isempty(res.inference_error_reports)
    end
end

# will be used in the following two testsets
const CONCRETIZATION_PATTERNS_FILE   = normpath(@__DIR__, "fixtures", "concretization_patterns.jl")
const CONCRETIZATION_PATTERNS_CONFIG = normpath(@__DIR__, "fixtures", "..JET.toml")

@testset "custom concretization pattern" begin
    # the analysis on `test/fixtures/concretization_patterns.jl` will produce inappropriate
    # top-level error report because of missing concretization
    let
        res = analyze_file(CONCRETIZATION_PATTERNS_FILE)
        @test length(res.toplevel_error_reports) == 1
        let r = first(res.toplevel_error_reports)
            @test isa(r, MissingConcretization)
        end
    end

    # we can circumvent the issue by using the `concretization_patterns` configuration !
    let
        res = analyze_file(CONCRETIZATION_PATTERNS_FILE;
                           concretization_patterns = [:(GLOBAL_CODE_STORE = x_)],
                           )
        @test isempty(res.toplevel_error_reports)
    end

    # we can specify whatever pattern `@capture` can accept
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel vmod begin
            foo(a) = a
            foo(10)
            bar(a) = a
        end concretization_patterns = [:x_]
        @test is_concrete(vmod, :foo)
        @test is_concrete(vmod, :bar)
    end
end

# NOTE this test is only valid when the testset above passes, better to be in a separate file though
@testset "configuration file" begin
    dir = mktempdir()
    analysis_target = normpath(dir, "concretization_patterns.jl")
    config_target   = normpath(dir, JET.CONFIG_FILE_NAME)

    back = pwd()
    try
        # in order to check the functionality, fixtures/..JET.toml logs toplevel analysis
        # into toplevel.txt (relative to the current working directory)
        # and so let's cd into the temporary directory to not pollute this directory
        old = cd(dir)

        open(CONCRETIZATION_PATTERNS_FILE) do f
            write(analysis_target, f)
        end

        # no configuration, thus top-level analysis should fail
        let
            io = IOBuffer()
            _, res = report_file(io, analysis_target)
            @test res # error reported
        end

        # setup a configuration file
        open(CONCRETIZATION_PATTERNS_CONFIG) do f
            write(config_target, f)
        end

        # now any top-level analysis failure shouldn't happen
        let
            io = IOBuffer()
            _, res = report_file(io, analysis_target)
            @test !res # no error happened
            @test isfile("toplevel.txt") # not closed yet
        end
    catch err
        rethrow(err)
    finally
        cd(back)
    end
end
