module test_virtualprocess

include("../setup.jl")

@testset "syntax error reports" begin
    let s = """
        begin
            c = rand(Bool)
        end

        end
        """ |> strip

        res = report_text(s)
        report = only(res.res.toplevel_error_reports)
        @test report isa SyntaxErrorReport
        @test report.line == 5
    end
end

@testset "virtualize module context" begin
    # the context of the original module should be virtualized
    let actual = gen_virtual_module(@__MODULE__)
        Core.eval(actual, quote
            struct X end
            const Y = nothing
            Z() = return
        end)

        virtual = virtualize_module_context(actual)
        @test isdefined(virtual, :X)
        @test isdefined(virtual, :Y)
        @test isdefined(virtual, :Z)
    end

    # in the virtualized context, we can define a name that is already defined in the original module
    let actual = gen_virtual_module(@__MODULE__)
        Core.eval(actual, quote
            struct X end
            const Y = nothing
            Z() = return
        end)

        virtual = virtualize_module_context(actual)
        @test (Core.eval(virtual, quote
            struct X end
            const Y = nothing
            Z() = return
        end); true)
    end

    # end to end
    let orig = gen_virtual_module(@__MODULE__)
        Core.eval(orig, Expr(:toplevel, (quote
            module Foo
            bar() = return :baz
            export bar
            end
        end).args...))

        res = @analyze_toplevel context = orig begin
            module Foo
            bar() = return :baz
            export bar
            end
        end

        # "cannot assign a value to variable orig.Foo from module orig" shouldn't be reported
        @test isempty(res.res.toplevel_error_reports)
    end

    # don't error if there is undefined export
    let actual = gen_virtual_module(@__MODULE__)
        Core.eval(actual, :(export undefined))

        @test (virtualize_module_context(actual); true)

        res = @analyze_toplevel context = actual begin
            println(undefined)
        end

        @test isempty(res.res.toplevel_error_reports)
        @test any(res.res.inference_error_reports) do err
            isa(err, UndefVarErrorReport) &&
            err.var isa GlobalRef && err.var.name === :undefined
        end
    end
end

@testset "test to_simple_module_usages" begin
    @test JET.to_simple_module_usages(:(using A, B)) ==
        Expr[:(using A), :(using B)]
    @test JET.to_simple_module_usages(:(export a, b)) ==
        Expr[:(export a), :(export b)]
    @test JET.to_simple_module_usages(:(export a)) ==
        Expr[:(export a)]
    @test JET.to_simple_module_usages(:(import Pkg as P)) ==
        Expr[:(import Pkg as P)]
    @test JET.to_simple_module_usages(:(using A)) ==
        Expr[:(using A)]
    @test JET.to_simple_module_usages(:(using A: sym1, sym2)) ==
        Expr[:(using A: sym1), :(using A: sym2)]
    @test JET.to_simple_module_usages(:(import Pkg: status as s, gc as g)) ==
        Expr[:(import Pkg: status as s), :(import Pkg: gc as g)]
end

@testset "fix self-reference of virtual module" begin
    let res = @analyze_toplevel begin
            const foo = sum
            Main.foo("julia") # `Main.sum` should be resolved as constant
        end
        test_sum_over_string(res)
    end

    let res = @analyze_toplevel begin
            let Main = "julia" # local `Main` should not be resolved to virtual module
                sum(Main)
            end
        end
        test_sum_over_string(res)
    end

    # https://github.com/aviatesk/JET.jl/issues/151
    let res = @analyze_toplevel begin
            struct X end

            module A
            using ..Main: X
            end
        end

        @test isempty(res.res.toplevel_error_reports)
    end

    let res = @analyze_toplevel begin
            struct X end

            module A
            using ..Main: X as X′
            end
        end

        @test isempty(res.res.toplevel_error_reports)
    end

    # stress test
    let res = @analyze_toplevel begin
            module A
            struct X end
            struct Y end
            export X, y
            end

            module B1
            import ..Main.A
            println(A.X)
            end
            module B2
            using ..Main.A
            println(A.X)
            end

            module C11
            import ..Main.A: X
            println(X)
            end
            module C12
            import ..Main.A: X, Y
            println(X, Y)
            end
            module C21
            using ..Main.A: X
            println(X)
            end
            module C22
            using ..Main.A: X, Y
            println(X, Y)
            end

            module D11
            import ..Main.A: X as x
            println(x)
            end
            module D12
            import ..Main.A: X as x, Y as y
            println(x, y)
            end
            module D21
            using ..Main.A: X as x
            println(x)
            end
            module D22
            using ..Main.A: X as x, Y as y
            println(x, y)
            end
        end

        @test isempty(res.res.toplevel_error_reports)
    end
end

@testset "fix toplevel global `Symbol`" begin
    # this case otherwise will throw an error in `CC.typ_for_val` in optimization
    let res = @analyze_toplevel begin
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
    let res = @analyze_toplevel begin
            c = false
            if c
                throw("should be ignored")
            end
        end

        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "'toplevel definitions'" begin
    let
        vmod, = @analyze_toplevel2 begin
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
        vmod, res = @analyze_toplevel2 begin
            gb = rand(Bool)

            struct Foo
                b::Bool
                Foo(b = gb) = new(b)
            end

            foo = Foo(gb)
        end

        # global variables aren't evaluated but kept in `analyzer` instead
        @test isa_abstract(vmod, :gb, Bool)
        @test isa_concrete(vmod, :Foo, Type)
        @test isa_abstract(vmod, :foo, vmod.Foo)
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    # definitions using type aliases
    let
        vmod, = @analyze_toplevel2 begin
            const BT = Bool

            struct Foo
                b::BT
            end

            const c = Foo(rand(Bool))
            uc = Foo(rand(Bool))
        end

        @test is_concrete(vmod, :BT)
        @test isa_concrete(vmod, :Foo, Type)
        @test isa_abstract(vmod, :c, vmod.Foo)
        @test isa_abstract(vmod, :uc, vmod.Foo)
    end

    @testset "toplevel definitions within a block" begin
        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    struct Foo
                        bar
                    end

                    foo = Foo(:bar)
                    println(foo)
                end
            end

            @test isempty(res.res.toplevel_error_reports)
            @test isa_concrete(vmod, :Foo, Type)
            @test isa_analyzed(vmod, :foo, vmod.Foo)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    abstract type Foo end
                    struct Foo1 <: Foo
                        foo
                    end

                    foo = Foo1(:foo)
                    println(foo)
                end
            end

            @test isempty(res.res.toplevel_error_reports)
            @test isa_concrete(vmod, :Foo, Type)
            @test isa_concrete(vmod, :Foo1, Type)
            @test isa_analyzed(vmod, :foo, vmod.Foo1)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    import Base: getproperty
                    struct Foo
                        bar
                    end
                    getproperty(foo::Foo, sym::Symbol) =
                        sym === :baz ? getfield(foo, :bar) : getfield(foo, :bar)
                end
                bar = Foo(gensym()).baz
            end

            @test isempty(res.res.toplevel_error_reports)
            @test isa_concrete(vmod, :Foo, Type)
            @test is_abstract(vmod, :bar)
        end

        let
            vmod, res = mktemp() do path, io
                write(io, quote
                    struct Foo
                        bar
                    end
                end |> string)
                flush(io)

                @eval @analyze_toplevel2 begin
                    begin
                        include($path)
                        getbar(foo::Foo) = foo.bar
                    end
                    bar = getbar(Foo(gensym()))
                end
            end

            @test isempty(res.res.toplevel_error_reports)
            @test isa_concrete(vmod, :Foo, Type)
            @test is_abstract(vmod, :bar)
        end
    end
end

@testset "macro expansions" begin
    let
        vmod, res = @analyze_toplevel2 begin
            @inline foo(a) = identity(a)
            foo(10)
        end

        @test is_concrete(vmod, :foo)
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let
        vmod, res = @analyze_toplevel2 begin
            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                push!(ex.args, 1)
                return ex
            end

            @foo sin() # otherwise NoMethodError
        end

        @test is_concrete(vmod, Symbol("@foo"))
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    # macro expansions with access to global variables will fail
    let
        vmod, res = @analyze_toplevel2 begin
            const arg = rand(Bool)

            macro foo(ex)
                @assert Meta.isexpr(ex, :call)
                arg && push!(ex.args, 1)
                return ex
            end

            @foo sin()
        end

        @test is_concrete(vmod, Symbol("@foo"))
        let r = only(res.res.toplevel_error_reports)
            @test isa(r, MissingConcretization) # this error should be considered as missing concretization
        end
        @test isempty(res.res.inference_error_reports)
    end

    # macros that can general :module or :toplevel expressions
    let # if we don't expand macros before we check `:toplevel` or `:module` expressions,
        # we may pass `:toplevel` or `:module` expressions to `partially_interpret!` and
        # eventually we will fail to concretize them and their toplevel definitions
        vmod, res = @analyze_toplevel2 begin
            macro wrap_in_mod(blk)
                ex = Expr(:module, true, esc(:foo), esc(blk))
                return Expr(:toplevel, ex)
            end

            @wrap_in_mod begin
                bar() = nothing
            end
        end
        @test is_concrete(vmod, :foo)
        @test is_concrete(vmod.foo, :bar)

        vmod, res = @analyze_toplevel2 begin
            """
                foo

            Docstring for module `foo`
            """
            module foo
            bar() = nothing
            end
        end
        @test is_concrete(vmod, :foo)
        @test is_concrete(vmod.foo, :bar)
    end
end

@testset "remove `const`" begin
    let
        res = @analyze_toplevel begin
            const s = "julia"
            sum(s)
        end

        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        res = @analyze_toplevel begin
            let
                const s = "julia"
                sum(s)
            end
        end

        @test !isempty(res.res.toplevel_error_reports)
        @test only(res.res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "expression flattening" begin
    @test (let
        # internal error shouldn't occur
        res = @analyze_toplevel begin
            r = rand(); s = sin(a); c = cos(b); tan(c)
        end
    end; true)
end

@testset "handle `include`" begin
    let
        f1 = normpath(FIXTURES_DIR, "include1.jl")
        f2 = normpath(FIXTURES_DIR, "include2.jl")

        context = gen_virtual_module(@__MODULE__)
        res = report_file2(f1; context, virtualize = false)

        @test f1 in res.res.included_files
        @test f2 in res.res.included_files
        @test is_concrete(context, :foo)
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let
        f = normpath(FIXTURES_DIR, "nonexistinclude.jl")
        res = report_file2(f)

        @test f in res.res.included_files
        @test !isempty(res.res.toplevel_error_reports)
        @test first(res.res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.res.inference_error_reports)
        @test first(res.res.inference_error_reports) isa UndefVarErrorReport
    end

    let
        f = normpath(FIXTURES_DIR, "selfrecursiveinclude.jl")
        res = report_file2(f)

        @test f in res.res.included_files
        @test !isempty(res.res.toplevel_error_reports)
        @test first(res.res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURES_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURES_DIR, "chainrecursiveinclude2.jl")
        res = report_file2(f1)

        @test f1 in res.res.included_files
        @test f2 in res.res.included_files
        @test !isempty(res.res.toplevel_error_reports)
        let report = only(res.res.toplevel_error_reports)
            @test report isa RecursiveIncludeErrorReport
            @test report.duplicated_file == f1
            @test f1 in report.files
            @test f2 in report.files
            @test report.file == f2
            @test report.line == 1
        end
    end

    let
        f1 = normpath(FIXTURES_DIR, "includetwice.jl")
        f2 = normpath(FIXTURES_DIR, "include2.jl")
        res = report_file2(f1)

        @test f1 in res.res.included_files
        @test f2 in res.res.included_files
        @test isempty(res.res.toplevel_error_reports)
    end

    let
        modf = normpath(FIXTURES_DIR, "modinclude.jl")
        inc2 = normpath(FIXTURES_DIR, "include2.jl")

        context = gen_virtual_module(@__MODULE__)
        res = report_file2(modf; context, virtualize=false)

        @test modf in res.res.included_files
        @test inc2 in res.res.included_files
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
        @test is_concrete(context.Outer, :foo)
    end

    # bad includes
    # ------------

    let res = @analyze_toplevel begin
            include(Symbol("somefile.jl"))
        end
        @test length(res.res.toplevel_error_reports) == 1
        report = only(res.res.toplevel_error_reports)
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Base.include("somefile.jl", __xxx__)
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
        @test err.f === Base.include
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Core.include("somefile.jl", __xxx__)
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
        @test err.f === Core.include
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Base.include(__xxx__, Symbol("somefile.jl"))
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
    end

    # unsupported features
    # --------------------

    let res = @test_logs (:warn,) @analyze_toplevel begin
            function mymapexpr(x::Expr)
                println(x)
                nothing
            end
            include(mymapexpr, "somefile.jl")
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
    res = report_text(s)
    @test isempty(res.res.toplevel_error_reports)
end

@testset "module usage" begin
    # using
    let res = @analyze_toplevel begin
            module foo

            using Base.Meta: isexpr

            isexpr(:(foo(bar)), :call)
            isexpr2(:(foo(bar)), :call)

            end
        end

        @test isempty(res.res.toplevel_error_reports)
        report = only(res.res.inference_error_reports)
        @test report isa UndefVarErrorReport
        @test occursin("isexpr2", get_msg(report))
    end

    # error handling for module usages
    let res = @analyze_toplevel begin
            using Base: foo
        end

        @test !isempty(res.res.toplevel_error_reports)
        er = first(res.res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err isa UndefVarError && er.err.var === :foo
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 7
    end

    # sequential usage
    let res = @analyze_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo

            bar("julia") # -> UndefVarErrorReport

            end # module bar

            end # module foo
        end

        @test isempty(res.res.toplevel_error_reports)
        @test !isempty(res.res.inference_error_reports)
        @test only(res.res.inference_error_reports) isa UndefVarErrorReport
    end

    # usage of global objects
    let res = @analyze_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo

            foo.bar("julia") # -> MethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let res = @analyze_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            using ..foo: bar

            bar("julia") # -> MethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # module usage within a block
    let res = @analyze_toplevel begin
            module foo

            bar(s) = sum(s)

            module baz

            begin
                using ..foo: bar
                bar("julia") # -> MethodErrorReports
            end

            end # module bar

            end # module foo
        end

        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    @testset "module usage of abstract global variable" begin
        let res = @analyze_toplevel begin
                module foo

                const bar = sum

                module baz

                using ..foo: bar

                bar("julia")

                end # module bar

                end # module foo
            end

            @test isempty(res.res.toplevel_error_reports)
            test_sum_over_string(res)
        end

        let res = @analyze_toplevel begin
                module foo

                const bar = "julia"

                module baz

                using ..foo: bar

                sum(bar)

                end # module bar

                end # module foo
            end

            @test isempty(res.res.toplevel_error_reports)
            test_sum_over_string(res)
        end

        let res = @analyze_toplevel begin
                module foo

                const bar = "julia"

                export bar

                end # module foo

                using .foo
                sum(bar)
            end

            @test isempty(res.res.toplevel_error_reports)
            test_sum_over_string(res)
        end
    end

    # export
    let res = @analyze_toplevel begin
            module foo

            bar(s) = sum(s)

            export bar

            end

            using .foo

            bar("julia") # -> MethodErrorReports
        end

        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "sequential" begin
    let res = @analyze_toplevel begin
            foo(1:1000)

            foo(a) = length(a)
        end
        @test only(res.res.inference_error_reports) isa UndefVarErrorReport
    end

    let res = @analyze_toplevel begin
            usefunc(a) = sin(func_impl(a))

            func_impl(a) = length(a)
            usefunc("julia") # should not error

            func_impl(a) = a
            usefunc(42)  # should not error too
        end
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "abstract global variables" begin
    let
        vmod, res = @analyze_toplevel2 begin
            var = rand(Bool)
            const constvar = rand(Bool)
        end

        @test is_abstract(vmod, :var)
        @test isa_abstract(vmod, :var, Bool)
        @test is_abstract(vmod, :constvar)
        @test isa_abstract(vmod, :constvar, Bool)
    end

    @testset "scope" begin
        # function
        # --------

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local localvar = rand(Bool)
                    global globalvar = rand(Bool)
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end

        # blocks
        # ------

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    globalvar = rand(Bool)
                end
            end

            @test isa_abstract(vmod, :globalvar, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local localvar = rand(Bool)
                    globalvar = localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local localvar
                    localvar = rand(Bool) # this shouldn't be annotated as `global`
                    globalvar = localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                globalvar2 = begin
                    local localvar = rand(Bool)
                    globalvar1 = localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar1, Bool)
            @test isa_abstract(vmod, :globalvar2, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                let
                    localvar = rand(Bool)
                end
            end

            @test !is_analyzed(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                globalvar = let
                    localvar = rand(Bool)
                    localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end

        # loops
        # -----

        let
            vmod, res = @analyze_toplevel2 begin
                for i = 1:100
                    localvar = i
                end
            end

            @test !is_analyzed(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                for i in 1:10
                    localvar = rand(Bool)
                    global globalvar = localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                while true
                    localvar = rand(Bool)
                end
            end

            @test !is_analyzed(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                while true
                    localvar = rand(Bool)
                    global globalvar = localvar
                end
            end

            @test !is_analyzed(vmod, :localvar)
            @test isa_abstract(vmod, :globalvar, Bool)
        end
    end

    @testset "multiple declaration/assignment" begin
        let
            vmod, res = @analyze_toplevel2 begin
                r1, r2 = rand(2)
            end

            @test isa_abstract(vmod, :r1, Float64)
            @test isa_abstract(vmod, :r2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local r1, r2
                    r1, r2 = rand(2)
                end
            end

            @test !is_analyzed(vmod, :r1)
            @test !is_analyzed(vmod, :r2)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                let
                    global r1, r2
                    r1, r2 = rand(2)
                end
            end

            @test isa_abstract(vmod, :r1, Float64)
            @test isa_abstract(vmod, :r2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                ro1, ro2 = let
                    ri1, ri2 = rand(2)
                end
            end

            @test !is_analyzed(vmod, :ri1)
            @test !is_analyzed(vmod, :ri2)
            @test isa_abstract(vmod, :ro1, Float64)
            @test isa_abstract(vmod, :ro2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local l
                    l, g = rand(2)
                end
            end
            @test !is_analyzed(vmod, :l)
            @test isa_abstract(vmod, :g, Float64)
        end
    end

    @testset "concretize statically constant variables" begin
        let
            m, = @analyze_toplevel2 begin
                const a = 0
            end
            @test is_concrete(m, :a) && m.a == 0
        end

        # try to concretize even if it's not declared as constant
        let
            m, = @analyze_toplevel2 begin
                a = 0
            end
            @test is_concrete(m, :a) && m.a == 0
        end

        let
            m, = @analyze_toplevel2 begin
                const a = :jetzero # should be quoted, otherwise undef var error
            end
            @test is_concrete(m, :a) && m.a === :jetzero
        end

        # sequential
        let
            m, = @analyze_toplevel2 begin
                a = rand(Int)
                a = 0
            end
            @test is_concrete(m, :a) && m.a == 0
        end
        let
            m, = @analyze_toplevel2 begin
                a = 0
                a = rand(Int)
            end
            @test isa_abstract(m, :a, Int)
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

        @test isempty(res.res.toplevel_error_reports)
    end

    @testset "https://github.com/aviatesk/JET.jl/issues/280" begin
        res = @analyze_toplevel begin
            using Libdl
            let
                llvmpaths = filter(lib -> occursin(r"LLVM\b", basename(lib)), Libdl.dllist())
                if length(llvmpaths) != 1
                    throw(ArgumentError("Found one or multiple LLVM libraries"))
                end
                libllvm = Libdl.dlopen(llvmpaths[1])
                gethostcpufeatures = Libdl.dlsym(libllvm, :LLVMGetHostCPUFeatures)
                ccall(gethostcpufeatures, Cstring, ())
            end
        end

        @test isempty(res.res.toplevel_error_reports)
    end
end

let # global declaration
    res = @analyze_toplevel begin
        global var
    end
    @test isempty(res.res.toplevel_error_reports)
end

@testset "toplevel throw" begin
    res = @analyze_toplevel begin
        throw("throw me")
    end
    @test only(res.res.inference_error_reports) isa UncaughtExceptionReport
end

@testset "error handling within ConcreteInterpreter" begin
    # NOTE some of the tests below are line-number-sensitive

    let
        res = @analyze_toplevel begin
            struct A <: B end # UndefVarError(:B) should be handled into `res.toplevel_error_reports`
        end

        er = only(res.res.toplevel_error_reports)
        @test er isa ActualErrorWrapped
        @test er.err isa UndefVarError && er.err.var === :B
        @test er.file == (@__FILE__) && er.line == (@__LINE__) - 6
    end

    @testset "stacktrace scrubbing" begin
        # scrub internal frames until (errored) user macro
        let
            res = @analyze_toplevel begin
                macro badmacro(s) throw(s) end # L1
                @badmacro "hi"                 # L2
            end

            er = only(res.res.toplevel_error_reports)
            @test er isa ActualErrorWrapped
            @test er.file == (@__FILE__) && er.line == (@__LINE__) - 5 # L2
            @test length(er.st) == 1
            sf = first(er.st)
            @test sf.file === Symbol(@__FILE__) && sf.line == (@__LINE__) - 9 # L1
            @test sf.func === Symbol("@badmacro")
        end

        # TODO add test for `eval_with_err_handling` and `lower_with_err_handling`

        # scrub all the internal frame when errors happens in `maybe_evaluate_builtin`
        let res = @analyze_toplevel begin
                struct A
                    fld::UndefinedType
                end
            end

            er = only(res.res.toplevel_error_reports)
            @test er isa ActualErrorWrapped
            @test er.err isa UndefVarError && er.err.var === :UndefinedType
            @test er.file == (@__FILE__) && er.line == (@__LINE__) - 8
            @test isempty(er.st)
        end

        # errors from user functions (i.e. those from `@invokelatest f(fargs...)` in the overload
        # `JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)`)
        let res = @analyze_toplevel begin
                foo() = throw("don't call me, pal")
                struct A <: foo() end
            end

            er = only(res.res.toplevel_error_reports)
            @test er isa ActualErrorWrapped
            @test er.err == "don't call me, pal"
            @test er.file == (@__FILE__) && er.line == (@__LINE__) - 6
            sf = only(er.st)
            @test sf.file === Symbol(@__FILE__) && sf.line == (@__LINE__) - 9
        end
    end
end

@testset "invalid constant redefinition/declaration" begin
    # for abstract global assignment
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            const foo = fib(1000000000000) # ::Int
            foo = fib(1000000000000.) # ::Float64
        end

        @test is_analyzed(vmod, :foo)
        er = only(res.res.inference_error_reports)
        @test er isa InvalidConstantRedefinition
        @test er.name === :foo
    end
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            foo = fib(1000000000000.) # ::Float64
            const foo = fib(1000000000000) # ::Int
        end

        @test is_analyzed(vmod, :foo)
        er = only(res.res.inference_error_reports)
        @test er isa InvalidConstantDeclaration
        @test er.name === :foo
    end

    # for concretized constants
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            const T = typeof(fib(1000000000000)) # never terminates
            T = Nothing
        end

        @test is_analyzed(vmod, :T) # this can even be concretized
        er = only(res.res.inference_error_reports)
        @test er isa InvalidConstantRedefinition
        @test er.name === :T
    end
    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
            fib(n) = n≤2 ? n : fib(n-1)+fib(n-1)
            T = Nothing
            const T = typeof(fib(1000000000000)) # never terminates
        end

        @test is_analyzed(vmod, :T) # this can even be concretized
        er = only(res.res.inference_error_reports)
        @test er isa InvalidConstantDeclaration
        @test er.name === :T
    end

    let
        vmod = gen_virtual_module()
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
            a = 0
            const a = 1
        end
        let er = only(res.res.inference_error_reports)
            @test er isa InvalidConstantDeclaration
            @test er.name === :a
        end
    end
end

@testset "non-deterministic abstract global assignments" begin
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = 0 # deterministic
        end
        @test isa_analyzed(vmod, :v, Int)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = rand(Int) # deterministic
        end
        @test isa_analyzed(vmod, :v, Int)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = 0) # non-deterministic
        end
        @test isa_analyzed(vmod, :v, Union{Char,Int})
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = rand(Int)) # non-deterministic
        end
        @test isa_analyzed(vmod, :v, Union{Char,Int})
    end

    # end to end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = rand(Int) # deterministic

            f(::Number) = :ok
            f(v) # no method error should NOT happen here
        end

        @test isempty(res.res.inference_error_reports)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = rand(Int)) # non-deterministic

            f(::Number) = :ok
            f(v) # union-split no method error should happen here
        end

        er = only(res.res.inference_error_reports)
        @test er isa MethodErrorReport
        @test isa(er.t, Vector)
    end
end

@testset "docstrings" begin
    # can find errors within docstring generation
    let
        vmod, res = @analyze_toplevel2 begin
            """
                foo

            This is a invalid docstring for `$(foo)`,
            becuase `foo` is undefined at this point.
            """
            :(foo)
        end
        r = only(res.res.inference_error_reports)
        @test r isa UndefVarErrorReport
        @test r.var isa GlobalRef && r.var.name === :foo
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
    @test isempty(res.res.toplevel_error_reports)
end

@testset "avoid too much bail out from `_virtual_process!`" begin
    let
        res = @analyze_toplevel begin
            sin′
        end
        @test !isempty(res.res.inference_error_reports)
    end

    let
        res = @analyze_toplevel begin
            s = nothing
            sin′
        end
        @test !isempty(res.res.inference_error_reports)
    end
end

# will be used in the following two testsets
let fixtures_dir = normpath(@__DIR__, "..", "fixtures")
    global const CONCRETIZATION_PATTERNS_FILE   = normpath(fixtures_dir, "concretization_patterns.jl")
    global const CONCRETIZATION_PATTERNS_CONFIG = normpath(fixtures_dir, "..JET.toml")
end

@testset "custom concretization pattern" begin
    # custom concretization pattern should work on AST level
    let (vmod, res) = @analyze_toplevel2 begin
            const foo = Dict() # won't be concretized by default
        end
        @test !is_concrete(vmod, :foo)
    end
    let (vmod, res) = @analyze_toplevel2 begin
            const foo = Dict() # now this will be forcibly concretized
        end concretization_patterns = [:(const foo = Dict())]
        @test is_concrete(vmod, :foo)
    end

    # the analysis on `test/fixtures/concretization_patterns.jl` will produce inappropriate
    # top-level error report because of missing concretization
    let res = report_file2(CONCRETIZATION_PATTERNS_FILE)
        let r = only(res.res.toplevel_error_reports)
            @test isa(r, MissingConcretization)
        end
    end

    # we can circumvent the issue by using the `concretization_patterns` configuration !
    let res = report_file2(CONCRETIZATION_PATTERNS_FILE;
                           concretization_patterns = [:(const GLOBAL_CODE_STORE = Dict())],
                           )
        @test isempty(res.res.toplevel_error_reports)
    end

    # we can specify whatever pattern `@capture` can accept
    let (vmod, res) = @analyze_toplevel2 begin
            foo() = rand((1,2,3))
            a = foo()
            b = foo()
        end concretization_patterns = [:x_] # means "concretize everything"
        @test is_concrete(vmod, :a)
        @test is_concrete(vmod, :b)
    end

    # `concretization_patterns` should "intuitively" work for code with documentations attached
    let (vmod, res) = @analyze_toplevel2 begin
            """
                foo

            This is a documentation for `foo`
            """
            const foo = Dict()
        end concretization_patterns = [:(const foo = Dict())]
        @test is_concrete(vmod, :foo)
    end
end

# NOTE better to be in test_misc.jl, but here it is since this test is only valid when the testset above gets passed
@testset "configuration file" begin
    mktempdir() do dir
        analysis_target = normpath(dir, "concretization_patterns.jl")
        config_target   = normpath(dir, JET.CONFIG_FILE_NAME)

        back = pwd()
        try # in order to check the functionality, fixtures/..JET.toml logs toplevel analysis
            # into toplevel.txt (relative to the current working directory)
            # and so let's `cd` into the temporary directory to not pollute this directory
            cd(dir)

            open(CONCRETIZATION_PATTERNS_FILE) do f
                write(analysis_target, f)
            end

            # no configuration, thus top-level analysis should fail
            let res = report_file2(analysis_target)
                nreported = print_reports(IOBuffer(), res)
                @test !iszero(nreported) # error reported
            end

            # setup a configuration file
            open(CONCRETIZATION_PATTERNS_CONFIG) do f
                write(config_target, f)
            end

            # now any top-level analysis failure shouldn't happen
            let res = report_file2(analysis_target)
                nreported = print_reports(IOBuffer(), res)
                @test iszero(nreported) # no error happened
                @test isfile("toplevel.txt") # not closed yet
            end
        finally
            cd(back)
        end
    end
end

@testset "`collect_toplevel_signature!`" begin
    res = @analyze_toplevel analyze_from_definitions=false begin
        foo() = return
        bar(a) = return a
        baz(a) = return a
        baz(a::Int) = return a
        qux(a::T) where T<:Integer = return a
    end
    @test isempty(res.res.toplevel_signatures)

    vmod, res = @analyze_toplevel2 analyze_from_definitions=true begin
        foo() = return
        bar(a) = return a
        baz(a) = return a
        baz(a::Int) = return a
        qux(a::T) where T<:Integer = return a
    end
    @test !isempty(res.res.toplevel_signatures)
    @test any(res.res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.foo)}
    end
    @test any(res.res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.bar),Any}
    end
    @test any(res.res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.baz),Any}
    end
    @test any(res.res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.baz),Int}
    end
    @test any(res.res.toplevel_signatures) do sig
        sig <: Tuple{typeof(vmod.qux),Integer} # `Tuple{typeof(vmod.qux),TypeVar}`
    end

    vmod, res = @analyze_toplevel2 analyze_from_definitions=true begin
        # borrowed from LinearAlgebra's definition
        struct Diagonal{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
            diag::V
        end
        function issue474(out::Vector{Diagonal{T, V}}, A::Diagonal{T, V}, k::Integer;
                          caches = nothing) where {T <: Number, V <: AbstractVector{T}}
            nothing
        end
    end
    @test !isempty(res.res.toplevel_signatures)
    @test all(res.res.toplevel_signatures) do @nospecialize sig
        !Base.has_free_typevars(sig)
    end
end

@testset "`analyze_from_definitions`" begin
    let res = @analyze_toplevel analyze_from_definitions=false begin
            foo() = return undefvar
        end
        @test isempty(res.res.inference_error_reports)

        res = @analyze_toplevel analyze_from_definitions=true begin
            foo() = return undefvar
        end
        @test !isempty(res.res.inference_error_reports)
        let r = only(res.res.inference_error_reports)
            @test is_global_undef_var(r, :undefvar)
        end
    end

    # COMBAK use different aggregation policy for "user-script" analysis ?
    let res = @analyze_toplevel analyze_from_definitions=true begin
            foo(a) = b # typo
            bar() = foo("julia")
        end
        @test length(res.res.inference_error_reports) ≥ 1
        @test_broken any(res.res.inference_error_reports) do r # report analyzed from `foo`
            is_global_undef_var(r, :b) && length(err.vst) == 1
        end &&       any(res.res.inference_error_reports) do r # report analyzed from `bar`
            is_global_undef_var(r, :b) && length(err.vst) == 2
        end
    end

    let res = @analyze_toplevel analyze_from_definitions=true begin
            foo(a) = sum(a)
            bar() = foo("julia")
        end
        test_sum_over_string(res)
    end

    # avoid error report from branching on the return value of uninferred comparison operator calls
    # https://github.com/aviatesk/JET.jl/issues/542
    let res = @analyze_toplevel analyze_from_definitions=true ignore_missing_comparison=true begin
            struct Issue542 end
            isa542(x) = x == Issue542() ? true : false
        end
        @test isempty(res.res.inference_error_reports)
    end
    let res = @analyze_toplevel analyze_from_definitions=true ignore_missing_comparison=true begin
            struct Issue542; x; end
            isa542(x) = x in (Issue542, Issue542) ? true : false
        end
        @test isempty(res.res.inference_error_reports)
    end
    # xref: https://github.com/JuliaGraphs/Graphs.jl/pull/249#issuecomment-1605290837
    let res = @analyze_toplevel analyze_from_definitions=true ignore_missing_comparison=true begin
            f(a::Vector{T}, b::Vector{T}) where T<:Integer = a == b ? true : false
        end
        @test isempty(res.res.inference_error_reports)
    end
    # make sure we get the error report from the interactive entry
    let res = report_call((Any, Some{Any})) do x, y
            x == y ? true : false
        end
        @test isa(only(get_reports_with_test(res)), NonBooleanCondErrorReport)
    end

    # https://github.com/aviatesk/JET.jl/issues/426
    let res = @analyze_toplevel analyze_from_definitions=true begin
            function f1(func, x::String)
                return 1 + x
            end

            function f2(x::String)
                return 1 + x
            end
        end
        reports = res.res.inference_error_reports
        @test length(reports) == 2
        @test all(r->isa(r,MethodErrorReport), reports)
    end

    # avoid unassigned keyword argument error from top-level definition analysis
    # https://github.com/aviatesk/JET.jl/issues/487
    let res = @analyze_toplevel analyze_from_definitions=true begin
            struct Bar
                x
            end
            Bar(; x) = Bar(x)
        end
        @test isempty(res.res.inference_error_reports)
    end

    # avoid error report from methods that are written to throw
    # https://github.com/aviatesk/JET.jl/issues/477
    let res = @analyze_toplevel analyze_from_definitions=true begin
            function foo end
            @noinline foo(::T) where T = error(lazy"`foo(::$T)` is not implemented")
        end
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "top-level statement selection" begin
    # simplest example
    let # global function
        vmod, res = @analyze_toplevel2 begin
            foo() = return # should be concretized
        end
        @test is_concrete(vmod, :foo)

        # inner function
        vmod, res = @analyze_toplevel2 analyze_from_definitions=true let
            foo(a) = a # should be concretized, and its signature should have been collected
        end
        @test length(res.res.toplevel_signatures) == 1
    end

    # simple negative case test, which checks we do NOT select statements not involved with any definition
    # this particular example is adapted from https://en.wikipedia.org/wiki/Program_slicing
    let src = @src let
            N = 10
            sum = 0
            product = 1 # should NOT be selected
            w = 7
            for i in 1:N
                sum += i + w
                product *= i # should NOT be selected
            end
            @eval global getsum() = $sum # concretization is forced
            write(product) # should NOT be selected
        end
        slice = JET.select_statements(src)

        found_N = found_sum = found_product = found_w = found_write = false
        for (i, stmt) in enumerate(src.code)
            if JET.isexpr(stmt, :(=))
                lhs, rhs = stmt.args
                if isa(lhs, Core.SlotNumber)
                    if src.slotnames[lhs.id] === :w
                        found_w = true
                        @test slice[i]
                    elseif src.slotnames[lhs.id] === :sum
                        found_sum = true
                        @test slice[i]
                    elseif src.slotnames[lhs.id] === :N
                        found_N = true
                        @test slice[i]
                    elseif src.slotnames[lhs.id] === :product
                        found_product = true
                        @test !slice[i]
                    end
                end
            elseif JET.@capture(stmt, write(x_))
                found_write = true
                @test !slice[i]
            elseif (JET.isexpr(stmt, :call) && (arg1 = stmt.args[1]; arg1 isa Core.SSAValue) &&
                src.code[arg1.id] === :write)
                found_write = true
                @test !slice[i]
            end
        end
        @test found_N; @test found_sum; @test found_product; @test found_w; @test found_write
    end
    let s = mktemp() do path, io # high level test
            redirect_stdout(io) do
                vmod, res = @analyze_toplevel2 let
                    N = 10
                    sum = 0
                    product = 1 # should NOT be selected
                    w = 7
                    for i in 1:N
                        sum += i + w
                        product *= i # should NOT be selected
                    end
                    @eval global getsum() = $sum # concretization is forced
                    println("This should not be printed: ", product) # should NOT be selected
                end
                @test isempty(res.res.toplevel_error_reports)
                @test is_concrete(vmod, :getsum)
            end
            flush(io)
            read(path, String)
        end
        @test isempty(s)
    end

    @testset "captured variables" begin
        let (vmod, res) = @analyze_toplevel2 begin
                begin
                    s = join(rand(Char, 100))
                    foo() = return s
                end
            end
            @test is_concrete(vmod, :foo)
            @test is_abstract(vmod, :s)
        end

        # captured variables for global functions
        let # XXX `s` below aren't necessarily concretized, but concretization of `foo` requires
            # it (since `s` will be embedded into `foo`'s body wrapped in `QuoteNode`)
            # and thus we can't abstract it away as far as we depend on JuliaInterpreter ...
            vmod, res = @analyze_toplevel2 let
                s = "julia"
                global foo() = return s
            end
            @test is_concrete(vmod, :foo)

            vmod, res = @analyze_toplevel2 let
                s = undefvar # actual top-level error is better not to happen here
                global foo() = return s
            end
            @test_broken isempty(res.res.toplevel_error_reports)

            # more realistic example
            vmod, res = @analyze_toplevel2 let s = sprint(showerror, DivideError())
                global errmsg(s = s) = string("error: ", s)
            end
            @test is_concrete(vmod, :errmsg)
            @test isempty(res.res.toplevel_error_reports)
        end
    end

    @testset "conditional control flow" begin
        # ignore last statement of a definition block if possible
        let
            vmod, res = @analyze_toplevel2 if true # force multiple blocks
                foo() = return
                throw("foo")
            end
            @test is_concrete(vmod, :foo)
            @test isempty(res.res.toplevel_signatures)
        end
    end

    # NOTE here we test "try/catch"-involved control flow in a fine-grained level
    # we want to enrich this test suite as much as possible since this control flow often
    # appears in test files with the test macros, while we will do the e2e level tests
    # with those macros
    @testset "try/catch control flow" begin
        # https://github.com/aviatesk/JET.jl/issues/150
        let res = @analyze_toplevel analyze_from_definitions=true try
                foo(a) = sum(a) # essentially same as inner function, should be concretized
                foo("julia") # shouldn't be concretized
            catch err
                err
            end

            @test isempty(res.res.toplevel_error_reports)
            @test length(res.res.toplevel_signatures) == 1
            test_sum_over_string(res)
        end

        # captured variables within a try clause
        let res = @analyze_toplevel analyze_from_definitions=true try
                s = "julia"
                foo(f) = f(s) # should be concretized
                foo(sum) # shouldn't be concretized
            catch err
                err
            end

            @test isempty(res.res.toplevel_error_reports)
            @test length(res.res.toplevel_signatures) == 1
            test_sum_over_string(res)
        end

        # captured variables within a catch clause
        let res = @analyze_toplevel analyze_from_definitions=true try
                s = "julia"
                foo(f) = f(s) # should be concretized
                foo(sum) # shouldn't be concretized
            catch err
                function shows() # should be concretized
                    io = stderr::IO
                    showerror(io, err)
                    showerror(io, err2)
                end
                shows() # shouldn't be concretized
            end

            @test isempty(res.res.toplevel_error_reports)
            @test length(res.res.toplevel_signatures) == 2
            test_sum_over_string(res)
            @test any(r->is_global_undef_var(r, :err2), res.res.inference_error_reports)
        end

        # XXX similar to "captured variables for global functions" cases, but such patterns
        # can be even worse when used within a catch clause; it will just fail into a hell.
        # I include this case just for the reference and won't expect this to work robustly
        # since it heavily depends on Julia's AST lowering and the implementation detail of
        # JuliaInterpreter.jl
        let (vmod, res) = @analyze_toplevel2 try
                s = "julia"
                foo(f) = f(s) # should be concretized
                foo(sum) # shouldn't be concretized
            catch err # will be `MethodError` in actual execution
                global geterr() = return err # should be concretized
            end

            # thanks to JuliaInterpreter's implementation detail, the analysis above won't
            # report top-level errors and can concretize `geterr` even if the actual `err`
            # is not thrown and thus these first two test cases will pass
            @test isempty(res.res.toplevel_error_reports)
            @test isa_concrete(vmod, :geterr, Function) && length(methods(vmod.geterr)) == 1
            # yet we still need to make `geterr` over-approximate an actual execution soundly;
            # currently JET's abstract interpretation special-cases `_INACTIVE_EXCEPTION`
            # and fix it to `Any`, and we test it here in the last test case
            result = report_call(vmod.geterr)
            @test MethodError ⊑ get_result(result)
        end
    end

    @testset "top-level `eval` statement" begin
        let
            vmod, res = @analyze_toplevel2 let
                sym = :foo
                @eval $sym() = :foo # should be concretized
            end
            @test is_concrete(vmod, :foo)
        end
    end

    @testset "toplevel definitions involved within a loop" begin
        let
            vmod, res = @analyze_toplevel2 begin
                # these definitions should be concretized
                for fname in (:foo, :bar, :baz)
                    @eval begin
                        @inline ($(Symbol("is", fname)))(a) = a === $(QuoteNode(fname))
                    end
                end
            end

            @test is_concrete(vmod, :isfoo)
            @test is_concrete(vmod, :isbar)
            @test is_concrete(vmod, :isbaz)
        end

        let
            vmod, res = @analyze_toplevel2 let
                # these definitions should be concretized
                stack = [:foo, :bar, :baz]
                while !isempty(stack)
                    fname = popfirst!(stack)
                    @eval begin
                        @inline ($(Symbol("is", fname)))(a) = a === $(QuoteNode(fname))
                    end
                end
            end

            @test is_concrete(vmod, :isfoo)
            @test is_concrete(vmod, :isbar)
            @test is_concrete(vmod, :isbaz)
        end
    end

    @testset "concretize inplace operations" begin
        let (vmod, res) = @analyze_toplevel2 let
                N = 10
                tpl = (Any[i for i in 1:N]...,)
                @eval gettpl() = $tpl # `tpl` here should be fully concretized
            end
            @test isempty(res.res.toplevel_error_reports)
            @test is_concrete(vmod, :gettpl)
        end

        let (vmod, res) = @analyze_toplevel2 let
                N = 10
                ary = Vector{Any}(undef, N)
                for i in 1:N
                    ary[i] = i
                end
                tpl = (ary...,)
                @eval gettpl() = $tpl # `tpl` here should be fully concretized
            end
            @test isempty(res.res.toplevel_error_reports)
            @test is_concrete(vmod, :gettpl)
        end

        let s = mktemp() do path, io
                redirect_stdout(io) do
                    vmod, res = @analyze_toplevel2 let
                        N = 10
                        S = "This should not be printed\n"
                        tpl1 = (Any[i for i in 1:N]...,)
                        tpl2 = (Any[print(i) for i in S]...,)
                        @eval gettpl1() = $tpl1 # `tpl` here should be fully concretized
                    end
                    @test isempty(res.res.toplevel_error_reports)
                    @test is_concrete(vmod, :gettpl1)
                end
                flush(io)
                read(path, String)
            end
            @test_broken isempty(s) # TODO add_control_flow!
        end
    end

    # code expanded by `@enum` is fairly complex and it couldn't be handled well by JET's
    # general statement selection logic; so currently JET special case it and concretize
    # the whole macro context by pre-defined `concretization_patterns` configurations
    # (xref: originally reported from https://github.com/aviatesk/JET.jl/issues/175)
    @testset "test with @enum macro" begin
        res = @analyze_toplevel begin
            @enum Fruit apple orange
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    # force concretization of type aliases
    @testset "concretization of type aliases" begin
        let res = @analyze_toplevel begin
                const ParamType = Tuple{Real, Vararg{Real}}
                const OptParamType = Vector{ParamType}
                f(o::OptParamType) = true
            end
            @test isempty(res.res.toplevel_error_reports)
        end
        let res = @analyze_toplevel begin
                ParamType = Tuple{Real, Vararg{Real}}
                OptParamType = Vector{ParamType}
                f(o::OptParamType) = true
            end
            @test isempty(res.res.toplevel_error_reports)
        end
    end
end

# in this e2e test suite, we just check usage of test macros doesn't lead to (false positive)
# top-level errors (e.g. world age, etc.)
# TODO
# - leave where these sample code are extracted from
# - add test for `@inferred`
@testset "@test macros" begin
    vmod = Module()
    Core.eval(vmod, :(using Test))

    let # @test
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test true
            @test [1, 2] + [2, 1] == [3, 3]
            @test π ≈ 3.14 atol=0.01
            @test 2 + 2 ≈ 6 atol=1 broken=true
            @test 2 + 2 ≈ 5 atol=1 broken=false
            @test 2 + 2 == 5 skip=true
            @test 2 + 2 == 4 skip=false
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_skip
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_skip 1 == 2
            @test_skip 1 == 2 atol=0.1
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_broken
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_broken 1 == 2
            @test_broken 1 == 2 atol=0.1
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_throws
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_throws BoundsError [1, 2, 3][4]
            @test_throws DimensionMismatch [1, 2, 3] + [1, 2]
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_logs
        res = @analyze_toplevel context = vmod virtualize = false begin
            using Logging: Warn, Debug

            function foo(n)
                @info "Doing foo with n=$n"
                for i=1:n
                    @debug "Iteration $i"
                end
                42
            end
            @test_logs (:info, "Doing foo with n=2") foo(2)
            @test_logs (:info,"Doing foo with n=2") (:debug,"Iteration 1") (:debug,"Iteration 2") min_level=Debug foo(2)
            @test_logs (:info,) (:debug,"Iteration 42") min_level=Debug match_mode=:any foo(100)
            @test (@test_logs (:info,"Doing foo with n=2") foo(2)) == 42

            f() = return
            @test_logs min_level=Warn f()  # test `f` logs no messages when the logger level is warn.
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_warn, @test_nowarn
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test 1234 === @test_nowarn(1234)
            @test 5678 === @test_warn("WARNING: foo", begin println(stderr, "WARNING: foo"); 5678; end)
            let a
                @static if VERSION ≥ v"1.11.0-DEV.867"
                    @test_throws UndefVarError(:a, :local) a
                else
                    @test_throws UndefVarError(:a) a
                end
                @test_nowarn a = 1
                @test a === 1
            end
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @test_deprecated
        res = @analyze_toplevel context = vmod virtualize = false begin
            a() = 1
            b() = 2
            @test_deprecated a()
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @testset
        res = @analyze_toplevel context = vmod virtualize = false begin
            @testset "trigonometric identities" begin
                θ = 2/3*π
                @test sin(-θ) ≈ -sin(θ)
                @test cos(-θ) ≈ cos(θ)
                @test sin(2θ) ≈ 2*sin(θ)*cos(θ)
                @test cos(2θ) ≈ cos(θ)^2 - sin(θ)^2
            end
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let # @testset with actual type-level errors
        res = @analyze_toplevel context = vmod virtualize = false begin
            @testset "JET example" begin
                @test sum("julia") == "julia" # actual errors
            end
        end
        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    let
        # adapted from https://github.com/JuliaLang/julia/blob/ddeba0ba8aa452cb2064eb2d03bea47fe6b0ebbe/test/dict.jl#L469-L478
        res = @analyze_toplevel begin
            using Test
            @testset "IdDict{Any,Any} and partial inference" begin
                a = IdDict{Any,Any}()
                a[1] = a
                a[a] = 2

                ca = copy(a)
                ca = empty!(ca)
                @assert length(a) == 2 # FIXME this statement should be abstracted

                d = @inferred IdDict{Any,Any}(i=>i for i=1:3)
            end
        end
        @test isempty(res.res.toplevel_error_reports)
    end
end

# in this e2e test suite, we run JET against a selection of Julia's base test suite
# (i.e. from https://github.com/JuliaLang/julia/tree/master/test) and check that it won't
# produce (false positive) top-level errors (e.g., by actually running test code, etc.)
# NOTE this could be very fragile ...
@testset "test file targets" begin
    TARGET_DIR = normpath(FIXTURES_DIR, "targets")

    let res = report_file2(normpath(TARGET_DIR, "error.jl"))
        @test isempty(res.res.toplevel_error_reports)
    end

    let res = report_file2(normpath(TARGET_DIR, "dict.jl"))
        @test isempty(res.res.toplevel_error_reports)
    end
end

@testset "@generated function" begin
    genex = :(@generated function foo(a)
        if a <: Integer
            return :(a)
        elseif a <: Number
            return :(undefvar) # report me this case
        end
        throw("invalid argument")
    end)

    let res = @eval @analyze_toplevel begin
            $genex

            foo(100) # generate valid code
        end
        @test isempty(res.res.inference_error_reports)
    end

    let res = @eval @analyze_toplevel begin
            $genex

            foo(100.0) # generate invalid code
        end
        r = only(res.res.inference_error_reports)
        @test is_global_undef_var(r, :undefvar)
    end

    let res = @eval @analyze_toplevel begin
            $genex

            foo("julia") # unsuccessful code generation
        end
        r = only(res.res.inference_error_reports)
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end

    # when `analyze_from_definitions = true`, code generation will likely fail, because we
    # generally can't expect method signatures to be concrete, but the `may_invoke_generator`
    # guard in the `GeneratorErrorReport` error check pass will ignore that case
    let res = @eval @analyze_toplevel analyze_from_definitions=true begin
            $genex
        end
        @test isempty(res.res.inference_error_reports)
    end

    # when `analyze_from_definitions = true`, we can still analyze generator itself
    let res = @analyze_toplevel analyze_from_definitions = true begin
            @generated function foo(a)
                if a <: Integer
                    return :(a)
                elseif t <: Number # `t` is undefined
                    return :(2a)
                end
                throw("invalid argument")
            end
        end
        r = only(res.res.inference_error_reports)
        @test is_global_undef_var(r, :t)
    end
end

using Pkg
function test_report_package(test_func, (pkgname, code);
                             base_setup=function ()
                                Pkg.develop(; path=normpath(FIXTURES_DIR, "PkgAnalysisDep"), io=devnull)
                             end,
                             additional_setup=()->nothing,
                             jetconfigs...)
    old = Pkg.project().path
    pkgcode = Base.remove_linenums!(code)
    mktempdir() do tempdir
        try
            pkgpath = normpath(tempdir, pkgname)
            Pkg.generate(pkgpath; io=devnull)
            Pkg.activate(pkgpath; io=devnull)

            base_setup(); additional_setup();

            Pkg.activate(; temp=true, io=devnull)
            Pkg.develop(; path=pkgpath, io=devnull)

            pkgcode = Expr(:module, true, Symbol(pkgname), pkgcode)
            pkgfile = normpath(pkgpath, "src", "$pkgname.jl")
            write(pkgfile, string(pkgcode))

            res = report_package(pkgname; toplevel_logger=nothing, jetconfigs...)

            @eval @testset $pkgname $test_func($res)

            return res
        finally
            Pkg.activate(old; io=devnull)
        end
    end
end

@testset "package dependency" begin
    test_report_package("UsingCore" => quote
            using Core: Box
            makebox() = Core.Box()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportBase" => quote
            import Base: show
            struct XXX end
            show(io::IO, ::XXX) = xxx
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :xxx
    end

    test_report_package("UsingSimple" => quote
            using PkgAnalysisDep
            callfunc1() = func1()
            callfunc3() = func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func3
    end
    test_report_package("UsingSpecific" => quote
            using PkgAnalysisDep: func1
            callfunc1() = func1()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("UsingAlias" => quote
            using PkgAnalysisDep: func1 as func
            callfunc1() = func()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("UsingInner" => quote
            using PkgAnalysisDep.Inner
            callfunc1() = func1()
            callfunc3() = func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func1
    end
    test_report_package("UsingBlock" => quote
            begin
                using PkgAnalysisDep
                callfunc1() = func1()
                callfunc3() = func3()
            end
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func3
    end
    test_report_package("UsingBlock" => quote
            global truecond::Bool = true
            if truecond
                using PkgAnalysisDep
                callfunc1() = func1()
                callfunc3() = func3()
            end
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func3
    end

    test_report_package("ImportSimple" => quote
            import PkgAnalysisDep
            callfunc1() = PkgAnalysisDep.func1()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportAlias" => quote
            import PkgAnalysisDep as PAD
            callfunc1() = PAD.func1()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportInnerAlias" => quote
            import PkgAnalysisDep.Inner as PADI
            callfunc3() = PADI.func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportSpecific" => quote
            import PkgAnalysisDep: func1
            callfunc1() = func1()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportAlias" => quote
            import PkgAnalysisDep: func1 as func
            callfunc1() = func()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportInner" => quote
            import PkgAnalysisDep.Inner
            callfunc1() = Inner.func1()
            callfunc3() = Inner.func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func1
    end
    test_report_package("ImportBlock" => quote
            begin
                import PkgAnalysisDep
                callfunc1() = PkgAnalysisDep.func1()
            end
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("ImportBlock" => quote
            global truecond::Bool = true
            if truecond
                import PkgAnalysisDep
                callfunc1() = PkgAnalysisDep.func1()
            end
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    test_report_package("RelativeDependency" => quote
            import PkgAnalysisDep
            using .PkgAnalysisDep: func2
            callfunc1() = func1()
            callfunc2() = func2()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :func1
    end
    test_report_package("RelativeInner" => quote
            module Inner
            struct XXX end
            export XXX
            end
            using .Inner
            Base.show(io::IO, ::XXX) = xxx
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        r = only(res.res.inference_error_reports)
        @test isa(r, UndefVarErrorReport) && r.var.name === :xxx
    end
    test_report_package("BadRelativeInner" => quote
            module Inner end
            using Inner # should be `using .Inner`
        end) do res
        r = only(res.res.toplevel_error_reports)
        @test isa(r, DependencyError) && r.pkg == "BadRelativeInner" && r.dep == "Inner"
    end

    test_report_package("UninstalledDependency" => quote
            using UninstalledDep
        end) do res
        r = only(res.res.toplevel_error_reports)
        @test isa(r, DependencyError) && r.pkg == "UninstalledDependency" && r.dep == "UninstalledDep"
    end

    test_report_package("LoadPreferences" => quote
            using Preferences

            @load_preference("LoadRootConfig", false)
            const LoadRootConfig = @load_preference("LoadRootConfig", false)

            module Submodule
            using Preferences
            @load_preference("LoadSubConfig", false)
            const LoadSubConfig = @load_preference("LoadSubConfig", false)
            end
        end; additional_setup = function ()
            Pkg.add("Preferences"; io=devnull)
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    test_report_package("SelfImport1" => quote
            function overload end
            module SubModule
            using SelfImport1
            import SelfImport1: overload
            overload(::Integer) = :integer
            end # module SubModule
            call_overload(x::Number) = overload(x)
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    test_report_package("SelfImport2" => quote
            function overload end
            module SubModule
            using SelfImport2
            import SelfImport2: overload
            overload(::Integer) = :integer
            module SubSubModule
            using SelfImport2
            import SelfImport2: overload
            overload(::Int) = :int
            end # module SubSubModule
            end # module SubModule
            call_overload(x::Number) = overload(x)
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    test_report_package("SelfImport5" => quote
            function overload end
            module SubModule
            module SubSubModule
            module SubSubSubModule
            module SubSubSubSubModule
            module SubSubSubSubSubModule
            using SelfImport5
            import SelfImport5: overload
            overload(::Int) = :int
            end # module SubSubSubSubSubModule
            end # module SubSubSubSubModule
            end # module SubSubSubModule
            end # module SubSubModule
            end # module SubModule
            call_overload(x::Number) = overload(x)
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    # ignore_missing_comparison should be turned on by default for `report_package`
    test_report_package("Issue542_1" => quote
            struct Issue542Typ end
            isa542(x) = x == Issue542Typ() ? true : false
        end;
        base_setup=()->nothing) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("Issue542_2" => quote
            struct Issue542Typ end
            isa542(x) = x == Issue542Typ() ? true : false
        end;
        base_setup=()->nothing,
        ignore_missing_comparison=false) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isa(only(res.res.inference_error_reports), NonBooleanCondErrorReport)
    end

    # special cases for `reduce_empty` and `mapreduce_empty`
    test_report_package("ReduceEmpty" => quote
            reducer(a::Vector{String}) = maximum(length, a)
        end;
        base_setup=()->nothing) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    test_report_package("Issue554_1" => quote
            using PkgAnalysisDep: Inner.func3
            callfunc3() = func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("Issue554_2" => quote
            using PkgAnalysisDep: Inner.func3 as func
            callfunc() = func()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("Issue554_3" => quote
            import PkgAnalysisDep: Inner.func3
            callfunc3() = func3()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("Issue554_4" => quote
            import PkgAnalysisDep: Inner.func3 as func
            callfunc() = func()
        end) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    test_report_package("Issue554" => quote
            using LinearAlgebra: BLAS.BlasFloat
            issue554(x::BlasFloat) = x
        end,
        base_setup=()->Pkg.add("LinearAlgebra", io=devnull)) do res
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
end

# aviatesk/JET.jl#597: don't try to concrete-interpret `:jl_extern_c`
let old = Pkg.project().path
    try
        Pkg.activate(; temp=true, io=devnull)
        Pkg.develop(; path=normpath(FIXTURES_DIR, "JET597"), io=devnull)

        using JET597

        res = report_package(JET597; toplevel_logger=nothing)
        @test isempty(res.res.toplevel_error_reports)
    finally
        Pkg.activate(old; io=devnull)
    end
end

end # module test_virtualprocess
