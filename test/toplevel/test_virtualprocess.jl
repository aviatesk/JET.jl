@testset "syntax error reports" begin
    let
        s = """
        begin
            c = rand(Bool)
        end

        end
        """

        res = report_text(s).res

        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa SyntaxErrorReport
    end
end

@testset "virtualize module context" begin
    # the context of the original module should be virtualized
    let
        actual = gen_virtual_module(@__MODULE__)
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
    let
        actual = gen_virtual_module(@__MODULE__)
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
    let
        orig = gen_virtual_module(@__MODULE__)
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
        @test isempty(res.toplevel_error_reports)
    end

    # don't error if there is undefined export
    let
        actual = gen_virtual_module(@__MODULE__)
        Core.eval(actual, :(export undefined))

        @test (virtualize_module_context(actual); true)

        res = @analyze_toplevel context = actual begin
            println(undefined)
        end
        @test isempty(res.toplevel_error_reports)
        @test any(res.inference_error_reports) do err
            isa(err, GlobalUndefVarErrorReport) &&
            err.name === :undefined
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

    # https://github.com/aviatesk/JET.jl/issues/151
    let
        res = @analyze_toplevel begin
            struct X end

            module A
            using ..Main: X
            end
        end
        @test isempty(res.toplevel_error_reports)
    end

    let
        res = @analyze_toplevel begin
            module A
            struct X end
            export X
            end

            module B
            import ..Main.A
            println(A.X)
            end

            module C
            import ..Main.A: X
            println(X)
            end

            module D
            using ..Main.A
            println(X)
            end

            module E
            using ..Main.A: X
            println(X)
            end
        end
        @test isempty(res.toplevel_error_reports)
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
        vmod, = @analyze_toplevel2 begin
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
            @test is_concrete(vmod, :Foo)
            @test is_analyzed(vmod, :foo)
            @test isa_analyzed(vmod.foo, vmod.Foo)
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
            @test isempty(res.toplevel_error_reports)
            @test is_analyzed(vmod, :foo)
            @test isa_analyzed(vmod.foo, vmod.Foo1)
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

            @test isempty(res.toplevel_error_reports)
            @test is_analyzed(vmod, :Foo)
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

            @test isempty(res.toplevel_error_reports)
            @test is_analyzed(vmod, :Foo)
            @test is_analyzed(vmod, :bar)
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
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
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
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
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
        vmod, res = @analyze_toplevel2 begin
            macro wrap_in_mod(blk)
                ex = Expr(:module, true, esc(:foo), esc(blk))
                return Expr(:toplevel, ex)
            end

            @wrap_in_mod begin
                bar() = nothing
            end
        end
        @test isdefined(vmod, :foo) && isdefined(vmod.foo, :bar)

        vmod, res = @analyze_toplevel2 begin
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
        f2 = normpath(FIXTURE_DIR, "include2.jl")

        context = gen_virtual_module(@__MODULE__)
        res = report_file2(f1; context, virtualize = false).res

        @test f1 in res.included_files
        @test f2 in res.included_files
        @test is_concrete(context, :foo)
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
    end

    let
        f = normpath(FIXTURE_DIR, "nonexistinclude.jl")
        res = report_file2(f).res

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa ActualErrorWrapped
        @test !isempty(res.inference_error_reports)
        @test first(res.inference_error_reports) isa GlobalUndefVarErrorReport
    end

    let
        f = normpath(FIXTURE_DIR, "selfrecursiveinclude.jl")
        res = report_file2(f).res

        @test f in res.included_files
        @test !isempty(res.toplevel_error_reports)
        @test first(res.toplevel_error_reports) isa RecursiveIncludeErrorReport
    end

    let
        f1 = normpath(FIXTURE_DIR, "chainrecursiveinclude1.jl")
        f2 = normpath(FIXTURE_DIR, "chainrecursiveinclude2.jl")
        res = report_file2(f1).res

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

    let
        modf = normpath(FIXTURE_DIR, "modinclude.jl")
        inc2 = normpath(FIXTURE_DIR, "include2.jl")

        context = gen_virtual_module(@__MODULE__)
        res = report_file2(modf; context, virtualize=false).res

        @test modf in res.included_files
        @test inc2 in res.included_files
        @test isempty(res.toplevel_error_reports)
        @test isempty(res.inference_error_reports)
        @test is_concrete(context.Outer, :foo)
    end

    # bad includes
    # ------------

    let res = @analyze_toplevel begin
            include(Symbol("somefile.jl"))
        end
        @test length(res.toplevel_error_reports) == 1
        report = res.toplevel_error_reports[1]
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Base.include("somefile.jl", __xxx__)
        end
        @test length(res.toplevel_error_reports) == 1
        report = res.toplevel_error_reports[1]
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
        @test err.f === Base.include
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Core.include("somefile.jl", __xxx__)
        end
        @test length(res.toplevel_error_reports) == 1
        report = res.toplevel_error_reports[1]
        @test report isa ActualErrorWrapped
        err = report.err
        @test err isa MethodError
        @test err.f === Core.include
    end
    let res = @analyze_toplevel begin
            module __xxx__ end
            Base.include(__xxx__, Symbol("somefile.jl"))
        end
        @test length(res.toplevel_error_reports) == 1
        report = res.toplevel_error_reports[1]
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
    res = report_text(s).res
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
        vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
                begin
                    globalvar = rand(Bool)
                end
            end

            @test is_abstract(vmod, :globalvar)
            @test isa_abstract(vmod.globalvar, Bool)
        end

        let
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
                let
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
                for i = 1:100
                    localvar = i
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
                while true
                    localvar = rand(Bool)
                end
            end

            @test !isdefined(vmod, :localvar)
        end

        let
            vmod, res = @analyze_toplevel2 begin
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
            vmod, res = @analyze_toplevel2 begin
                r1, r2 = rand(2)
            end

            @test is_abstract(vmod, :r1)
            @test isa_abstract(vmod.r1, Float64)
            @test is_abstract(vmod, :r2)
            @test isa_abstract(vmod.r2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local r1, r2
                    r1, r2 = rand(2)
                end
            end

            @test !isdefined(vmod, :r1)
            @test !isdefined(vmod, :r2)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                let
                    global r1, r2
                    r1, r2 = rand(2)
                end
            end

            @test is_abstract(vmod, :r1)
            @test isa_abstract(vmod.r1, Float64)
            @test is_abstract(vmod, :r2)
            @test isa_abstract(vmod.r2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                ro1, ro2 = let
                    ri1, ri2 = rand(2)
                end
            end

            @test !isdefined(vmod, :ri1)
            @test !isdefined(vmod, :ri2)
            @test is_abstract(vmod, :ro1)
            @test isa_abstract(vmod.ro1, Float64)
            @test is_abstract(vmod, :ro2)
            @test isa_abstract(vmod.ro2, Float64)
        end

        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    local l
                    l, g = rand(2)
                end
            end
            @test !isdefined(vmod, :l)
            @test is_abstract(vmod, :g)
            @test isa_abstract(vmod.g, Float64)
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
            @test is_concrete(m, :a)
            @test m.a == 0
        end
        let
            m, = @analyze_toplevel2 begin
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

        @test isempty(res.toplevel_error_reports)
    end
end

let # global declaration
    res = @analyze_toplevel begin
        global var
    end
    @test isempty(res.toplevel_error_reports)
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
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
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
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
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
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
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
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
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
        res = @test_logs (:warn,) @analyze_toplevel context = vmod virtualize = false begin
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

@testset "non-deterministic abstract global assignments" begin
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = 0 # deterministic
        end
        @test is_analyzed(vmod, :v)
        @test isa_analyzed(vmod.v, Int)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = rand(Int) # deterministic
        end
        @test is_analyzed(vmod, :v)
        @test isa_analyzed(vmod.v, Int)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = 0) # non-deterministic
        end
        @test is_analyzed(vmod, :v)
        @test isa_analyzed(vmod.v, Union{Char,Int})
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = rand(Int)) # non-deterministic
        end
        @test is_analyzed(vmod, :v)
        @test isa_analyzed(vmod.v, Union{Char,Int})
    end

    # end to end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            v = rand(Int) # deterministic

            f(::Number) = :ok
            f(v) # no method error should NOT happen here
        end

        @test isempty(res.inference_error_reports)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            v = '0'
            rand(Bool) && (v = rand(Int)) # non-deterministic

            f(::Number) = :ok
            f(v) # union-split no method error should happen here
        end

        @test length(res.inference_error_reports) === 1
        er = first(res.inference_error_reports)
        @test er isa NoMethodErrorReport
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

@testset "avoid too much bail out from `_virtual_process!`" begin
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
let
    fixtures_dir = normpath(@__DIR__, "..", "fixtures")
    global const CONCRETIZATION_PATTERNS_FILE   = normpath(fixtures_dir, "concretization_patterns.jl")
    global const CONCRETIZATION_PATTERNS_CONFIG = normpath(fixtures_dir, "..JET.toml")
end

@testset "custom concretization pattern" begin
    # custom concretization pattern should work on AST level
    let
        vmod, res = @analyze_toplevel2 begin
            const foo = Dict() # won't be concretized by default
        end
        @test !is_concrete(vmod, :foo)
    end
    let
        vmod, res = @analyze_toplevel2 begin
            const foo = Dict() # now this will be forcibly concretized
        end concretization_patterns = [:(const foo = Dict())]
        @test is_concrete(vmod, :foo)
    end

    # the analysis on `test/fixtures/concretization_patterns.jl` will produce inappropriate
    # top-level error report because of missing concretization
    let
        res = report_file2(CONCRETIZATION_PATTERNS_FILE).res
        @test length(res.toplevel_error_reports) == 1
        let r = first(res.toplevel_error_reports)
            @test isa(r, MissingConcretization)
        end
    end

    # we can circumvent the issue by using the `concretization_patterns` configuration !
    let
        res = report_file2(CONCRETIZATION_PATTERNS_FILE;
                           concretization_patterns = [:(const GLOBAL_CODE_STORE = Dict())],
                           ).res
        @test isempty(res.toplevel_error_reports)
    end

    # we can specify whatever pattern `@capture` can accept
    let
        vmod, res = @analyze_toplevel2 begin
            foo() = rand((1,2,3))
            a = foo()
            b = foo()
        end concretization_patterns = [:x_] # means "concretize everything"
        @test is_concrete(vmod, :a)
        @test is_concrete(vmod, :b)
    end

    # `concretization_patterns` should "intuitively" work for code with documentations attached
    let
        vmod, res = @analyze_toplevel2 begin
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
    dir = mktempdir()
    analysis_target = normpath(dir, "concretization_patterns.jl")
    config_target   = normpath(dir, JET.CONFIG_FILE_NAME)

    back = pwd()
    try
        # in order to check the functionality, fixtures/..JET.toml logs toplevel analysis
        # into toplevel.txt (relative to the current working directory)
        # and so let's `cd` into the temporary directory to not pollute this directory
        cd(dir)

        open(CONCRETIZATION_PATTERNS_FILE) do f
            write(analysis_target, f)
        end

        # no configuration, thus top-level analysis should fail
        let
            res = report_file2(analysis_target)
            nreported = print_reports(IOBuffer(), res)
            @test !iszero(nreported) # error reported
        end

        # setup a configuration file
        open(CONCRETIZATION_PATTERNS_CONFIG) do f
            write(config_target, f)
        end

        # now any top-level analysis failure shouldn't happen
        let
            res = report_file2(analysis_target)
            nreported = print_reports(IOBuffer(), res)
            @test iszero(nreported) # no error happened
            @test isfile("toplevel.txt") # not closed yet
        end
    catch err
        rethrow(err)
    finally
        cd(back)
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
    @test isempty(res.toplevel_signatures)

    vmod, res = @analyze_toplevel2 analyze_from_definitions=true begin
        foo() = return
        bar(a) = return a
        baz(a) = return a
        baz(a::Int) = return a
        qux(a::T) where T<:Integer = return a
    end
    @test !isempty(res.toplevel_signatures)
    @test any(res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.foo)}
    end
    @test any(res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.bar),Any}
    end
    @test any(res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.baz),Any}
    end
    @test any(res.toplevel_signatures) do sig
        sig === Tuple{typeof(vmod.baz),Int}
    end
    @test any(res.toplevel_signatures) do sig
        sig <: Tuple{typeof(vmod.qux),Integer} # `Tuple{typeof(vmod.qux),TypeVar}`
    end
end

@testset "analyze from definitions" begin
    let
        res = @analyze_toplevel analyze_from_definitions=false begin
            foo() = return undefvar
        end
        @test isempty(res.inference_error_reports)

        res = @analyze_toplevel analyze_from_definitions=true begin
            foo() = return undefvar
        end
        @test !isempty(res.inference_error_reports)
        @test any(res.inference_error_reports) do err
            isa(err, GlobalUndefVarErrorReport) &&
            err.name === :undefvar
        end
    end

    let # COMBAK use different aggregation policy for "user-script" analysis ?
        res = @analyze_toplevel analyze_from_definitions=true begin
            foo(a) = b # typo
            bar() = foo("julia")
        end
        @test length(res.inference_error_reports) ≥ 1
        # @test_broken any(res.inference_error_reports) do err # report analyzed from `foo`
        #     isa(err, GlobalUndefVarErrorReport) &&
        #     err.name === :b &&
        #     length(err.vst) == 1
        # end &&       any(res.inference_error_reports) do err # report analyzed from `bar`
        #     isa(err, GlobalUndefVarErrorReport) &&
        #     err.name === :b &&
        #     length(err.vst) == 2
        # end
    end

    let
        res = @analyze_toplevel analyze_from_definitions=true begin
            foo(a) = sum(a)
            bar() = foo("julia")
        end
        test_sum_over_string(res)
    end
end

@testset "top-level statement selection" begin
    # simplest example
    let
        # global function
        vmod, res = @analyze_toplevel2 begin
            foo() = return # should be concretized
        end
        @test is_concrete(vmod, :foo)

        # inner function
        vmod, res = @analyze_toplevel2 analyze_from_definitions=true let
            foo(a) = a # should be concretized, and its signature should have been collected
        end
        @test length(res.toplevel_signatures) == 1
    end

    # simple negative case test, which checks we do NOT select statements not involved with any definition
    # this particular example is adapted from https://en.wikipedia.org/wiki/Program_slicing
    let
        src = JET.@src let
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

        for (i, stmt) in enumerate(src.code)
            if JET.isexpr(stmt, :(=))
                lhs, rhs = stmt.args
                if isa(lhs, Core.SlotNumber)
                    if src.slotnames[lhs.id] === :w || src.slotnames[lhs.id] === :sum
                        @test slice[i]
                    elseif src.slotnames[lhs.id] === :product
                        @test !slice[i]
                    end
                end
            elseif JET.@capture(stmt, write(x_))
                @test !slice[i]
            end
        end
    end

    @testset "captured variables" begin
        let
            vmod, res = @analyze_toplevel2 begin
                begin
                    s = join(rand(Char, 100))
                    foo() = return s
                end
            end
            @test is_concrete(vmod, :foo)
            @test is_abstract(vmod, :s)
        end

        # captured variables for global functions
        let
            # XXX `s` below aren't necessarily concretized, but concretization of `foo` requires
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
            @test_broken isempty(res.toplevel_error_reports)

            # more realistic example
            vmod, res = @analyze_toplevel2 let s = sprint(showerror, DivideError())
                global errmsg(s = s) = string("error: ", s)
            end
            @test is_concrete(vmod, :errmsg)
            @test isempty(res.toplevel_error_reports)
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
            @test isempty(res.toplevel_signatures)
        end
    end

    # NOTE here we test "try/catch"-involved control flow in a fine-grained level
    # we want to enrich this test suite as much as possible since this control flow often
    # appears in test files with the test macros, while we will do the e2e level tests
    # with those macros
    @testset "try/catch control flow" begin
        # https://github.com/aviatesk/JET.jl/issues/150
        let
            res = @analyze_toplevel analyze_from_definitions=true try
                foo(a) = sum(a) # essentially same as inner function, should be concretized
                foo("julia") # shouldn't be concretized
            catch err
                err
            end

            @test isempty(res.toplevel_error_reports)
            @test length(res.toplevel_signatures) == 1
            test_sum_over_string(res)
        end

        # captured variables within a try clause
        let
            res = @analyze_toplevel analyze_from_definitions=true try
                s = "julia"
                foo(f) = f(s) # should be concretized
                foo(sum) # shouldn't be concretized
            catch err
                err
            end

            @test isempty(res.toplevel_error_reports)
            @test length(res.toplevel_signatures) == 1
            test_sum_over_string(res)
        end

        # captured variables within a catch clause
        let
            res = @analyze_toplevel analyze_from_definitions=true try
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

            @test isempty(res.toplevel_error_reports)
            @test length(res.toplevel_signatures) == 2
            test_sum_over_string(res)
            @test any(res.inference_error_reports) do r
                isa(r, GlobalUndefVarErrorReport) &&
                r.name === :err2
            end
        end

        # XXX similar to "captured variables for global functions" cases, but such patterns
        # can be even worse when used within a catch clause; it will just fail into a hell.
        # I include this case just for the reference and won't expect this to work robustly
        # since it heavily depends on Julia's AST lowering and the implementation detail of
        # JuliaInterpreter.jl
        let
            vmod, res = @analyze_toplevel2 try
                s = "julia"
                foo(f) = f(s) # should be concretized
                foo(sum) # shouldn't be concretized
            catch err # will be `MethodError` in actual execution
                global geterr() = return err # should be concretized
            end

            # thanks to JuliaInterpreter's implementation detail, the analysis above won't
            # report top-level errors and can concretize `geterr` even if the actual `err`
            # is not thrown and thus these first two test cases will pass
            @test isempty(res.toplevel_error_reports)
            @test is_concrete(vmod, :geterr) && length(methods(vmod.geterr)) == 1
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

    # code expanded by `@enum` is fairly complex and it couldn't be handled well by JET's
    # general statement selection logic; so currently JET special case it and concretize
    # the whole macro context by pre-defined `concretization_patterns` configurations
    # (xref: originally reported from https://github.com/aviatesk/JET.jl/issues/175)
    @testset "test with @enum macro" begin
        res = @analyze_toplevel begin
            @enum Fruit apple orange
        end
        @test isempty(res.toplevel_error_reports)
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
        @test isempty(res.toplevel_error_reports)
    end

    let # @test_skip
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_skip 1 == 2
            @test_skip 1 == 2 atol=0.1
        end
        @test isempty(res.toplevel_error_reports)
    end

    let # @test_broken
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_broken 1 == 2
            @test_broken 1 == 2 atol=0.1
        end
        @test isempty(res.toplevel_error_reports)
    end

    let # @test_throws
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test_throws BoundsError [1, 2, 3][4]
            @test_throws DimensionMismatch [1, 2, 3] + [1, 2]
        end
        @test isempty(res.toplevel_error_reports)
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
        @test isempty(res.toplevel_error_reports)
    end

    let # @test_warn, @test_nowarn
        res = @analyze_toplevel context = vmod virtualize = false begin
            @test 1234 === @test_nowarn(1234)
            @test 5678 === @test_warn("WARNING: foo", begin println(stderr, "WARNING: foo"); 5678; end)
            let a
                @test_throws UndefVarError(:a) a
                @test_nowarn a = 1
                @test a === 1
            end
        end
        @test isempty(res.toplevel_error_reports)
    end

    let # @test_deprecated
        res = @analyze_toplevel context = vmod virtualize = false begin
            a() = 1
            b() = 2
            @test_deprecated a()
        end
        @test isempty(res.toplevel_error_reports)
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
        @test isempty(res.toplevel_error_reports)
    end

    let # @testset with actual type-level errors
        res = @analyze_toplevel context = vmod virtualize = false begin
            @testset "JET example" begin
                @test sum("julia") == "julia" # actual errors
            end
        end
        @test isempty(res.toplevel_error_reports)
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
        @test isempty(res.toplevel_error_reports)
    end
end

# in this e2e test suite, we run JET against a selection of Julia's base test suite
# (i.e. from https://github.com/JuliaLang/julia/tree/master/test) and check that it won't
# produce (false positive) top-level errors (e.g., by actually running test code, etc.)
# NOTE this could be very fragile ...
@testset "test file targets" begin
    TARGET_DIR = normpath(FIXTURE_DIR, "targets")

    let
        res = report_file2(normpath(TARGET_DIR, "error.jl")).res
        @test isempty(res.toplevel_error_reports)
    end

    let
        res = report_file2(normpath(TARGET_DIR, "dict.jl")).res
        @test isempty(res.toplevel_error_reports)
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

    let
        res = @eval @analyze_toplevel begin
            $genex

            foo(100) # generate valid code
        end
        @test isempty(res.inference_error_reports)
    end

    let
        res = @eval @analyze_toplevel begin
            $genex

            foo(100.0) # generate invalid code
        end
        @test length(res.inference_error_reports) == 1
        r = first(res.inference_error_reports)
        @test isa(r, GlobalUndefVarErrorReport)
    end

    let
        res = @eval @analyze_toplevel begin
            $genex

            foo("julia") # unsuccessful code generation
        end
        @test length(res.inference_error_reports) == 1
        r = first(res.inference_error_reports)
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end

    # when `analyze_from_definitions = true`, code generation will likely fail
    # (because we can't expect method signatures to be concrete for `@generated` function),
    # but we just ignore that
    let
        res = @eval @analyze_toplevel analyze_from_definitions=true begin
            $genex
        end
        @test isempty(res.inference_error_reports)
    end

    # when `analyze_from_definitions = true`, we can analyze generator itself
    let
        res = @analyze_toplevel analyze_from_definitions = true begin
            @generated function foo(a)
                if a <: Integer
                    return :(a)
                elseif t <: Number # `t` is undefined
                    return :(2a)
                end
                throw("invalid argument")
            end
        end
        @test length(res.inference_error_reports) == 1
        r = first(res.inference_error_reports)
        @test isa(r, GlobalUndefVarErrorReport) && r.name === :t
    end
end
