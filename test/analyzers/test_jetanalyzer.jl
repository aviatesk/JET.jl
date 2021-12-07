@testset "cache per configuration" begin
    # cache key should be same for the same configurations
    let
        k1 = JET.get_cache_key(JETAnalyzer())

        k2 = JET.get_cache_key(JETAnalyzer())

        @test k1 == k2
    end

    # cache key should be different for different configurations
    let
        analyzer1 = JETAnalyzer(; max_methods=3)
        k1 = JET.get_cache_key(analyzer1)

        analyzer2 = JETAnalyzer(; max_methods=4)
        k2 = JET.get_cache_key(analyzer2)

        @test k1 ≠ k2
    end

    # configurations other than `InferenceParams` and `ReportPass`
    # shouldn't affect the cache key identity
    let
        analyzer1 = JETAnalyzer(; toplevel_logger=nothing)
        k1 = JET.get_cache_key(analyzer1)

        analyzer2 = JETAnalyzer(; toplevel_logger=IOBuffer())
        k2 = JET.get_cache_key(analyzer2)

        @test k1 == k2
    end

    # cache key should be different for different report passes
    let
        analyzer1 = JETAnalyzer(; report_pass=JET.BasicPass())
        k1 = JET.get_cache_key(analyzer1)

        analyzer2 = JETAnalyzer(; report_pass=JET.SoundPass())
        k2 = JET.get_cache_key(analyzer2)

        @test k1 ≠ k2
    end

    # end to end test
    let
        m = Module()
        @eval m begin
            foo(a::Val{1}) = 1
            foo(a::Val{2}) = 2
            foo(a::Val{3}) = 3
            foo(a::Val{4}) = undefvar
        end

        # run first analysis and cache
        result = @eval m $report_call((Int,); max_methods=3) do a
            foo(Val(a))
        end
        @test isempty(get_reports(result))

        # should use the cached result
        result = @eval m $report_call((Int,); max_methods=3) do a
            foo(Val(a))
        end
        @test isempty(get_reports(result))

        # should re-run analysis, and should get a report
        result = @eval m $report_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(get_reports(result)) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end

        # should run the cached previous result
        result = @eval m $report_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(get_reports(result)) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end
    end
end

@testset "report no method matching" begin
    # if there is no method matching case, it should be reported
    let
        # NOTE: we can't just wrap them into `let`, closures can't be inferred correctly
        m = Module()
        result = Core.eval(m, quote
            foo(a::Integer) = :Integer
            $report_call((AbstractString,)) do a
                foo(a)
            end
        end)
        @test length(get_reports(result)) === 1
        report = first(get_reports(result))
        @test report isa NoMethodErrorReport
        @test report.t === Tuple{typeof(m.foo), AbstractString}
    end

    let
        result = report_call(()->sum([]))
        @test length(get_reports(result)) === 1
        report = first(get_reports(result))
        @test report isa SeriousExceptionReport
        @test report.err isa MethodError
        @test report.err.f === zero
    end

    # if there is no method matching case in union-split, it should be reported
    let
        m = Module()
        result = Core.eval(m, quote
            foo(a::Integer) = :Integer
            foo(a::AbstractString) = "AbstractString"

            $report_call(a->foo(a), (Union{Nothing,Int},))
        end)

        @test length(get_reports(result)) === 1
        report = first(get_reports(result))
        @test report isa NoMethodErrorReport
        @test report.t == Any[Tuple{typeof(m.foo), Nothing}]
    end
end

@testset "report local undefined variables" begin
    let
        result = report_call((Bool,)) do b
            if b
                bar = rand(Int)
                return bar
            end
            return bar # undefined in this pass
        end
        @test length(get_reports(result)) === 1
        r = first(get_reports(result))
        @test r isa LocalUndefVarErrorReport
        @test r.name === :bar
        @test last(r.vst).line == (@__LINE__)-6
    end

    # deeper level
    let
        m = @fixturedef begin
            function foo(b)
                if b
                    bar = rand(Int)
                    return bar
                end
                return bar # undefined in this pass
            end
            baz(a) = foo(a)
        end

        result = Core.eval(m, :($report_call(baz, (Bool,))))
        @test length(get_reports(result)) === 1
        r = first(get_reports(result))
        @test r isa LocalUndefVarErrorReport
        @test r.name === :bar
        @test last(r.vst).line == (@__LINE__)-10

        # works when cached
        result = Core.eval(m, :($report_call(baz, (Bool,))))
        @test length(get_reports(result)) === 1
        r = first(get_reports(result))
        @test r isa LocalUndefVarErrorReport
        @test r.name === :bar
        @test last(r.vst).line == (@__LINE__)-18
    end

    # try to exclude false negatives as possible (by collecting reports in after-optimization pass)
    let
        result = report_call((Bool,)) do b
            if b
                bar = rand()
            end

            return if b
                return bar # this shouldn't be reported
            else
                return nothing
            end
        end
        @test isempty(get_reports(result))
    end

    let
        result = report_call((Bool,)) do b
            if b
                bar = rand()
            end

            return if b
                return nothing
            else
                # ideally we want to have report for this pass, but tons of work will be
                # needed to report this pass
                return bar
            end
        end
        @test_broken length(get_reports(result)) === 1 &&
            first(get_reports(result)) isa LocalUndefVarErrorReport &&
            first(get_reports(result)).name === :bar
    end

    let
        result = report_call((Int,)) do a
            function inner(n)
                if n > 0
                   a = n
                end
            end
            inner(rand(Int))
            return a
        end
        @test isempty(get_reports(result))
    end

    let # should work for top-level analysis
        res = @analyze_toplevel begin
            foo = let
                if rand(Bool)
                    bar = rand(Int)
                else
                    bar # undefined in this pass
                end
            end
        end
        @test length(res.inference_error_reports) === 1 &&
              first(res.inference_error_reports) isa LocalUndefVarErrorReport &&
              first(res.inference_error_reports).name === :bar
    end
end

@testset "report undefined (global) variables" begin
    let
        result = report_call(()->foo)
        @test length(get_reports(result)) === 1
        @test first(get_reports(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports(result)).name === :foo
    end

    # deeper level
    let
        m = @fixturedef begin
            foo(bar) = bar + baz
            qux(a) = foo(a)
        end

        result = Core.eval(m, :($report_call(qux, (Int,))))
        @test length(get_reports(result)) === 1
        @test first(get_reports(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports(result)).name === :baz

        # works when cached
        result = Core.eval(m, :($report_call(qux, (Int,))))
        @test length(get_reports(result)) === 1
        @test first(get_reports(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports(result)).name === :baz
    end
end

@testset "report non-boolean condition error" begin
    # simple case
    let
        result = report_call((Int,)) do a
            a ? a : nothing
        end
        @test length(get_reports(result)) === 1
        er = first(get_reports(result))
        @test er isa NonBooleanCondErrorReport
        @test er.t === Int
    end

    # don't report when a type can be `Bool` (Bool ⊑ type)
    let
        result = report_call((Integer,)) do a
            a ? a : nothing
        end
        @test isempty(get_reports(result))
    end

    # report union split case
    let
        result = report_call((Union{Nothing,Bool},)) do a
            a ? a : false
        end
        @test length(get_reports(result)) === 1
        let r = first(get_reports(result))
            @test r isa NonBooleanCondErrorReport
            @test r.t == [Nothing]
            @test occursin("for 1 of 2 union split cases", r.msg)
        end
    end

    let
        result = report_call() do
            anyary = Any[1,2,3]
            first(anyary) ? first(anyary) : nothing
        end
        @test isempty(get_reports(result)) # very untyped, we can't report on this ...
    end
end

@testset "UndefKeywordError" begin
    isa2(t) = x -> isa(x, t)
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            foo(a; #= can be undef =# kw) =  a, kw
            $report_call(foo, (Any,))
        end)
        @test !isempty(get_reports(result))
        @test any(get_reports(result)) do r
            r isa SeriousExceptionReport || return false
            err = r.err
            err isa UndefKeywordError && err.var === :kw
        end
        # there shouldn't be duplicated report for the `throw` call
        @test !any(isa2(UncaughtExceptionReport), get_reports(result))
    end
end

@testset "DivideError" begin
    isa2(t) = x -> isa(x, t)
    let
        apply(f, args...) = f(args...)

        result = report_call() do
            apply(div, 1, 0)
        end
        @test !isempty(get_reports(result))
        @test any(isa2(DivideErrorReport), get_reports(result))

        result = report_call() do
            apply(rem, 1, 0)
        end
        @test !isempty(get_reports(result))
        @test any(isa2(DivideErrorReport), get_reports(result))

        # JET analysis isn't sound
        result = report_call((Int,Int)) do a, b
            apply(div, a, b)
        end
        @test isempty(get_reports(result))
    end
end

@testset "report `throw` calls" begin
    # simplest case
    let
        result = report_call(()->throw("foo"))
        @test !isempty(get_reports(result))
        @test first(get_reports(result)) isa UncaughtExceptionReport
    end

    # throws in deep level
    let
        foo(a) = throw(a)
        result = report_call(()->foo("foo"))
        @test !isempty(get_reports(result))
        @test first(get_reports(result)) isa UncaughtExceptionReport
    end

    # don't report possibly false negative `throw`s
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(foo, (Int,))
        @test isempty(get_reports(result))
    end

    # constant prop sometimes helps exclude false negatives
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(()->foo(0))
        @test !isempty(get_reports(result))
        @test first(get_reports(result)) isa UncaughtExceptionReport
    end

    # report even if there're other "critical" error exist
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            foo(a) = sum(a) # should be reported
            bar(a) = throw(a) # shouldn't be reported first
            $report_call((Bool, String)) do b, s
                b && foo(s)
                bar(s)
            end
        end)
        @test length(get_reports(result)) === 3
        test_sum_over_string(get_reports(result))
    end

    # end to end
    let
        # this should report `throw(ArgumentError("Sampler for this object is not defined")`
        result = report_call(rand, (Char,))
        @test !isempty(get_reports(result))
        @test first(get_reports(result)) isa UncaughtExceptionReport

        # this should not report `throw(DomainError(x, "sin(x) is only defined for finite x."))`
        result = report_call(sin, (Int,))
        @test isempty(get_reports(result))

        # again, constant prop sometimes can exclude false negatives
        result = report_call(()->sin(Inf))
        @test !isempty(get_reports(result))
        @test first(get_reports(result)) isa UncaughtExceptionReport
    end
end

@testset "keyword argument methods" begin
    result = Core.eval(gen_virtual_module(), quote
        f(a; b = nothing, c = nothing) = return
        $report_call((Any,)) do b
            f(1; b)
        end
    end)
    @test isempty(get_reports(result))
end

@testset "don't early escape if type grows up to `Any`" begin
    vmod = gen_virtual_module()
    res = @analyze_toplevel context = vmod virtualize = false begin
        abstract type Foo end
        struct Foo1 <: Foo
            bar
        end
        struct Foo2 <: Foo
            baz # typo
        end
        struct Foo3 <: Foo
            qux # typo
        end
        bar(foo::Foo) = foo.bar

        f = rand(Bool) ? Foo1(1) : rand(Bool) ? Foo2(1) : Foo3(1)
        bar(f)
    end

    # `bar(::Foo1)` is valid but its return type is `Any`, and so we can't collect possible
    # error points for `bar(::Foo2)` and `bar(::Foo3)` if we bail out on `Any`-grew return type
    @test length(res.inference_error_reports) === 2
    @test any(res.inference_error_reports) do er
        return er isa NoFieldErrorReport &&
            er.typ === vmod.Foo2 &&
            er.name === :bar
    end
    @test any(res.inference_error_reports) do er
        return er isa NoFieldErrorReport &&
            er.typ === vmod.Foo3 &&
            er.name === :bar
    end
end

@testset "abstract_invoke" begin
    # non-`Type` `argtypes`
    result = report_call() do
        invoke(sin, :this_should_be_type, 1.0)
    end
    @test length(get_reports(result)) == 1
    r = first(get_reports(result))
    @test isa(r, InvalidInvokeErrorReport)

    # invalid `argtype`
    result = report_call() do
        Base.@invoke sin(1.0::Int)
    end
    @test length(get_reports(result)) == 1
    r = first(get_reports(result))
    @test isa(r, InvalidInvokeErrorReport)

    # don't report errors collected in `invoke`d functions
    result = @eval Module() begin
        foo(i::Integer) = throw(string(i))
        $report_call((Int,)) do a
            Base.@invoke foo(a::Integer)
        end
    end
    @test !isempty(get_reports(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports(result))

    #== LINE SENSITIVITY START ===#
    BAR_LINE = (@__LINE__) + 3
    BAZ_LINE = (@__LINE__) + 5
    result = @eval begin
        bar(a) = return a + undefvar
        function baz(a)
            a += 1
            Base.@invoke bar(a::Int) # `invoke` is valid, but error should happne within `bar`
        end
        $report_call(baz, (Any,))
    end
    #== LINE SENSITIVITY END ===#
    @test !isempty(get_reports(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports(result))
    # virtual stack trace should include frames of both `bar` and `baz`
    # we already `@assert` it within `typeinf` when `JET_DEV_MODE` is enabled,
    # but test it explicitly here just to make sure it's working
    @test all(get_reports(result)) do r
        any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == BAR_LINE && # `bar`
            vf.linfo.def.name === :bar
        end || return false
        any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == BAZ_LINE && # `baz`
            vf.linfo.def.name === :baz
        end || return false
        return true
    end
end

@testset "staged programming" begin
    m = @fixturedef begin
        @generated function foo(a)
            if a <: Integer
                return :(a)
            elseif a <: Number
                return :(undefvar) # report me this case
            end
            throw("invalid argument")
        end

        bar(args...) = foo(args...)
    end

    let # successful code generation, valid code
        result = report_call(m.foo, (Int,))
        @test isempty(get_reports(result))
    end

    let # successful code generation, invalid code
        result = report_call(m.foo, (Float64,))
        @test length(get_reports(result)) == 1
        r = first(get_reports(result))
        @test isa(r, GlobalUndefVarErrorReport)
        @test r.name === :undefvar
    end

    let # unsuccessful code generation
        result = report_call(m.foo, (String,))
        @test length(get_reports(result)) == 1
        r = first(get_reports(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end

    let # should work if cached
        result = report_call(m.bar, (String,))
        @test length(get_reports(result)) == 1
        r = first(get_reports(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end
end

@testset "report invalid builtin call" begin
    result = report_call((Int, Type{Int}, Any)) do a, b, c
        isa(a, b, c)
    end
    @test length(get_reports(result)) === 1
    report = first(get_reports(result))
    @test report isa InvalidBuiltinCallErrorReport &&
        widenconst.(report.argtypes) == [Int, Type{Int}, Any]

    @testset "constant propagation" begin
        m = gen_virtual_module()
        Core.eval(m, quote
            struct T
                v
            end
            access_field(t, sym) = getfield(t, sym)
        end)

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(get_reports(result))

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:w), (T,))
        end)
        @test length(get_reports(result)) === 1
        er = first(get_reports(result))
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(get_reports(result))
    end
end

@testset "malformed getfield" begin
    let
        # shouldn't error
        result = report_call((Any,)) do a
            getfield(a)
        end
        @test length(get_reports(result)) == 1
        @test first(get_reports(result)) isa InvalidBuiltinCallErrorReport
    end
end

@testset "special case `return_type`" begin
    # don't report invalid method calls simulated in `return_type_tfunc`
    let
        result = report_call(()->CC.return_type(sum, Tuple{String}))
        @test isempty(get_reports(result))
    end

    # report invalid call of `return_type` itself
    let
        result = report_call(()->CC.return_type(sum))
        @test length(get_reports(result)) == 1
        @test isa(first(get_reports(result)), InvalidReturnTypeCall)
    end

    # end to end
    let
        # this shouldn't report "no matching method found for call signature: Base.iterate(itr::DataType)",
        # which otherwise will be caught in `abstract_cal` in `return_type_tfunc`
        result = report_call(() -> Dict('a' => 1, :b => 2))
        @test isempty(get_reports(result))
    end
end

@testset "target_modules" begin
    # from `PrintConfig` docstring

    M = Module()
    @eval M begin
        function foo(a)
            r1 = sum(a)       # => Base: MethodError(+(::Char, ::Char)), MethodError(zero(::Type{Any}))
            r2 = undefsum(a)  # => @__MODULE__: UndefVarError(:undefsum)
            return r1, r2
        end
    end

    let
        result = @report_call M.foo("julia")
        @test length(get_reports(result)) == 3
        @test any(get_reports(result)) do report
            isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
        end
    end

    let
        result = @report_call target_modules=(M,) M.foo("julia")
        report = only(get_reports(result))
        @test isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
    end
end

@testset "BasicPass" begin
    @testset "basicfilter" begin
        # skip errors on abstract dispatch
        let # https://github.com/aviatesk/JET.jl/issues/154
            res = @analyze_toplevel analyze_from_definitions=true begin
                struct Foo
                    x::AbstractVector{<:AbstractString}
                end
            end
            @test isempty(res.inference_error_reports)
        end

        # should still report anything within entry frame
        let
            res = @eval Module() begin
                foo(a::Int) = "hello"
                $report_call((AbstractString,)) do a # this abstract call isn't concrete dispatch
                    foo(a)
                end
            end
            @test !isempty(get_reports(res))
            @test any(r->isa(r,NoMethodErrorReport), get_reports(res))
        end

        # skip errors on abstract entry frame entered by `analyze_from_definitions!`
        let
            res = @analyze_toplevel analyze_from_definitions=true begin
                struct Foo end
                function isfoo(x)
                    # ==(::Missing, ::Foo) -> Missing will lead to `NonBooleanCondErrorReport` otherwise
                    return x == Foo() ? :foo : :bar
                end
            end
            @test !any(res.inference_error_reports) do r
                isa(r, NonBooleanCondErrorReport)
            end
        end
    end
end

@testset "SoundPass" begin
    let # `:basicfilter`
        m = Module()
        # `==(::Missing, ::Any) -> Missing` will be reported
        @eval m foo(a, b) = a == b ? 0 : 1
        result = @eval m $report_call((Any,Symbol)) do a, b
            foo(a, b)
        end
        @test isempty(get_reports(result))

        result = report_call((Any,Symbol); mode = :sound) do a, b
            a == b ? 0 : 1
        end
        @test any(get_reports(result)) do r
            isa(r, NonBooleanCondErrorReport) &&
            r.t == Type[Missing]
        end
    end

    let # `!(t ⊑ Bool)` -> error
        basic = report_call((Integer,)) do cond
            cond ? 0 : 1
        end
        @test isempty(get_reports(basic))

        sound = report_call((Integer,); mode=:sound) do cond
            cond ? 0 : 1
        end
        @test any(get_reports(sound)) do r
            isa(r, NonBooleanCondErrorReport) &&
            r.t == Integer
        end
    end
end

@testset "TypoPass" begin
    @test_call mode=:typo sum("julia") # don't report NoMethodError, etc.

    @testset "global undef var" begin
        let
            result = report_call(()->foo; mode=:typo)
            @test length(get_reports(result)) === 1
            @test first(get_reports(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports(result)).name === :foo
        end

        let # deeper level
            m = @fixturedef begin
                foo(bar) = bar + baz
                qux(a) = foo(a)
            end

            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            @test length(get_reports(result)) === 1
            @test first(get_reports(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports(result)).name === :baz

            # works when cached
            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            @test length(get_reports(result)) === 1
            @test first(get_reports(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports(result)).name === :baz
        end
    end

    @testset "no field" begin
        m = @fixturedef begin
            struct T
                v
            end
            access_field(t, sym) = getfield(t, sym)
        end

        result = Core.eval(m, :($report_call(t->access_field(t,:w), (T,); mode=:typo)))
        @test length(get_reports(result)) === 1
        er = first(get_reports(result))
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w
    end
end
