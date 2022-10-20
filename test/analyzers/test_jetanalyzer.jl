@testset "configurations" begin
    @test_throws ArgumentError("`mode` configuration should be either of `:basic`, `:sound` or `:typo`") report_call(
        function () end; mode=:badmode)
    @test_throws ArgumentError("either of `report_pass` and `mode` configurations can be specified") report_call(
        function () end; report_pass=JET.BasicPass(), mode=:sound)

    # cache key should be same for the same configurations
    let k1 = JET.get_cache_key(JETAnalyzer()),
        k2 = JET.get_cache_key(JETAnalyzer())
        @test k1 == k2
    end

    # cache key should be different for different configurations
    let analyzer1 = JETAnalyzer(; max_methods=3),
        analyzer2 = JETAnalyzer(; max_methods=4)
        k1 = JET.get_cache_key(analyzer1)
        k2 = JET.get_cache_key(analyzer2)
        @test k1 ≠ k2
    end

    # configurations other than `InferenceParams` and `ReportPass`
    # shouldn't affect the cache key identity
    let analyzer1 = JETAnalyzer(; toplevel_logger=nothing),
        analyzer2 = JETAnalyzer(; toplevel_logger=IOBuffer())
        k1 = JET.get_cache_key(analyzer1)
        k2 = JET.get_cache_key(analyzer2)
        @test k1 == k2
    end

    # cache key should be different for different report passes
    let analyzer1 = JETAnalyzer(; report_pass=JET.BasicPass()),
        analyzer2 = JETAnalyzer(; report_pass=JET.SoundPass())
        k1 = JET.get_cache_key(analyzer1)
        k2 = JET.get_cache_key(analyzer2)
        @test k1 ≠ k2
    end

    # end to end test
    let m = Module()
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
        @test isempty(get_reports_with_test(result))

        # should use the cached result
        result = @eval m $report_call((Int,); max_methods=3) do a
            foo(Val(a))
        end
        @test isempty(get_reports_with_test(result))

        # should re-run analysis, and should get a report
        result = @eval m $report_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(get_reports_with_test(result)) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end

        # should run the cached previous result
        result = @eval m $report_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(get_reports_with_test(result)) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end
    end
end

@testset "MethodErrorReport" begin
    # report no match case
    # --------------------

    # if there is no method matching case, it should be reported
    let m = Module()
        result = Core.eval(m, quote
            foo(a::Integer) = :Integer
            $report_call((AbstractString,)) do a
                foo(a)
            end
        end)
        report = only(get_reports_with_test(result))
        @test report isa MethodErrorReport
        @test report.t === Tuple{typeof(m.foo), AbstractString}
    end

    let result = report_call(()->sum([]))
        report = only(get_reports_with_test(result))
        @test report isa SeriousExceptionReport
        @test report.err isa MethodError
        @test report.err.f === zero
    end

    # if there is no method matching case in union-split, it should be reported
    let m = Module()
        result = Core.eval(m, quote
            foo(a::Integer) = :Integer
            foo(a::AbstractString) = "AbstractString"
            $report_call(a->foo(a), (Union{Nothing,Int},))
        end)

        report = only(get_reports_with_test(result))
        @test report isa MethodErrorReport
        @test report.t == Any[Tuple{typeof(m.foo), Nothing}]
    end

    # report uncovered match
    # ----------------------

    # only in :sound mode
    let (basic, sound) = @eval Module() begin
            onlyint(::Int) = :ok
            basic = $report_call((Any,)) do x
                onlyint(x)
            end
            sound = $report_call((Any,); mode=:sound) do x
                onlyint(x)
            end
            basic, sound
        end
        @test isempty(get_reports_with_test(basic))
        report = only(get_reports_with_test(sound))
        @test report isa MethodErrorReport
        @test report.uncovered
        @test report.sig[end] === Symbol
    end

    let (basic, sound) = @eval Module() begin
            integer_or_nothing(::Integer) = :ok1
            integer_or_nothing(::Nothing) = :ok2
            basic = $report_call((Union{Number,Nothing},)) do x
                integer_or_nothing(x)
            end
            sound = $report_call((Union{Number,Nothing},); mode=:sound) do x
                integer_or_nothing(x)
            end
            basic, sound
        end
        @test isempty(get_reports_with_test(basic))
        report = only(get_reports_with_test(sound))
        @test report isa MethodErrorReport
        @test report.uncovered
        @test report.sig[end] === Symbol
    end

    # report both no match error and uncovered match error
    let result = @eval Module() begin
            onlyint(::Int) = :ok
            $report_call((Union{Integer,Nothing},); mode=:sound) do x
                onlyint(x)
            end
        end
        @test length(get_reports_with_test(result)) == 2
        @test any(get_reports_with_test(result)) do report
            # no match for `onlyint(::Nothing)`
            report isa MethodErrorReport &&
            !report.uncovered &&
            report.sig[end] === Union{}
        end
        @test any(get_reports_with_test(result)) do report
            # uncovered match for `onlyint(::Integer)`
            report isa MethodErrorReport &&
            report.uncovered &&
            report.sig[end] === Symbol
        end
    end
end

@testset "LocalUndefVarErrorReport" begin
    let
        result = report_call((Bool,)) do b
            if b
                bar = rand(Int)
                return bar
            end
            return bar # undefined in this pass
        end
        @test length(get_reports_with_test(result)) === 1
        r = first(get_reports_with_test(result))
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
        @test length(get_reports_with_test(result)) === 1
        r = first(get_reports_with_test(result))
        @test r isa LocalUndefVarErrorReport
        @test r.name === :bar
        @test last(r.vst).line == (@__LINE__)-10

        # works when cached
        result = Core.eval(m, :($report_call(baz, (Bool,))))
        @test length(get_reports_with_test(result)) === 1
        r = first(get_reports_with_test(result))
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
        @test isempty(get_reports_with_test(result))
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
        @test_broken length(get_reports_with_test(result)) === 1 &&
            first(get_reports_with_test(result)) isa LocalUndefVarErrorReport &&
            first(get_reports_with_test(result)).name === :bar
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
        @test isempty(get_reports_with_test(result))
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
        let r = only(res.res.inference_error_reports)
            @test r isa LocalUndefVarErrorReport
            @test r.name === :bar
        end
    end
end

@testset "GlobalUndefVarErrorReport" begin
    let result = report_call(()->foo)
        @test length(get_reports_with_test(result)) === 1
        @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports_with_test(result)).name === :foo
    end

    # deeper level
    let m = @fixturedef begin
            foo(bar) = bar + baz
            qux(a) = foo(a)
        end

        result = Core.eval(m, :($report_call(qux, (Int,))))
        @test length(get_reports_with_test(result)) === 1
        @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports_with_test(result)).name === :baz

        # works when cached
        result = Core.eval(m, :($report_call(qux, (Int,))))
        @test length(get_reports_with_test(result)) === 1
        @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
        @test first(get_reports_with_test(result)).name === :baz
    end

    let result = @eval Module() begin
            $report_call() do
                getfield(@__MODULE__, :undefvar)
            end
        end
        report = only(get_reports_with_test(result))
        @test report isa GlobalUndefVarErrorReport
        @test report.name === :undefvar
    end

    @static @isdefined(getglobal) && let result = @eval Module() begin
            $report_call() do
                getglobal(@__MODULE__, :undefvar)
            end
        end
        report = only(get_reports_with_test(result))
        @test report isa GlobalUndefVarErrorReport
        @test report.name === :undefvar
    end
end

@static isdefined(@__MODULE__, :setglobal!) && @testset "InvalidGlobalAssignmentError" begin
    global __int_globalvar__::Int
    let result = report_call((Nothing,)) do x
            setglobal!(@__MODULE__, :__int_globalvar__, x)
        end
        report = only(get_reports_with_test(result))
        @test report isa InvalidGlobalAssignmentError
        @test report.mod === @__MODULE__
        @test report.name === :__int_globalvar__
    end
end

@testset "report non-boolean condition error" begin
    # simple case
    let result = report_call((Int,)) do a
            a ? a : nothing
        end
        @test length(get_reports_with_test(result)) === 1
        er = first(get_reports_with_test(result))
        @test er isa NonBooleanCondErrorReport
        @test er.t === Int
    end

    # don't report when a type can be `Bool` (Bool ⊑ type)
    let result = report_call((Integer,)) do a
            a ? a : nothing
        end
        @test isempty(get_reports_with_test(result))
    end

    # report union split case
    let result = report_call((Union{Nothing,Bool},)) do a
            a ? a : false
        end
        @test length(get_reports_with_test(result)) === 1
        let r = first(get_reports_with_test(result))
            @test r isa NonBooleanCondErrorReport
            @test r.t == [Nothing]
            @test occursin("(1/2 union split)", get_msg(r))
        end
    end

    # sound mode
    let result = report_call() do
            anyary = Any[1,2,3]
            first(anyary) ? first(anyary) : nothing
        end
        @test isempty(get_reports_with_test(result)) # very untyped, we don't report this by default
    end
    let result = report_call(; mode=:sound) do
            anyary = Any[1,2,3]
            first(anyary) ? first(anyary) : nothing
        end
        @test any(get_reports_with_test(result)) do @nospecialize report # very untyped, the sound mode should report this
            report isa NonBooleanCondErrorReport &&
            report.uncovered &&
            report.t === Any
        end
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
        @test !isempty(get_reports_with_test(result))
        @test any(get_reports_with_test(result)) do r
            r isa SeriousExceptionReport || return false
            err = r.err
            err isa UndefKeywordError && err.var === :kw
        end
        # there shouldn't be duplicated report for the `throw` call
        @test !any(isa2(UncaughtExceptionReport), get_reports_with_test(result))
    end
end

@testset "DivideError" begin
    isa2(t) = x -> isa(x, t)
    let
        apply(f, args...) = f(args...)

        result = report_call() do
            apply(div, 1, 0)
        end
        @test !isempty(get_reports_with_test(result))
        @test any(isa2(DivideErrorReport), get_reports_with_test(result))

        result = report_call() do
            apply(rem, 1, 0)
        end
        @test !isempty(get_reports_with_test(result))
        @test any(isa2(DivideErrorReport), get_reports_with_test(result))

        # JET analysis isn't sound
        result = report_call((Int,Int)) do a, b
            apply(div, a, b)
        end
        @test isempty(get_reports_with_test(result))
    end
end

@testset "report `throw` calls" begin
    # simplest case
    let
        result = report_call(()->throw("foo"))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end

    # throws in deep level
    let
        foo(a) = throw(a)
        result = report_call(()->foo("foo"))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end

    # don't report possibly false negative `throw`s
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(foo, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    # constant prop sometimes helps exclude false negatives
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(()->foo(0))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
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
        @test length(get_reports_with_test(result)) === 3
        test_sum_over_string(get_reports_with_test(result))
    end

    # end to end
    let
        # this should report `throw(ArgumentError("Sampler for this object is not defined")`
        result = report_call(rand, (Char,))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport

        # this should not report `throw(DomainError(x, "sin(x) is only defined for finite x."))`
        result = report_call(sin, (Int,))
        @test isempty(get_reports_with_test(result))

        # again, constant prop sometimes can exclude false negatives
        result = report_call(()->sin(Inf))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end
end

@testset "keyword argument methods" begin
    result = Core.eval(gen_virtual_module(), quote
        f(a; b = nothing, c = nothing) = return
        $report_call((Any,)) do b
            f(1; b)
        end
    end)
    @test isempty(get_reports_with_test(result))
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
    @test length(res.res.inference_error_reports) === 2
    @test any(res.res.inference_error_reports) do er
        return er isa NoFieldErrorReport &&
               er.typ === vmod.Foo2 &&
               er.name === :bar
    end
    @test any(res.res.inference_error_reports) do er
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
    @test length(get_reports_with_test(result)) == 1
    r = first(get_reports_with_test(result))
    @test isa(r, InvalidInvokeErrorReport)

    # invalid `argtype`
    result = report_call() do
        Base.@invoke sin(1.0::Int)
    end
    @test length(get_reports_with_test(result)) == 1
    r = first(get_reports_with_test(result))
    @test isa(r, InvalidInvokeErrorReport)

    # don't report errors collected in `invoke`d functions
    result = @eval Module() begin
        foo(i::Integer) = throw(string(i))
        $report_call((Int,)) do a
            Base.@invoke foo(a::Integer)
        end
    end
    @test !isempty(get_reports_with_test(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports_with_test(result))

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
    @test !isempty(get_reports_with_test(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports_with_test(result))
    # virtual stack trace should include frames of both `bar` and `baz`
    # we already `@assert` it within `typeinf` when `JET_DEV_MODE` is enabled,
    # but test it explicitly here just to make sure it's working
    @test all(get_reports_with_test(result)) do r
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
        @test isempty(get_reports_with_test(result))
    end

    let # successful code generation, invalid code
        result = report_call(m.foo, (Float64,))
        @test length(get_reports_with_test(result)) == 1
        r = first(get_reports_with_test(result))
        @test isa(r, GlobalUndefVarErrorReport)
        @test r.name === :undefvar
    end

    let # unsuccessful code generation
        result = report_call(m.foo, (String,))
        @test length(get_reports_with_test(result)) == 1
        r = first(get_reports_with_test(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end

    let # should work if cached
        result = report_call(m.bar, (String,))
        @test length(get_reports_with_test(result)) == 1
        r = first(get_reports_with_test(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end
end

@testset "report invalid builtin call" begin
    result = report_call((Int, Type{Int}, Any)) do a, b, c
        isa(a, b, c)
    end
    report = only(get_reports_with_test(result))
    @test report isa BuiltinErrorReport
    @test report.f === isa
    @test widenconst.(report.argtypes) == [Int, Type{Int}, Any]

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
        @test isempty(get_reports_with_test(result))

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:w), (T,))
        end)
        @test length(get_reports_with_test(result)) === 1
        er = first(get_reports_with_test(result))
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(get_reports_with_test(result))
    end
end

mutable struct SingleField
    x
end
@testset "NoFieldErrorReport" begin
    let result = report_call() do
            x = SingleField("foo")
            getfield(x, :y)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            x = SingleField("foo")
            getfield(x, 2)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            fieldtype(SingleField, :y)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            fieldtype(SingleField, 2)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            x = SingleField("foo")
            setfield!(x, :y, "bar")
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            x = SingleField("foo")
            setfield!(x, :y, 2)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end

    let result = report_call() do
            x = SingleField("foo")
            x.y
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
    let result = report_call() do
            x = SingleField("foo")
            x.y = "bar"
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
    end
end

@testset "getfield analysis" begin
    # analysis on malformed `getfield` shouldn't error
    let result = report_call((Any,)) do a
            getfield(a)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
    end

    let result = report_call() do
            getfield((1,2,3), :x)
        end
        @test only(get_reports_with_test(result)) isa NoFieldErrorReport
        let buf = IOBuffer()
            print(buf, result)
            s = String(take!(buf))
            @test occursin("type Tuple has no field x", s)
        end
    end

    let result = report_call() do
            getfield(@__MODULE__, 42)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport
        let buf = IOBuffer()
            try
                getfield(@__MODULE__, 42)
            catch err
                print(buf, result)
            end
            expected = String(take!(buf))
            print(buf, result)
            got = String(take!(buf))
            @test occursin(expected, got)
        end
    end
end

@testset "setfield! analysis" begin
    # analysis on malformed `setfield!` shouldn't error
    let result = report_call((Any,)) do a
            setfield!(a)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
    end

    # `setfield!` to a module raises an error
    let result = report_call() do
            setfield!(@__MODULE__, :___xxx___, 42)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport
        @test report.f === setfield!
        @test report.msg == JET.SETFIELD!_ON_MODULE_MSG
    end

    # mutability check
    let result = report_call((String,)) do s
            x = Some("julia")
            x.value = s
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport
        @test report.f === setfield!
    end
end

@testset "special case `return_type`" begin
    # don't report invalid method calls simulated in `return_type_tfunc`
    let
        result = report_call(()->CC.return_type(sum, Tuple{String}))
        @test isempty(get_reports_with_test(result))
    end

    # report invalid call of `return_type` itself
    let
        result = report_call(()->CC.return_type(sum))
        @test length(get_reports_with_test(result)) == 1
        @test isa(first(get_reports_with_test(result)), InvalidReturnTypeCall)
    end

    # end to end
    let
        # this shouldn't report "no matching method found for call signature: Base.iterate(itr::DataType)",
        # which otherwise will be caught in `abstract_cal` in `return_type_tfunc`
        result = report_call(() -> Dict('a' => 1, :b => 2))
        @test isempty(get_reports_with_test(result))
    end
end

@testset "configured_reports" begin
    M = Module()
    @eval M begin
        function foo(a)
            r1 = sum(a)       # => Base: MethodError(+(::Char, ::Char)), MethodError(zero(::Type{Any}))
            r2 = undefsum(a)  # => @__MODULE__: UndefVarError(:undefsum)
            return r1, r2
        end
    end

    let result = @report_call M.foo("julia")
        test_sum_over_string(result)
        @test any(get_reports_with_test(result)) do report
            isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
        end
    end

    let result = @report_call target_modules=(M,) M.foo("julia")
        report = only(get_reports_with_test(result))
        @test isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
    end

    let result = @report_call target_modules=(AnyFrameModule(M),) M.foo("julia")
        test_sum_over_string(result)
        @test any(get_reports_with_test(result)) do report
            isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
        end
    end

    let result = @report_call ignored_modules=(Base,) M.foo("julia")
        report = only(get_reports_with_test(result))
        @test isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
    end

    let result = @report_call ignored_modules=(M,) M.foo("julia")
        test_sum_over_string(result)
        @test !any(get_reports_with_test(result)) do report
            isa(report, GlobalUndefVarErrorReport) && report.name === :undefsum
        end
    end
end

issue363(f, args...) = f(args...)

@testset "BasicPass" begin
    @testset "basic_filter" begin
        # skip errors on abstract dispatch
        # https://github.com/aviatesk/JET.jl/issues/154
        let res = @analyze_toplevel analyze_from_definitions=true begin
                struct Foo
                    x::AbstractVector{<:AbstractString}
                end
            end
            @test isempty(res.res.inference_error_reports)
        end

        # https://github.com/aviatesk/JET.jl/issues/363
        let res = report_call() do
                issue363(sin, "42")
            end
            @test only(get_reports_with_test(res)) isa MethodErrorReport
        end

        # should still report anything within entry frame
        let res = @eval Module() begin
                foo(a::Int) = "hello"
                $report_call((AbstractString,)) do a # this abstract call isn't concrete dispatch
                    foo(a)
                end
            end
            @test !isempty(get_reports_with_test(res))
            @test any(r->isa(r,MethodErrorReport), get_reports_with_test(res))
        end

        # skip errors on abstract entry frame entered by `analyze_from_definitions!`
        let res = @analyze_toplevel analyze_from_definitions=true begin
                struct Foo end
                function isfoo(x)
                    # ==(::Missing, ::Foo) -> Missing will lead to `NonBooleanCondErrorReport` otherwise
                    return x == Foo() ? :foo : :bar
                end
            end
            @test !any(r->isa(r,NonBooleanCondErrorReport), res.res.inference_error_reports)
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
        @test isempty(get_reports_with_test(result))

        result = report_call((Any,Symbol); mode = :sound) do a, b
            a == b ? 0 : 1
        end
        @test any(get_reports_with_test(result)) do r
            isa(r, NonBooleanCondErrorReport) &&
            r.t == Type[Missing]
        end
    end

    let # `!(t ⊑ Bool)` -> error
        basic = report_call((Integer,)) do cond
            cond ? 0 : 1
        end
        @test isempty(get_reports_with_test(basic))

        sound = report_call((Integer,); mode=:sound) do cond
            cond ? 0 : 1
        end
        @test any(get_reports_with_test(sound)) do r
            isa(r, NonBooleanCondErrorReport) &&
            r.t == Integer
        end
    end

    let # https://github.com/aviatesk/JET.jl/issues/311
        result = report_call((Vector{Any},); mode=:sound) do xs
            xs[5]
        end
        @test only(get_reports_with_test(result)) isa UnsoundBuiltinErrorReport
    end
end

@testset "TypoPass" begin
    @test_call mode=:typo sum("julia") # don't report NoMethodError, etc.

    @testset "global undef var" begin
        let
            result = report_call(()->foo; mode=:typo)
            @test length(get_reports_with_test(result)) === 1
            @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports_with_test(result)).name === :foo
        end

        let # deeper level
            m = @fixturedef begin
                foo(bar) = bar + baz
                qux(a) = foo(a)
            end

            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            @test length(get_reports_with_test(result)) === 1
            @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports_with_test(result)).name === :baz

            # works when cached
            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            @test length(get_reports_with_test(result)) === 1
            @test first(get_reports_with_test(result)) isa GlobalUndefVarErrorReport
            @test first(get_reports_with_test(result)).name === :baz
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
        @test length(get_reports_with_test(result)) === 1
        er = first(get_reports_with_test(result))
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w
    end
end
