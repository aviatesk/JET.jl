module test_jetanalyzer

include("../setup.jl")

@testset "configurations" begin
    @test_throws JET.JETConfigError report_call(function () end; mode=:badmode)
    @test_throws JET.JETConfigError report_call(function () end; report_pass=JET.BasicPass(), mode=:sound)

    # cache key should be same for the same configurations
    let cache1 = JET.AnalysisCache(JETAnalyzer()),
        cache2 = JET.AnalysisCache(JETAnalyzer())
        @test cache1 == cache2
    end

    # cache key should be different for different configurations
    let analyzer1 = JETAnalyzer(; max_methods=3),
        analyzer2 = JETAnalyzer(; max_methods=4)
        cache1 = JET.AnalysisCache(analyzer1)
        cache2 = JET.AnalysisCache(analyzer2)
        @test cache1 ≠ cache2
    end

    # configurations other than `InferenceParams` and `ReportPass`
    # shouldn't affect the cache key identity
    let analyzer1 = JETAnalyzer(; toplevel_logger=nothing),
        analyzer2 = JETAnalyzer(; toplevel_logger=IOBuffer())
        cache1 = JET.AnalysisCache(analyzer1)
        cache2 = JET.AnalysisCache(analyzer2)
        @test cache1 == cache2
    end

    # cache key should be different for different report passes
    let analyzer1 = JETAnalyzer(; report_pass=JET.BasicPass()),
        analyzer2 = JETAnalyzer(; report_pass=JET.SoundPass())
        cache1 = JET.AnalysisCache(analyzer1)
        cache2 = JET.AnalysisCache(analyzer2)
        @test cache1 ≠ cache2
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
        @test any(r->is_global_undef_var(r, :undefvar), get_reports_with_test(result))

        # should run the cached previous result
        result = @eval m $report_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(r->is_global_undef_var(r, :undefvar), get_reports_with_test(result))
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

    # unanalyzed calls
    # only in :sound mode
    test_call((Any,Any)) do x, y
        x + y
    end
    let result = report_call((Any,Any); mode=:sound) do x, y
             x + y
        end
        report = only(get_reports_with_test(result))
        @test report isa UnanalyzedCallReport
        report.type === Tuple{typeof(+),Any,Any}
    end
end

@testset "UndefVarErrorReport" begin
    @testset "global" begin
        let result = report_call(()->foo)
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :foo)
        end

        # deeper level
        let m = @fixturedef begin
                foo(bar) = bar + baz
                qux(a) = foo(a)
            end

            result = Core.eval(m, :($report_call(qux, (Int,))))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)

            # works when cached
            result = Core.eval(m, :($report_call(qux, (Int,))))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)
        end

        let result = @eval Module() begin
                $report_call() do
                    getfield(@__MODULE__, :undefvar)
                end
            end
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :undefvar)
        end

        let result = @eval Module() begin
                $report_call() do
                    getglobal(@__MODULE__, :undefvar)
                end
            end
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :undefvar)
        end

        # if a global variable is type-declared, it will likely get assigned somewhere
        let res = @analyze_toplevel analyze_from_definitions=true begin
                global var::String
                function __init__()
                    global var
                    var = "init"
                end
                getvar() = (global var; var)
            end
            @test isempty(res.res.inference_error_reports)
        end
        # but the sound mode should still be sound
        let res = @analyze_toplevel mode=:sound analyze_from_definitions=true begin
                global var::String
                function __init__()
                    global var
                    var = "init"
                end
                getvar() = (global var; var)
            end
            r = only(get_reports_with_test(res))
            @test is_global_undef_var(r, :var)
        end
    end

    @static false && @testset "local" begin
        let result = report_call((Bool,)) do b
                if b
                    bar = rand(Int)
                    return bar
                end
                return bar # undefined in this pass
            end
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r, :bar)
            @test last(r.vst).line == (@__LINE__)-4
        end

        # deeper level
        let m = @fixturedef begin
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
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r, :bar)
            @test last(r.vst).line == (@__LINE__)-8

            # works when cached
            result = Core.eval(m, :($report_call(baz, (Bool,))))
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r, :bar)
            @test last(r.vst).line == (@__LINE__)-14
        end

        # try to exclude false negatives as possible (by collecting reports in after-optimization pass)
        let result = report_call((Bool,)) do b
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

        let result = report_call((Bool,)) do b
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
                is_local_undef_var(get_reports_with_test(result), :bar)
        end

        # TODO better closure handling
        let result = report_call() do
                local a
                function inner(n)
                    if n > 0
                       a = n
                    end
                end
                inner(rand(Int))
                return a
            end
            @test_broken isempty(get_reports_with_test(result))
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
                @test is_local_undef_var(r, :bar)
            end
        end
    end
end

global __int_globalvar__::Int
let result = report_call((Nothing,)) do x
        setglobal!(@__MODULE__, :__int_globalvar__, x)
    end
    report = only(get_reports_with_test(result))
    @test report isa InvalidGlobalAssignmentError
    @test report.mod === @__MODULE__
    @test report.name === :__int_globalvar__
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
    let result = report_call() do
            div(1, 0)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport
        @test report.msg == JET.DIVIDE_ERROR_MSG
    end
    let result = report_call() do
            rem(1, 0)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport
        @test report.msg == JET.DIVIDE_ERROR_MSG
    end
    # JETAnalyzer isn't sound
    let result = report_call((Int,Int)) do a, b
            div(a, b)
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
    @test all(res.res.inference_error_reports) do report
        report isa BuiltinErrorReport && report.f === getfield
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
        @invoke sin(1.0::Int)
    end
    @test length(get_reports_with_test(result)) == 1
    r = first(get_reports_with_test(result))
    @test isa(r, InvalidInvokeErrorReport)

    # don't report errors collected in `invoke`d functions
    result = @eval Module() begin
        foo(i::Integer) = throw(string(i))
        $report_call((Int,)) do a
            @invoke foo(a::Integer)
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
            @invoke bar(a::Int) # `invoke` is valid, but error should happen within `bar`
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
        @test is_global_undef_var(r, :undefvar)
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

struct InvalidBuiltinStruct; v; end
access_field(x::InvalidBuiltinStruct, sym) = getfield(x, sym)

@testset "report invalid builtin call" begin
    result = report_call((Int, Type{Int}, Any)) do a, b, c
        isa(a, b, c)
    end
    report = only(get_reports_with_test(result))
    @test report isa BuiltinErrorReport && report.f === isa

    @testset "constant propagation" begin
        result = report_call(x::InvalidBuiltinStruct->access_field(x,:v))
        @test isempty(get_reports_with_test(result))

        result = report_call(x::InvalidBuiltinStruct->access_field(x,:w))
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === getfield

        result = report_call(x::InvalidBuiltinStruct->access_field(x,:v))
        @test isempty(get_reports_with_test(result))
    end
end

function test_builtinerror_compatibility(@nospecialize(reproducer), result)
    buf = IOBuffer()
    try
        reproducer()
        @assert false "reproducer doesn't produce a target error"
    catch err
        Base.showerror(buf, err)
    end
    expected = String(take!(buf))
    print(buf, result)
    got = String(take!(buf))
    @test occursin(expected, got)
end

mutable struct SingleField
    x
end
@testset "no field error report" begin
    let result = report_call() do
            x = SingleField("foo")
            getfield(x, :y)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            getfield(x, :y)
        end
    end
    let result = report_call() do
            x = SingleField("foo")
            getfield(x, 2)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            getfield(x, 2)
        end
    end
    let result = report_call() do
            fieldtype(SingleField, :y)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            fieldtype(SingleField, :y)
        end
    end
    let result = report_call() do
            fieldtype(SingleField, 2)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        # XXX Julia doesn't render this error nicely
        # test_builtinerror_compatibility(result) do
        #     fieldtype(SingleField, 2)
        # end
    end
    let result = report_call() do
            x = SingleField("foo")
            setfield!(x, :y, "bar")
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            setfield!(x, :y, "bar")
        end
    end
    let result = report_call() do
            x = SingleField("foo")
            setfield!(x, :y, 2)
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            setfield!(x, :y, 2)
        end
    end

    let result = report_call() do
            x = SingleField("foo")
            x.y
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            x.y
        end
    end
    let result = report_call() do
            x = SingleField("foo")
            x.y = "bar"
        end
        @test only(get_reports_with_test(result)) isa BuiltinErrorReport
        test_builtinerror_compatibility(result) do
            x = SingleField("foo")
            x.y = "bar"
        end
    end
end

@testset "getfield analysis" begin
    # analysis on malformed `getfield` shouldn't error
    let result = report_call((Any,)) do a
            getfield(a)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === getfield
    end

    let result = report_call() do
            getfield((1,2,3), :x)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === getfield
        test_builtinerror_compatibility(result) do
            getfield((1,2,3), :x)
        end
    end

    let result = report_call() do
            getfield(@__MODULE__, 42)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === getglobal
        # XXX Julia raises `BoundsError` when ran in the compiler
        # test_builtinerror_compatibility(result) do
        #     getfield(@__MODULE__, 42)
        # end
    end
end

@testset "setfield! analysis" begin
    # analysis on malformed `setfield!` shouldn't error
    let result = report_call((Any,)) do a
            setfield!(a)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === setfield!
    end

    # `setfield!` to a module raises an error
    let result = report_call() do
            setfield!(@__MODULE__, :___xxx___, 42)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === setfield!
        test_builtinerror_compatibility(result) do
            setfield!(@__MODULE__, :___xxx___, 42)
        end
    end

    # mutability check
    let result = report_call((String,)) do s
            x = Some("julia")
            x.value = s
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === setfield!
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
    let # this shouldn't report "no matching method found for call signature: Base.iterate(itr::DataType)",
        # which otherwise will be caught in `abstract_call` in `return_type_tfunc`
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
        @test any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
    end

    let result = @report_call target_modules=(M,) M.foo("julia")
        r = only(get_reports_with_test(result))
        @test is_global_undef_var(r, :undefsum)
    end

    let result = @report_call target_modules=(AnyFrameModule(M),) M.foo("julia")
        test_sum_over_string(result)
        @test any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
    end

    let result = @report_call ignored_modules=(Base,) M.foo("julia")
        r = only(get_reports_with_test(result))
        @test is_global_undef_var(r, :undefsum)
    end

    let result = @report_call ignored_modules=(M,) M.foo("julia")
        test_sum_over_string(result)
        @test !any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
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

    # https://github.com/aviatesk/JET.jl/issues/311
    @static if VERSION ≥ v"1.11.0-DEV.753"
        let result = report_call((Vector{Int},); mode=:sound) do xs
                xs[5]
            end
            reports = get_reports_with_test(result)
            @test length(reports) == 1 # bounds error check
        end
        let result = report_call((Vector{Any},); mode=:sound) do xs
                xs[5]
            end
            reports = get_reports_with_test(result)
            @test length(reports) == 2 # bounds error check + potential UndefRefError
        end
    else
        let result = report_call((Vector{Int},); mode=:sound) do xs
                xs[5]
            end
            @test only(get_reports_with_test(result)) isa UnsoundBuiltinErrorReport
        end
    end
end

struct NoFieldStruct; v; end
access_field(x::NoFieldStruct, sym) = getfield(x, sym)


@testset "TypoPass" begin
    @test_call mode=:typo sum("julia") # don't report NoMethodError, etc.

    @testset "global undef var" begin
        let
            result = report_call(()->foo; mode=:typo)
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :foo)
        end

        let # deeper level
            m = @fixturedef begin
                foo(bar) = bar + baz
                qux(a) = foo(a)
            end

            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)

            # works when cached
            result = Core.eval(m, :($report_call(qux, (Int,); mode=:typo)))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)
        end
    end

    @testset "no field" begin
        result = report_call(x::NoFieldStruct->access_field(x,:w); mode=:typo)
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === getfield
    end
end

# https://github.com/aviatesk/JET.jl/issues/404
function issue_404(c::Bool)
    x = c ? () : (1,)
    for v in x
        println(v)
    end
end
test_call(issue_404, (Bool,))

@static VERSION ≥ v"1.10.0-DEV.197" && @testset "intrinsic errors" begin
    let result = report_call((Int32,Int64)) do x, y
            return Core.Intrinsics.add_int(x, y)
        end
        r = only(get_reports_with_test(result))
        @test r isa BuiltinErrorReport && r.f === Core.Intrinsics.add_int
        err = (try
            Core.Intrinsics.add_int(zero(Int32), zero(Int64))
        catch err
            err
        end)::ErrorException
        @test err.msg == r.msg
    end

    let result = report_call((Int32,)) do x
            return Core.Intrinsics.bitcast(Int64, x)
        end
        r = only(get_reports_with_test(result))
        @test r isa BuiltinErrorReport && r.f === Core.Intrinsics.bitcast
        err = (try
            Core.Intrinsics.bitcast(Int64, zero(Int32))
        catch err
            err
        end)::ErrorException
        @test err.msg == r.msg
    end
end

# https://github.com/JuliaLang/julia/pull/49801
test_call(Base.aligned_sizeof, (Union{DataType,Union},))

# special case on `reduce_empty` and `mapreduce_empty`
# In the `:basic` mode, downgrade `MethodErrorReport` on call of `reduce_empty` or `mapreduce_empty`
# to `UncaughtExceptionReport` so that it can be filtered out in common cases.
# xref: https://github.com/JuliaLang/julia/pull/41885/
@test_call maximum(length, ["f", "ba", "baz"])
@test_call maximum(length, ["f", "ba", "baz"]; init=0)

# We should still be able to get an error that is obvious from the original `reduce_empty` definitions:
# NOTE Since method matches with overlay method table do not consider method specialities
# across both the native method table and the overlay one, we can't do something like:
# `@overlay JET_METHOD_TABLE Base.reduce_empty(op, T) = Base._empty_reduce_error(op, T)`
# since it it does not allow dispatch on `Base.reduce_empty(::typeof(add_sum), ::Type{Union{}})`
# that is in the native method table (as the overlay definition fully-covers)
@test !isempty(get_reports_with_test(@report_call sum(())))
@test_call sum((1,2,3))
@test_call sum(Int[])

# allow concrete evaluation for core reflection methods
# https://github.com/aviatesk/JET.jl/issues/522
@test Base.return_types(; interp=JET.JETAnalyzer()) do
    Val(fieldcount(Int))
end |> only === Val{0}
let n = @static VERSION ≥ v"1.11.0-DEV.753" ? 2 : 0
    @test Base.return_types(; interp=JET.JETAnalyzer()) do
        Val(fieldcount(Vector))
    end |> only === Val{n}
end
struct CheckFieldIndex; a; end
@test Base.return_types(; interp=JET.JETAnalyzer()) do
    Val(Base.fieldindex(CheckFieldIndex, :a))
end |> only === Val{1}
@test @eval Base.return_types(; interp=JET.JETAnalyzer()) do
    Val(length($(Core.svec(1,2,3))))
end |> only === Val{3}
@test_call sort([1,2,3])
@test_call sort!([1,2,3])

@test isconcretetype(only(Base.return_types(pairs, (@NamedTuple{kw1::Int,kw2::String},); interp=JET.JETAnalyzer())))

end # module test_jetanalyzer
