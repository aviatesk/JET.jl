module test_jetanalyzer

include("../setup.jl")

@testset "configurations" begin
    @test_throws JET.JETConfigError report_call(function () end; mode=:badmode)

    # cache key should be same for the same configurations
    let cache1 = JET.AnalysisToken(JETAnalyzer()),
        cache2 = JET.AnalysisToken(JETAnalyzer())
        @test cache1 === cache2
    end

    # cache key should be different for different configurations
    let analyzer1 = JETAnalyzer(; max_methods=3),
        analyzer2 = JETAnalyzer(; max_methods=4)
        cache1 = JET.AnalysisToken(analyzer1)
        cache2 = JET.AnalysisToken(analyzer2)
        @test cache1 !== cache2
    end

    # configurations other than `InferenceParams` and analyzer mode
    # shouldn't affect the cache key identity
    let analyzer1 = JETAnalyzer(; toplevel_logger=nothing),
        analyzer2 = JETAnalyzer(; toplevel_logger=IOBuffer())
        cache1 = JET.AnalysisToken(analyzer1)
        cache2 = JET.AnalysisToken(analyzer2)
        @test cache1 === cache2
    end

    # cache key should be different for different analyzer modes
    let analyzer1 = JETAnalyzer(; mode=:basic),
        analyzer2 = JETAnalyzer(; mode=:sound)
        cache1 = JET.AnalysisToken(analyzer1)
        cache2 = JET.AnalysisToken(analyzer2)
        @test cache1 !== cache2
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

func_method_error1(a::Integer) = :Integer
func_method_error2(a::Integer) = :Integer
func_method_error2(a::AbstractString) = "AbstractString"
func_onlyint(::Int) = :ok
func_integer_or_nothing(::Integer) = :ok1
func_integer_or_nothing(::Nothing) = :ok2

@testset "MethodErrorReport" begin
    # report no match case
    # --------------------

    # if there is no method matching case, it should be reported
    let result = report_call((AbstractString,)) do a
            func_method_error1(a)
        end
        report = only(get_reports_with_test(result))
        @test report isa MethodErrorReport
        @test report.t === Tuple{typeof(func_method_error1), AbstractString}
    end

    let result = report_call(()->sum([]))
        report = only(get_reports_with_test(result))
        @test report isa SeriousExceptionReport
        @test report.err isa MethodError
        @test report.err.f === zero
    end

    # if there is no method matching case in union-split, it should be reported
    let result = report_call(a->func_method_error2(a), (Union{Nothing,Int},))
        report = only(get_reports_with_test(result))
        @test report isa MethodErrorReport
        @test report.t == Any[Tuple{typeof(func_method_error2), Nothing}]
    end

    # report uncovered match
    # ----------------------

    # only in :sound mode
    let basic = report_call((Any,)) do x
            func_onlyint(x)
        end,
        sound = report_call((Any,); mode=:sound) do x
            func_onlyint(x)
        end
        @test isempty(get_reports_with_test(basic))
        report = only(get_reports_with_test(sound))
        @test report isa MethodErrorReport
        @test report.uncovered
        @test report.sig[end] === Symbol
    end

    let basic = report_call((Union{Number,Nothing},)) do x
            func_integer_or_nothing(x)
        end,
        sound = report_call((Union{Number,Nothing},); mode=:sound) do x
            func_integer_or_nothing(x)
        end
        @test isempty(get_reports_with_test(basic))
        report = only(get_reports_with_test(sound))
        @test report isa MethodErrorReport
        @test report.uncovered
        @test report.sig[end] === Symbol
    end

    # report both no match error and uncovered match error
    let result = report_call((Union{Integer,Nothing},); mode=:sound) do x
            func_onlyint(x)
        end
        @test length(get_reports_with_test(result)) == 2
        @test any(get_reports_with_test(result)) do report
            # no match for `func_onlyint(::Nothing)`
            report isa MethodErrorReport &&
            !report.uncovered &&
            report.sig[end] === Union{}
        end
        @test any(get_reports_with_test(result)) do report
            # uncovered match for `func_onlyint(::Integer)`
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
        @test report.type === Tuple{typeof(+),Any,Any}
    end
end

issue285(x, y::Vararg{T}) where {T} = T
issue586(t::Vararg{Type{<:T}}) where {T} = T
_func_undefvar(bar) = bar + baz
func_undefvar(a) = _func_undefvar(a)

@noinline callfunc_localundef(f) = f()

@testset "UndefVarErrorReport" begin
    @testset "global" begin
        let result = report_call(()->foo)
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :foo)
        end

        # deeper level
        let result = report_call(func_undefvar, (Int,))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)

            # works when cached
            result = report_call(func_undefvar, (Int,))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :baz)
        end

        let sym = gensym()
            result = @eval report_call() do
                getfield(@__MODULE__, $(QuoteNode(sym)))
            end
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, sym)
        end
        let sym = gensym()
            result = @eval report_call() do
                getglobal(@__MODULE__, $(QuoteNode(sym)))
            end
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, sym)
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
            @test_broken isempty(res.res.inference_error_reports)
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

    @testset "local undef var" begin
        let result = report_call((Bool,)) do b
                if b
                    bar = rand(Int)
                    return bar
                end
                return bar # undefined in this pass
            end
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r; name=:bar, maybeundef=false)
            @test last(r.vst).line == (@__LINE__)-4
        end

        let result = report_call((Bool,)) do b
                if b
                    bar = 42
                end
                return bar # may be undefined
            end
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r; name=:bar, maybeundef=true)
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
            @test is_local_undef_var(r; name=:bar)
            @test last(r.vst).line == (@__LINE__)-8

            # works when cached
            result = Core.eval(m, :($report_call(baz, (Bool,))))
            r = only(get_reports_with_test(result))
            @test is_local_undef_var(r; name=:bar)
            @test last(r.vst).line == (@__LINE__)-14
        end

        # @isdefined integration
        let result = report_call((Bool,)) do b
                if b
                    bar = rand()
                end

                if @isdefined bar
                    return bar # this shouldn't be reported
                else
                    return nothing
                end
            end
            @test isempty(get_reports_with_test(result))
        end

        # should not report false positives for closure patterns
        let result = report_call() do
                local res = nothing
                callfunc_localundef() do
                    println(res)
                end
            end
            @test isempty(get_reports_with_test(result))
        end
        let result = report_call() do
                local res
                callfunc_localundef() do
                    println(res)
                end
            end
            @test_broken is_local_undef_var(only(get_reports(result)); name=:res)
        end
        let result = report_call() do
                local a
                @noinline function inner(n)
                    if n > 0
                       a = n
                    end
                end
                inner(rand(Int))
                return a
            end
            @test_broken is_local_undef_var(only(get_reports(result)); name=:res)
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
            r = only(res.res.inference_error_reports)
            @test is_local_undef_var(r; name=:bar)
        end
    end

    @testset "static parameter" begin
        @test_call issue285(1, 2)
        let result = @report_call issue285(1)
            r = only(get_reports_with_test(result))
            @test r isa UndefVarErrorReport && r.var isa TypeVar && r.var.name == :T
        end
        test_call((Vector{Any},)) do xs
            issue285(xs...)
        end
        let result = report_call((Vector{Any},); mode=:sound) do xs
                issue285(xs...)
            end
            @test any(get_reports_with_test(result)) do r
                r isa UndefVarErrorReport && r.var isa TypeVar && r.var.name == :T
            end
        end
        @test_call issue586(Int, Int)
        let result = @report_call issue586(Int, String)
            r = only(get_reports_with_test(result))
            @test r isa UndefVarErrorReport && r.var isa TypeVar && r.var.name == :T
        end
        test_call((Vector{Type},)) do ts
            issue586(ts...)
        end
        let result = report_call((Vector{Type},); mode=:sound) do ts
                issue586(ts...)
            end
            @test any(get_reports_with_test(result)) do r
                r isa UndefVarErrorReport && r.var isa TypeVar && r.var.name == :T
            end
        end
    end
end

global __int_globalvar__::Int
let result = report_call((Nothing,)) do x
        setglobal!(@__MODULE__, :__int_globalvar__, x)
    end
    report = only(get_reports_with_test(result))
    @test report isa IncompatibleGlobalAssignmentError
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

func_undef_keyword(a; kw) =  a, kw # `kw` can be undefined
@testset "UndefKeywordError" begin
    result = report_call(func_undef_keyword, (Any,))
    @test !isempty(get_reports_with_test(result))
    @test any(get_reports_with_test(result)) do r
        r isa SeriousExceptionReport || return false
        err = r.err
        err isa UndefKeywordError && err.var === :kw
    end
    # there shouldn't be duplicated report for the `throw` call
    @test !any(r->isa(r,UncaughtExceptionReport), get_reports_with_test(result))
end

func_invalid_index(xs, i) = xs[i]
func_invalid_index(xs::Vector{Int}) = xs[findfirst(>(0), xs)]
@testset "report invalid index" begin
    let result = report_call(func_invalid_index, (Vector{Int},Nothing))
        @test count(get_reports_with_test(result)) do r
            r isa SeriousExceptionReport
        end == 1
    end
    let result = report_call(func_invalid_index, (Vector{Int},))
        @test count(get_reports_with_test(result)) do r
            r isa SeriousExceptionReport
        end == 1
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

func_throw_call_foo(a) = sum(a)
func_throw_call_bar(a) = throw(a)

@testset "report `throw` calls" begin
    # simplest case
    let result = report_call(()->throw("foo"))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end

    # throws in deep level
    let foo(a) = throw(a)
        result = report_call(()->foo("foo"))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end

    # don't report possibly false negative `throw`s
    let foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(foo, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    # constant prop sometimes helps exclude false negatives
    let foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        result = report_call(()->foo(0))
        @test !isempty(get_reports_with_test(result))
        @test first(get_reports_with_test(result)) isa UncaughtExceptionReport
    end

    # report even if there're other "critical" error exist
    let result = report_call((Bool,String)) do b, s
            b && func_throw_call_foo(s)
            func_throw_call_bar(s)
        end
        @test length(get_reports_with_test(result)) === 3
        test_sum_over_string(get_reports_with_test(result))
    end

    # end to end
    let # this should report `throw(ArgumentError("Sampler for this object is not defined")`
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

func_keyword_argument(a; b = nothing, c = nothing) = nothing
let result = report_call((Any,)) do b
        func_keyword_argument(1; b)
    end
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

        global f::Union{Foo1, Foo2, Foo3} # COMBAK 1.12 update
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

abstract_invoke1(i::Integer) = throw(string(i))
#== LINE SENSITIVITY START ===#
const _ABSTRACT_INVOKE2_LINE = (@__LINE__) + 2
const ABSTRACT_INVOKE2_LINE = (@__LINE__) + 4
_abstract_invoke2(a) = return a + undefvar
function abstract_invoke2(a)
    a += 1
    return @invoke _abstract_invoke2(a::Int) # `invoke` is valid, but error should happen within `bar`
end
#== LINE SENSITIVITY END ===#
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
    result = report_call((Int,)) do a
        @invoke abstract_invoke1(a::Integer)
    end
    @test !isempty(get_reports_with_test(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports_with_test(result))

    result = report_call(abstract_invoke2, (Any,))
    #== LINE SENSITIVITY END ===#
    @test !isempty(get_reports_with_test(result))
    @test !any(r->isa(r, InvalidInvokeErrorReport), get_reports_with_test(result))
    # virtual stack trace should include frames of both `bar` and `baz`
    # we already `@assert` it within `typeinf` when `JET_DEV_MODE` is enabled,
    # but test it explicitly here just to make sure it's working
    @test all(get_reports_with_test(result)) do r
        any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == _ABSTRACT_INVOKE2_LINE && # `_abstract_invoke2`
            vf.linfo.def.name === :_abstract_invoke2
        end || return false
        any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == ABSTRACT_INVOKE2_LINE && # `abstract_invoke2`
            vf.linfo.def.name === :abstract_invoke2
        end || return false
        return true
    end
end

@generated function staged_func(a)
    if a <: Integer
        return :(a)
    elseif a <: Number
        return :(undefvar) # report me this case
    end
    throw("invalid argument")
end
call_staged_func(args...) = staged_func(args...)
@testset "staged programming" begin
    let # successful code generation, valid code
        result = report_call(staged_func, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    let # successful code generation, invalid code
        result = report_call(staged_func, (Float64,))
        @test length(get_reports_with_test(result)) == 1
        r = only(get_reports_with_test(result))
        @test is_global_undef_var(r, :undefvar)
    end

    let # unsuccessful code generation
        result = report_call(staged_func, (String,))
        @test length(get_reports_with_test(result)) == 1
        r = only(get_reports_with_test(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end

    let # should work if cached
        result = report_call(call_staged_func, (String,))
        @test length(get_reports_with_test(result)) == 1
        r = first(get_reports_with_test(result))
        @test isa(r, GeneratorErrorReport) && r.err == "invalid argument"
    end
end

struct InvalidBuiltinStruct; v; end
access_field(x::InvalidBuiltinStruct, sym) = getfield(x, sym)
@testset "report invalid builtin call" begin
    let result = report_call((Int, Type{Int}, Any)) do a, b, c
            isa(a, b, c)
        end
        report = only(get_reports_with_test(result))
        @test report isa BuiltinErrorReport && report.f === isa
    end

    @testset "constant propagation" begin
        let result = report_call(x::InvalidBuiltinStruct->access_field(x,:v))
            @test isempty(get_reports_with_test(result))
        end
        let result = report_call(x::InvalidBuiltinStruct->access_field(x,:w))
            report = only(get_reports_with_test(result))
            @test report isa BuiltinErrorReport && report.f === getfield
        end
        let result = report_call(x::InvalidBuiltinStruct->access_field(x,:v))
            @test isempty(get_reports_with_test(result))
        end
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
func_report_entry(a::Int) = "hello"

@testset "BasicJETAnalyzer" begin
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
        let res = report_call((AbstractString,)) do a # this abstract call isn't concrete dispatch
                func_report_entry(a)
            end
            @test !isempty(get_reports_with_test(res))
            @test any(r->isa(r,MethodErrorReport), get_reports_with_test(res))
        end
    end
end

@testset ":sound mode" begin
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
end

struct NoFieldStruct; v; end
access_field(x::NoFieldStruct, sym) = getfield(x, sym)

@testset ":typo mode" begin
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

@testset "intrinsic errors" begin
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
@test Base.infer_return_type(; interp=JET.JETAnalyzer()) do
    Val(fieldcount(Int))
end === Val{0}
@test Base.infer_return_type(; interp=JET.JETAnalyzer()) do
    Val(fieldcount(Vector))
end === Val{2}
struct CheckFieldIndex; a; end
@test Base.infer_return_type(; interp=JET.JETAnalyzer()) do
    Val(Base.fieldindex(CheckFieldIndex, :a))
end === Val{1}
@test @eval Base.infer_return_type(; interp=JET.JETAnalyzer()) do
    Val(length($(Core.svec(1,2,3))))
end === Val{3}
@test_call sort([1,2,3])
@test_call sort!([1,2,3])
# aviatesk/JET.jl#669
struct Point669{dim,T}
    coord::NTuple{dim,T}
end
getcoordinate669(n) = n.coord
f669(p) = getcoordinate669.(p)
let pts = Point669.(rand(NTuple{2,Float64}, 10))
    @test_call f669(pts)
end

@test isconcretetype(only(Base.return_types(pairs, (@NamedTuple{kw1::Int,kw2::String},); interp=JET.JETAnalyzer())))

# Broken: cache consistency with undefined bindings (xref: JuliaLang/julia#40399)
func40399() = sin(Main.binding40399)
let res = report_call() do
        func40399()
    end
    @test only(get_reports_with_test(res)) isa UndefVarErrorReport
end
global binding40399::Float64 = rand()
test_call(; broken=true) do
    func40399()
end

# aviatesk/JET.jl#695
test_call((Vector{Int},String)) do a, b
    if Base.isvatuple(Tuple{Int,Int})
        sum(b)
    else
        sum(a)
    end
end

end # module test_jetanalyzer
