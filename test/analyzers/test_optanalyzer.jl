# OptAnalyzer
# ===========

getsomething(x::Any) = x
getsomething(x::Array) = x[]
getsomething(::Nothing) = throw(ArgumentError("nothing is nothing"))
getsomething(::Missing) = throw(ArgumentError("too philosophical"))

# bad: will lead to excessive specializations via runtime dispatch
function isType1(x)
    if isa(x, DataType)
        return isa(x, DataType) && x.name === Type.body.name
    elseif isa(x, Union)
        return isType1(x.a) && isType1(x.b)
    elseif isa(x, UnionAll)
        return isType1(x.body)
    else
        return false
    end
end

# good: will be statically dispatched
function isType2(@nospecialize x)
    if isa(x, DataType)
        return isa(x, DataType) && x.name === Type.body.name
    elseif isa(x, Union)
        return isType2(x.a) && isType2(x.b)
    elseif isa(x, UnionAll)
        return isType2(x.body)
    else
        return false
    end
end

@testset "runtime dispatch" begin
    test_opt((Int, Vector{Any}, String,)) do a, b, c
        return (
            getsomething(a),
            getsomething(b),
            getsomething(c),
            getsomething(nothing),
            getsomething(missing))
    end

    # NOTE the following test is line-sensitive !
    # if the argument type isn't well typed, compiler can't determine which method to call,
    # and it will lead to runtime dispatch
    let result = report_opt((Vector{Any},)) do xs
            getsomething(xs[1]) # runtime dispatch !
        end
        @test length(get_reports_with_test(result)) == 1
        r = only(get_reports_with_test(result))
        @test isa(r, RuntimeDispatchReport)
        @test any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == (@__LINE__) - 7
        end
    end

    # union split might help
    test_opt((Vector{Union{Int,String,Nothing}},)) do xs
        getsomething(xs[1]) # runtime dispatch !
    end

    # NOTE the following test is line-sensitive !
    let result = report_opt((Vector{Any},)) do xs
            isType1(xs[1])
        end
        @test length(get_reports_with_test(result)) == 1
        r = only(get_reports_with_test(result))
        @test isa(r, RuntimeDispatchReport)
        @test any(r.vst) do vf
            vf.file === Symbol(@__FILE__) &&
            vf.line == (@__LINE__) - 7
        end
    end
    test_opt((Vector{Any},)) do xs
        isType2(xs[1])
    end

    # real-world targets
    # the `unoptimize_throw_blocks` configuration disables optimizations on "throw blocks" by default,
    # but `DispatchAnalyzer` ignores problems from them, so we don't get error reports here
    @test_opt sin(10)
end

@testset "captured variables" begin
    let result = @eval Module() begin
            function foo(a, n)
                incr! = x -> a += x

                for i = 1:n
                    if isodd(i)
                        incr!(i)
                    end
                end

                return a
            end
            $report_opt(foo, (Int, Int))
        end
        @test any(get_reports_with_test(result)) do report
            return isa(report, CapturedVariableReport) &&
                   report.name === :a
        end
    end

    let M = Module() # we report `Core.Box` whatever it's type-stable
        @eval M begin
            # adapted https://docs.julialang.org/en/v1/manual/performance-tips/#man-performance-captured
            function abmult(r::Int)
                if r < 0
                    r = -r
                end
                f = x -> x * r
                return f
            end

            function abmult2(r0::Int)
                r::Int = r0
                if r < 0
                    r = -r
                end
                f = x -> x * r
                return f
            end

            function abmult3(r::Int)
                if r < 0
                    r = -r
                end
                f = let r = r
                    x -> x * r
                end
                return f
            end
        end

        let result = @report_opt M.abmult(42)
            @test any(get_reports_with_test(result)) do report
                return isa(report, CapturedVariableReport) &&
                       report.name === :r
            end
        end
        let result = @report_opt M.abmult2(42)
            @test any(get_reports_with_test(result)) do report
                return isa(report, CapturedVariableReport) &&
                       report.name === :r
            end
        end

        @test_opt M.abmult3(42) # no captured variable for `abmult3` !
    end
end

@testset "OptAnalyzer configurations" begin
    @testset "function_filter" begin
        M = Module()
        @eval M begin
            with_runtime_dispatch(::UInt8)  = :UInt8
            with_runtime_dispatch(::UInt16) = :UInt16
            with_runtime_dispatch(::UInt32) = :UInt32
            with_runtime_dispatch(::UInt64) = :UInt64
            with_runtime_dispatch(::UInt128) = :UInt128
        end
        @assert JET.OptimizationParams(JET.OptAnalyzer()).MAX_UNION_SPLITTING < 5

        let
            result = @eval M $report_opt((Vector{Any},)) do xs
                with_runtime_dispatch(xs[1])
            end
            @test !isempty(get_reports_with_test(result))
            @test any(r->isa(r,RuntimeDispatchReport), get_reports_with_test(result))
        end

        let
            function_filter(x) = x !== typeof(M.with_runtime_dispatch)
            @eval M $test_opt((Vector{Any},); function_filter=$function_filter) do xs
                with_runtime_dispatch(xs[1])
            end
        end
    end

    @testset "skip_noncompileable_calls" begin
        let M = Module()
            CALLF_DEFINITION_LINE = (@__LINE__)+1
            @eval M callf(f, a) = f(a)

            let # by default, we only report the runtime dispatch within the lambda function,
                # and ignore error reports from `callf` calls
                result = @eval M $report_opt((Vector{Any},)) do ary
                    callf(sin, ary[1]) # runtime dispatch !
                end
                @test length(get_reports_with_test(result)) == 1
                @test any(get_reports_with_test(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == (@__LINE__) - 5 # report for the lambda function
                end
            end

            let # when the `skip_noncompileable_calls` configuration is turned off,
                # we will get error reports from `callsin` as well
                result = @eval M $report_opt((Vector{Any},); skip_noncompileable_calls=false) do ary
                    callf(sin, ary[1]) # runtime dispatch !
                end
                @test length(get_reports_with_test(result)) == 2
                @test any(get_reports_with_test(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == (@__LINE__) - 5 # report for the lambda function
                end
                @test any(get_reports_with_test(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == CALLF_DEFINITION_LINE # report for `f(a::Any)`
                end
            end
        end

        let M = Module()
            CALLG_DEFINITION_LINE = (@__LINE__)+1
            @eval M callg(g, @nospecialize a) = g(a)

            let # `skip_noncompileable_calls` shouldn't ignore `@nospecialize` annotation
                result = @eval M $report_opt((Vector{Any},)) do ary
                    callg(sin, ary[1]) # no runtime dispatch here, but `g(a)` is runtime dispatch
                end
                @test length(get_reports_with_test(result)) == 1
                @test any(get_reports_with_test(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == CALLG_DEFINITION_LINE # report for `g(a::Any)`
                end
            end
        end
    end

    @testset "target_modules" begin
        M = Module()
        @eval M begin
            # problem: when ∑1/n exceeds 30 ?
            function compute(x)
                r = 1
                s = 0.0
                n = 1
                @time while r < x
                    s += 1/n
                    if s ≥ r
                        println("round $r/$x has been finished") # we're not interested type-instabilities within this call
                        r += 1
                    end
                    n += 1
                end
                return n, s
            end
        end

        let # we will get bunch of reports from the `println` call
            result = @report_opt M.compute(30)
            @test !isempty(get_reports_with_test(result))
        end

        let # if we use different `target_modules`, the reports from `println` should get filtered out
            @test_opt target_modules=(@__MODULE__,) M.compute(30)
        end
    end
end

# https://github.com/aviatesk/JET.jl/issues/334
# integration with concrete evaluation added in 1.8
test_opt() do
    Val(:ϵ)
end
@test_opt log(2.1)

# https://github.com/aviatesk/JET.jl/issues/335
# don't report duplicated problems from inlined callees
issue335_callf(f, args...) = f(args...)

@inline function issue335_problematic_callee(val)
    return issue335_undefined_call(val)
end

let result = @report_opt issue335_callf(issue335_problematic_callee, 42)
    report = only(get_reports_with_test(result))
    @test any(report.vst) do vsf
        vsf.line == (@__LINE__)-6 &&
        vsf.linfo.def.name === :issue335_problematic_callee
    end
end

let result = report_opt() do
        issue335_callf(42) do val
            if val < 0
                return issue335_problematic_callee(val)
            end
            return sin(val)
        end
    end
    @test isempty(get_reports_with_test(result))
end
