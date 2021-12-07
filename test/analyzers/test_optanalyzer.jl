# OptAnalyzer
# ===========

@testset "runtime dispatch" begin
    let M = Module()
        @eval M begin
            f(a) = a
            f(a::Number) = a
        end

        # `f(::Int)` a concrete call and just type stable and anything shouldn't be reported
        @test_opt M.f(10)  # should be ok

        let # if the argument type isn't well typed, compiler can't determine which method to call,
            # and it will lead to runtime dispatch
            result = @eval M begin
                $report_opt((Vector{Any},)) do ary
                    f(ary[1]) # runtime dispatch !
                end
            end
            @test length(get_reports(result)) == 1
            r = first(get_reports(result))
            @test isa(r, RuntimeDispatchReport)
        end
    end

    let M = Module()
        @eval M begin
            # if we annotate `@noinline` to a function, then its call won't be inlined and will be
            # dispatched runtime
            @inline   g1(a) = return a
            @noinline g2(a) = return a
        end

        let
            result = @eval M $report_opt((Vector{Any},)) do ary
                a = ary[1]
                g1(a) # this call should be statically resolved and inlined
                g2(a) # this call should be statically resolved but not inlined, and will be dispatched
            end

            # NOTE the following test is line-sensitive !
            @test length(get_reports(result)) == 1
            r = first(get_reports(result))
            @test isa(r, RuntimeDispatchReport)
            @test any(r.vst) do vf
                vf.file === Symbol(@__FILE__) &&
                vf.line == (@__LINE__) - 9
            end
        end
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
        @test any(get_reports(result)) do report
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
            @test any(get_reports(result)) do report
                return isa(report, CapturedVariableReport) &&
                       report.name === :r
            end
        end
        let result = @report_opt M.abmult2(42)
            @test any(get_reports(result)) do report
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
            @test !isempty(get_reports(result))
            @test any(r->isa(r,RuntimeDispatchReport), get_reports(result))
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
                @test length(get_reports(result)) == 1
                @test any(get_reports(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == (@__LINE__) - 5 # report for the lambda function
                end
            end

            let # when the `skip_noncompileable_calls` configuration is turned off,
                # we will get error reports from `callsin` as well
                result = @eval M $report_opt((Vector{Any},); skip_noncompileable_calls=false) do ary
                    callf(sin, ary[1]) # runtime dispatch !
                end
                @test length(get_reports(result)) == 2
                @test any(get_reports(result)) do r
                    isa(r, RuntimeDispatchReport) &&
                    last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == (@__LINE__) - 5 # report for the lambda function
                end
                @test any(get_reports(result)) do r
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
                @test length(get_reports(result)) == 1
                @test any(get_reports(result)) do r
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
            @test !isempty(get_reports(result))
        end

        let # if we use different `target_modules`, the reports from `println` should get filtered out
            @test_opt target_modules=(@__MODULE__,) M.compute(30)
        end
    end
end
