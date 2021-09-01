# PerformanceAnalyzer
# ===================

@testset "runtime dispatch" begin
    let M = Module()
        @eval M begin
            f(a) = a
            f(a::Number) = a
        end

        # `f(::Int)` a concrete call and just type stable and anything shouldn't be reported
        @test_nopitfall M.f(10)  # should be ok

        let # if the argument type isn't well typed, compiler can't determine which method to call,
            # and it will lead to runtime dispatch
            result = @eval M begin
                $report_pitfall((Vector{Any},)) do ary
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
            result = @eval M $report_pitfall((Vector{Any},)) do ary
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
    @test_nopitfall sin(10)
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
            $report_pitfall(foo, (Int, Int))
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

        let result = @report_pitfall M.abmult(42)
            @test any(get_reports(result)) do report
                return isa(report, CapturedVariableReport) &&
                       report.name === :r
            end
        end
        let result = @report_pitfall M.abmult2(42)
            @test any(get_reports(result)) do report
                return isa(report, CapturedVariableReport) &&
                       report.name === :r
            end
        end

        @test_nopitfall M.abmult3(42) # no captured variable for `abmult3` !
    end
end

@testset "module_filter" begin
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
        result = @report_pitfall M.compute(30)
        @test !isempty(get_reports(result))
    end

    let # if we use different `frame_filter`, a fresh analysis should run
        function module_filter(mod::Module)
            return function (x::JET.State)
                x.mod === mod
            end
        end
        @test_nopitfall frame_filter=module_filter(@__MODULE__) M.compute(30)
    end
end

@testset "skip_nonconcrete_calls" begin
    M = Module()
    F1_DEFINITION_LINE = @__LINE__
    @eval M begin
        callsin(@nospecialize a) = sin(a)
    end

    # by default, we'd ignore error reports from `f1` calls
    @eval M $test_nopitfall((Vector{Any},)) do ary
        callsin(ary[1]) # runtime dispatch !
    end

    let # when the `skip_nonconcrete_calls` configuration is turned off, we will get error reports
        # from those non-concrete call inside of `callsin`
        result = @eval M $report_pitfall((Vector{Any},); skip_nonconcrete_calls=false) do ary
            callsin(ary[1]) # runtime dispatch !
        end
        @test length(get_reports(result)) ≥ 1
        @test any(get_reports(result)) do r
            isa(r, RuntimeDispatchReport) &&
            last(r.vst).file === Symbol(@__FILE__) && last(r.vst).line == F1_DEFINITION_LINE + 2 # report for `sin(a::Any)`
        end
    end
end
