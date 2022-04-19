# fresh execution/benchmark tools
include(normpath(@__DIR__, "..", "benchmark", "JETBenchmarkUtils.jl"))
using .JETBenchmarkUtils

@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    let ok = false
        for _ = 1:3 # try multiple times
            stats = @timed @report_call report_pass=FreshPass(JET.BasicPass()) println(QuoteNode(nothing))
            if stats.time ≤ (get(ENV, "CI", nothing) == "true" ? 20 : 10) # a CI runner might be slow
                ok = true
                break
            else
                @warn "bad performance detected: @report_call report_pass=FreshPass(JET.BasicPass()) println(QuoteNode(nothing))"
            end
        end
        @test ok
    end
    let ok = false
        for _ = 1:3 # try multiple times
            stats = @timed @report_call report_pass=FreshPass(JET.SoundPass()) println(QuoteNode(nothing))
            if stats.time ≤ (get(ENV, "CI", nothing) == "true" ? 20 : 10) # a CI runner might be slow
                ok = true
                break
            else
                @warn "bad performance detected: @report_call report_pass=FreshPass(JET.SoundPass()) println(QuoteNode(nothing))"
            end
        end
        @test ok
    end
end
