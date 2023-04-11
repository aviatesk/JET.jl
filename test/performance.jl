module performance

using Test, JET

# fresh execution/benchmark tools
include(normpath(@__DIR__, "..", "benchmark", "JETBenchmarkUtils.jl"))
using .JETBenchmarkUtils

@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    let ok = false
        for _ = 1:3 # try multiple times
            stats = @timed @report_call report_pass=FreshPass(JET.BasicPass()) println(QuoteNode(nothing))
            allow = 10
            get(ENV, "CI", nothing) == "true" && (allow += 5) # the CI runner might be slow
            JET.JET_DEV_MODE && (allow += 5) # add extra nudge for assertion-related overhead
            if stats.time ≤ allow
                ok = true
                break
            else
                @warn "bad performance detected: @report_call report_pass=FreshPass(JET.BasicPass()) println(QuoteNode(nothing))" stats.time
            end
        end
        @test ok
    end
    let ok = false
        for _ = 1:3 # try multiple times
            stats = @timed @report_call report_pass=FreshPass(JET.SoundPass()) println(QuoteNode(nothing))
            allow = 10
            get(ENV, "CI", nothing) == "true" && (allow += 5) # the CI runner might be slow
            JET.JET_DEV_MODE && (allow += 5) # add extra nudge for assertion-related overhead
            if stats.time ≤ allow
                ok = true
                break
            else
                @warn "bad performance detected: @report_call report_pass=FreshPass(JET.SoundPass()) println(QuoteNode(nothing))" stats.time
            end
        end
        @test ok
    end
end

end
