@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    stats = @benchmark_freshexec ntimes = 1 @report_call println(QuoteNode(nothing))
    @test stats.time ≤ (get(ENV, "CI", nothing) == "true" ? 20 : 10) # a CI runner might be slow
end
