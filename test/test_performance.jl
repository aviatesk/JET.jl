@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    stats = @benchmark_freshexec ntimes = 1 @profile_call println(QuoteNode(nothing))
    @test stats.time ≤ 10
end
