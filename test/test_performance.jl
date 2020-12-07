# setup
# =====

include(normpath(@__DIR__, "..", "benchmark", "utils.jl"))

# test body
# =========

@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    stats = @freshexec @profile_call println(QuoteNode(nothing))
    @test stats.time ≤ 10
end
