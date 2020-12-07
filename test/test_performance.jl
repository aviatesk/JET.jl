const JULIA_BIN = normpath(Sys.BINDIR, "julia")

"""
    stats = @freshexec ex

Runs `ex` in an external process and collects execution statistics from [`@timed`](@ref).
This is particularly useful when checking the performance of first-time analysis, where
  the native code cache and JET's global report cache have no effect for its performance.
"""
macro freshexec(ex)
    body = string(ex)
    prog = """
    using JET

    @profile_call identity(nothing) # warm-up for JET

    stats = @timed begin
        $(body)
        nothing # ensure `stats` can be parsed
    end
    println(repr(stats))
    """

    cmd = Cmd([JULIA_BIN, "-e", prog])
    io = IOBuffer()
    run(pipeline(cmd; stdout = io))
    return Meta.parse(String(take!(io)))
end

@testset "https://github.com/aviatesk/JET.jl/issues/71" begin
    stats = @freshexec @profile_call println(QuoteNode(nothing))
    @test stats.time ≤ 10
end
