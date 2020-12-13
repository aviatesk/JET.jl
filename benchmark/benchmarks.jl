"""
    JETBenchmarkTools

This module exports `@jetbenchmarkable`, which benchmarks arbitrary code execution in new
  Julia process with integrating [BenchmarkTools.jl](https://github.com/JuliaCI/BenchmarkTools.jl).

!!! warning
    Note that the current design of `@jetbenchmarkable` allows us to collect only execution
      time, and the other execution statistics like memory estimate are not available.
    `JETBenchmarkUtils` provides utilties for that.
"""
module JETBenchmarkTools

import Base.Meta: isexpr
import BenchmarkTools: @benchmarkable

"""
    @jetbenchmarkable ex [benchmark_params...]

Defines a benchmark for the fresh execution of `ex` with `BenchmarkTools.@benchmarkable`.
Any of benchmark parameters for `BenchmarkTools.@benchmarkable` except `evals` can be given
  (`evals` is fixed to `evals = 1` by design).
Each "evaluation" of `ex` is done in new Julia process in order to benchmark the performance
  of the "first-time analysis", where the native code cache and JET's global report cache
  have no effect for execution (of course, this is very time-consuming though ...).
"""
macro jetbenchmarkable(ex, benchmark_params...)
    benchmark_params = collect(benchmark_params)
    issetup(x) = isexpr(x, :(=)) && first(x.args) === :setup
    i = findfirst(issetup, benchmark_params)
    setup_ex = i === nothing ? nothing : last(popat!(benchmark_params, i).args)
    isevals(x) = isexpr(x, :(=)) && first(x.args) === :evals
    any(isevals, benchmark_params) && throw(ArgumentError("@jetbenchmarkable doesn't accept `evals` option"))

    # we need to flatten block expression into a toplevel expression to correctly handle
    # e.g. macro expansions
    setup_exs = isexpr(setup_ex, :block) ? setup_ex.args : [setup_ex]
    setup_script = join(string.(setup_exs), '\n')
    exs = isexpr(ex, :block) ? ex.args : [ex]
    script = join(string.(exs), '\n')

    return quote
        @benchmarkable begin
            write(stdin, $(script), "JET_BENCHMARK_INPUT_EOL")
            readuntil(stdout, "JET_BENCHMARK_OUTPUT_EOL")

            err = String(take!(stderr))
            if !isempty(err)
                kill(proc)
                println(err)
                throw(ErrorException("error happened while jetbenchmarkable"))
            end
        end setup = begin
            stdin  = Base.BufferStream()
            stdout = Base.BufferStream()
            stderr = IOBuffer()

            pipe = pipeline(`$(normpath(Sys.BINDIR, "julia")) --project=@. -e '
                while true
                    s = readuntil(stdin, "JET_BENCHMARK_INPUT_EOL")
                    try
                        include_string(Main, s, "benchmarks.jl")
                    catch err
                        showerror(stderr, err, stacktrace(catch_backtrace()))
                    finally
                        println(stdout, "JET_BENCHMARK_OUTPUT_EOL")
                    end
                end
            '`; stdin, stdout, stderr)
            proc = run(pipe; wait = false)

            write(stdin, $(setup_script), "JET_BENCHMARK_INPUT_EOL")
            readuntil(stdout, "JET_BENCHMARK_OUTPUT_EOL")

            err = String(take!(stderr))
            if !isempty(err)
                kill(proc)
                println(err)
                throw(ErrorException("error happened while jetbenchmarkable setup"))
            end
        end teardown = begin
            kill(proc)
        end evals = 1 $(benchmark_params...)
    end
end

export @jetbenchmarkable

end


# benchmark body
# ==============

using BenchmarkTools, .JETBenchmarkTools

# BenchmarkTools.DEFAULT_PARAMETERS.seconds = 60

const SUITE = BenchmarkGroup()

# SUITE["identity(nothing) (first time)"] = @jetbenchmarkable (@profile_call identity(nothing)) setup = (using JET)
# SUITE["sum(\"julia\")"] = @jetbenchmarkable (@profile_call sum("julia")) setup = begin
#     using JET
#     @profile_call identity(nothing)
# end
# SUITE["sum(\"julia\") (cached)"] = @jetbenchmarkable (@profile_call sum("julia")) setup = begin
#     using JET
#     @profile_call sum("julia")
# end
# SUITE["rand(Bool)"] = @jetbenchmarkable (@profile_call rand(Bool)) setup = begin
#     using JET
#     @profile_call identity(nothing)
# end
SUITE["test"] = @jetbenchmarkable identity(:nothing) seconds = 10
