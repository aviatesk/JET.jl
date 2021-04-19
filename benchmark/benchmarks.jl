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

    filename = string(__source__.file)
    runner_code = :(while true
        s = readuntil(stdin, "JET_BENCHMARK_INPUT_EOL")
        try
            include_string(Main, s, $filename)
        catch err
            showerror(stderr, err, stacktrace(catch_backtrace()))
        finally
            println(stdout, "JET_BENCHMARK_OUTPUT_EOL")
        end
    end) |> string

    # we need to flatten block expression into a toplevel expression to correctly handle
    # e.g. macro expansions
    setup_exs = isexpr(setup_ex, :block) ? setup_ex.args : [setup_ex]
    setup_code = join(string.(setup_exs), '\n')
    exs = isexpr(ex, :block) ? ex.args : [ex]
    benchmark_code = join(string.(exs), '\n')

    return quote
        @benchmarkable begin
            write(stdin, $benchmark_code, "JET_BENCHMARK_INPUT_EOL")
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

            cmd = String[normpath(Sys.BINDIR, "julia"),
                         "--project=@.",
                         "-e",
                         $runner_code]
            pipe = pipeline(Cmd(cmd); stdin, stdout, stderr)
            proc = run(pipe; wait = false)

            write(stdin, $setup_code, "JET_BENCHMARK_INPUT_EOL")
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

BenchmarkTools.DEFAULT_PARAMETERS.seconds = 60

const SUITE = BenchmarkGroup()

SUITE["first time"] = @jetbenchmarkable (@analyze_call identity(nothing)) setup = (using JET)
SUITE["easy"] = @jetbenchmarkable (@analyze_call sum("julia")) setup = begin
    using JET
    @analyze_call identity(nothing)
end
SUITE["cached easy"] = @jetbenchmarkable (@analyze_call sum("julia")) setup = begin
    using JET
    @analyze_call sum("julia")
end
SUITE["a bit complex"] = @jetbenchmarkable (@analyze_call rand(Bool)) setup = begin
    using JET
    @analyze_call identity(nothing)
end
SUITE["invalidation"] = @jetbenchmarkable (@analyze_call println(QuoteNode(nothing))) setup = begin
    using JET
    @analyze_call println(QuoteNode(nothing))
    @eval Base begin
        function show_sym(io::IO, sym::Symbol; allow_macroname=false)
            if is_valid_identifier(sym)
                print(io, sym)
            elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
                print(io, '@')
                show_sym(io, sym_str[2:end]) # NOTE: `sym_str[2:end]` here is errorneous
            else
                print(io, "var", repr(string(sym)))
            end
        end
    end
end
SUITE["self profiling"] = @jetbenchmarkable(
    analyze_call(JET.virtual_process, (AbstractString,
                                       AbstractString,
                                       JET.JETInterpreter,
                                       JET.ToplevelConfig,
                                       )),
    setup = begin
        using JET
        @analyze_call identity(nothing)
    end)
SUITE["toplevel"] = @jetbenchmarkable(
    (@analyze_toplevel begin
        foo(args...) = sum(args)
        foo(rand(Char, 1000000000)...)
    end),
    setup = begin
        include("test/interactive_utils.jl")
        @analyze_toplevel begin
            const myidentity = identity
            myidentity(nothing)
        end
    end)
SUITE["toplevel first time"] = @jetbenchmarkable(
    (@analyze_toplevel begin
        foo(args...) = sum(args)
        foo(rand(Char, 1000000000)...)
    end),
    setup = include("test/interactive_utils.jl"))
