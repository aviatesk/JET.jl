include("JETBenchmarkUtils.jl")

using .JETBenchmarkUtils

function simple_benchmark(ntimes = 5; verbose = false)
    ret = []
    function _simple_benchmark!(setup_ex, ex, desc)
        stats = @benchmark_freshexec ntimes = $(ntimes) $(setup_ex) $(ex)
        msg = string("benchmarking: ", desc)
        if verbose
            @info msg ntimes setup_ex ex stats
        else
            @info msg
        end
        push!(ret, stats)
    end

    let
        setup_ex = :(using JET)
        ex       = :(@analyze_call identity(nothing))
        _simple_benchmark!(setup_ex, ex, "first-time analysis")
    end

    let
        setup_ex = quote
            using JET
            @analyze_call identity(nothing)
        end
        ex       = :(@analyze_call sum("julia"))
        _simple_benchmark!(setup_ex, ex, "simple analysis")
    end

    let
        setup_ex = quote
            using JET
            @analyze_call sum("julia")
        end
        ex       = :(@analyze_call sum("julia"))
        _simple_benchmark!(setup_ex, ex, "cached simple analysis")
    end

    let
        setup_ex = quote
            using JET
            @analyze_call identity(nothing)
        end
        ex       = :(@analyze_call rand(Bool))
        _simple_benchmark!(setup_ex, ex, "a bit complex analysis")
    end

    return ret
end
