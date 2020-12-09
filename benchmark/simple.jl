include("benchmark_utils.jl")

function simple_benchmark(ntimes = 5)
    ret = []
    function _simple_benchmark!(setup_ex, ex, desc)
        stats = @nbenchmark_freshexec ntimes = $(ntimes) $(setup_ex) $(ex)
        @info desc ntimes setup_ex ex stats
        push!(ret, stats)
    end

    let
        setup_ex = :(using JET)
        ex       = :(@profile_call identity(nothing))
        _simple_benchmark!(setup_ex, ex, "benchmark first time performance")
    end

    let
        setup_ex = quote
            using JET
            @profile_call identity(nothing)
        end
        ex       = :(@profile_call sum("julia"))
        _simple_benchmark!(setup_ex, ex, "benchmark easy error reporting")
    end

    let
        setup_ex = quote
            using JET
            @profile_call sum("julia")
        end
        ex       = :(@profile_call sum("julia"))
        _simple_benchmark!(setup_ex, ex, "benchmark analysis for cached frame")
    end

    let
        setup_ex = quote
            using JET
            @profile_call sum("julia")
        end
        ex       = :(@profile_call rand(Bool))
        _simple_benchmark!(setup_ex, ex, "benchmark a bit complex call")
    end

    return ret
end
