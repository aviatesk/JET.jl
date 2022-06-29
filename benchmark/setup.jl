include("JETBenchmarkUtils.jl")

using JET, BenchmarkTools, .JETBenchmarkUtils

include("../test/interactive_utils.jl")

@info "JET setup information:" JET.JET_DEV_MODE
