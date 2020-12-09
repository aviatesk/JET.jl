using JET: @benchmark_call, print_reports

if get(ENV, "CI", nothing) == "true"
    @info "benchmark platform"
    versioninfo(stdout)
end

@info "benchmark first time performance"
interp, frame = @benchmark_call identity(1)

@info "benchmark easy error reporting"
interp, frame = @benchmark_call sum("julia")

@info "benchmark analysis for cached frame"
interp, frame = @benchmark_call sum("julia")

@info "benchmark a bit complex call"
interp, frame = @benchmark_call rand(Bool)
