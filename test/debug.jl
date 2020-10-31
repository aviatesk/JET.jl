using JET
using JET: typeof′, profile_call, print_reports
using Base.Meta: isexpr

macro benchmark(ex, kwargs...)
    @assert isexpr(ex, :call) "function call expression should be given"
    f = first(ex.args)
    args = ex.args[2:end]

    return quote let
        println(stdout)
        argtypes = $(typeof′).(($(map(esc, args)...),))
        @info "profiling for $($(QuoteNode(ex))) ..."
        @time interp, frame = $(profile_call)($(esc(f)), argtypes; $(map(esc, kwargs)...))
        @info "$(length(interp.reports)) errors reported for $($(QuoteNode(ex)))"
        interp, frame
    end end
end

interp, frame = @benchmark rand(Bool)
print_reports(stdout, interp.reports; annotate_types = true)
