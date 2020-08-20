profile_and_watch_file(args...; kwargs...) = profile_and_watch_file(stdout, args...; kwargs...)
function profile_and_watch_file(io::IO, filename::AbstractString, args...;
                                log_profiling_timing::Bool = true,
                                print_inference_sucess::Bool = true,
                                kwargs...
                                )
    errored = true
    while true
        try
            errored = profile_file(io, filename, args...;
                                   log_profiling_timing,
                                   # no success message after profiling without error found
                                   # unless specified manually
                                   print_inference_sucess = print_inference_sucess && errored,
                                   kwargs...)
        catch err
            @error "internal error occured:" err
        end

        t = watch_file(filename)
        if t.renamed
            @info "$(filename) has been renamed"
            return
        end
        # keep profiling
    end
end
