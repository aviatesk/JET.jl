profile_and_watch_file(args...; kwargs...) = profile_and_watch_file(stdout, args...; kwargs...)
function profile_and_watch_file(io::IO, filename::AbstractString, args...;
                                profiling_logger::Union{Nothing,IO} = io,
                                print_inference_sucess::Bool = true,
                                kwargs...
                                )
    errored = true
    while true
        try
            errored = profile_file(io, filename, args...;
                                   profiling_logger,
                                   # no success message after profiling without error found
                                   # unless specified manually
                                   print_inference_sucess = print_inference_sucess && errored,
                                   kwargs...)
        catch err
            @error "internal error occured:" err
        end

        t = watch_file(filename)
        if !isfile(filename)
            @info "$(filename) has been removed"
            return
        end
        # keep profiling
    end
end
