profile_and_watch_file(args...; kwargs...) = profile_and_watch_file(stdout, args...; kwargs...)
function profile_and_watch_file(io::IO,
                                filename::AbstractString,
                                args...;
                                profiling_logger::Union{Nothing,IO} = io,
                                print_inference_sucess::Bool = true,
                                kwargs...)
    errored = true
    while true
        do_prehooks(errored,
                    io,
                    filename,
                    args...;
                    profiling_logger,
                    print_inference_sucess,
                    kwargs...)

        try
            println(io)
            errored = profile_file(io, filename, args...;
                                   profiling_logger,
                                   # no success message after profiling without error found
                                   # unless specified manually
                                   print_inference_sucess = print_inference_sucess && errored,
                                   kwargs...)
        catch err
            @error "internal error occured:"
            showerror(stderr, err, catch_backtrace())
        end

        t = watch_file(filename)
        if !isfile(filename)
            @info "$(filename) has been removed"
            return
        end

        do_posthooks(errored,
                     io,
                     filename,
                     args...;
                     profiling_logger,
                     print_inference_sucess,
                     kwargs...)
    end
end

# hooks
# -----

const PRE_HOOKS = Function[]
push_prehook!(f) = push!(PRE_HOOKS, f)
do_prehooks(args...; kwargs...) = foreach(f->f(args...; kwargs...), PRE_HOOKS)

const POST_HOOKS = Function[]
push_posthook!(f) = push!(POST_HOOKS, f)
do_posthooks(args...; kwargs...) = foreach(f->f(args...; kwargs...), POST_HOOKS)

function __init_revise__()
    @require Revise = "295af30f-e4ad-537b-8983-00126c2a3abe" begin
        using .Revise

        function __hook_revise__(args...; kwargs...)
            revise()
        end |> push_prehook!
    end
end |> push_inithook!
