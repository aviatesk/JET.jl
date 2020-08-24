profile_and_watch_file(args...; kwargs...) = profile_and_watch_file(stdout, args...; kwargs...)
function profile_and_watch_file(io::IO,
                                filename::AbstractString,
                                args...;
                                profiling_logger::Union{Nothing,IO} = io, # enable logger by default for watch mode
                                kwargs...)
    while true
        do_prehooks(io,
                    filename,
                    args...;
                    profiling_logger,
                    kwargs...)

        included_files, _ = try
            println(io)
            profile_file(io, filename, args...;
                         profiling_logger,
                         kwargs...)
        catch err
            @error "internal error occured:"
            showerror(stderr, err, catch_backtrace())
            [filename], true
        end

        # TODO: is there a way to abort tasks that are defeated in the race ?
        watch_chn = Channel{FileWatching.FileEvent}()
        for file in included_files
            @async put!(watch_chn, watch_file(file))
        end
        take!(watch_chn) # wait until any of the scheduled tasks fulfilled

        if !isfile(filename)
            @info "$(filename) has been removed"
            return
        end

        do_posthooks(io,
                     filename,
                     args...;
                     profiling_logger,
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
