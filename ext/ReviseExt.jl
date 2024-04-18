module ReviseExt

using Revise
using JET: JET, WatchConfig, InsufficientWatches
import JET: watch_file_with_func

function JET.watch_file_with_func(func, args...; jetconfigs...)
    local included_files::Set{String}

    config = JET.WatchConfig(; jetconfigs...)

    included_files = let res = func(args...; jetconfigs...)
        display(res)
        res.res.included_files
    end

    interrupted = false
    while !interrupted
        try
            Revise.entr(collect(included_files), config.revise_modules;
                        postpone = true, all = config.revise_all) do
                next_included_files = let res = func(args...; jetconfigs...)
                    display(res)
                    res.res.included_files
                end
                if any(∉(included_files), next_included_files)
                    # refresh watch files
                    throw(InsufficientWatches(next_included_files))
                end
                return nothing
            end
            interrupted = true # `InterruptException` was gracefully handled within `entr`, shutdown watch mode
        catch err
            # handle "expected" errors, keep running

            if isa(err, InsufficientWatches)
                included_files = err.included_files
                continue
            elseif (isa(err, LoadError) ||
                    (isa(err, ErrorException) && startswith(err.msg, "lowering returned an error")) ||
                    isa(err, Revise.ReviseEvalException))
                continue

            # async errors
            elseif isa(err, CompositeException)
                errs = err.exceptions
                i = findfirst(@nospecialize(e)->isa(e, TaskFailedException), errs)
                if !isnothing(i)
                    tfe = errs[i]::TaskFailedException
                    let res = tfe.task.result
                        if isa(res, InsufficientWatches)
                            included_files = res.included_files
                            continue
                        elseif (isa(res, LoadError) ||
                                (isa(res, ErrorException) && startswith(res.msg, "lowering returned an error")) ||
                                isa(res, Revise.ReviseEvalException))
                            continue
                        end
                    end
                end
            end

            # fatal uncaught error happened in Revise.jl
            rethrow(err)
        end
    end
end


end