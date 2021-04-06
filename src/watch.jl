"""
Configurations for "watch" mode.
The configurations will only be active when used with [`report_and_watch_file`](@ref).

---
- `revise_all::Bool = true` \\
  Redirected to [`Revise.entr`](https://timholy.github.io/Revise.jl/stable/user_reference/#Revise.entr)'s `all` keyword argument.
  When set to `true`, JET will retrigger analysis as soon as code updates are detected in
    any module tracked by Revise.
  Currently when encountering `import/using` statements, JET won't perform analysis, but
    rather will just load the modules as usual execution (this also means Revise will track
    those modules).
  So if you're editing both files analyzed by JET and modules that are used within the files,
    this configuration should be enabled.
---
- `revise_modules = nothing` \\
  Redirected to [`Revise.entr`](https://timholy.github.io/Revise.jl/stable/user_reference/#Revise.entr)'s `modules` positional argument.
  If a iterator of `Module` is given, JET will retrigger analysis whenever code in `modules` updates.

  !!! tip
      This configuration is useful when your're also editing files that are not tracked by Revise,
      e.g. editing functions defined in `Base`:
      ```julia
      # re-performe analysis when you make a change to `Base`
      julia> report_and_watch_file(yourfile; revise_modules = [Base])
      ```
"""
struct WatchConfig
    # Revise configurations
    revise_all::Bool
    revise_modules
    @jetconfigurable WatchConfig(; revise_all::Bool = true,
                                   revise_modules   = nothing,
                                   ) =
        return new(revise_all,
                   revise_modules,
                   )
end

"""
    report_and_watch_file([io::IO = stdout],
                          filename::AbstractString,
                          mod::Module = Main;
                          toplevel_logger::Union{Nothing,IO} = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $INFO_LOGGER_LEVEL),
                          jetconfigs...)

Watches `filename` and keeps re-triggering analysis with [`report_file`](@ref) on code update.
JET will try to analyze all the `include`d files reachable from `filename`, and it will
  re-trigger analysis if there is code update detected in any of the `include`d files.

This function internally uses [Revise.jl](https://timholy.github.io/Revise.jl/stable/) to
  track code updates. Revise also offers possibilities to track changes in files that are
  not directly analyzed by JET, or even changes in `Base` files. See [Watch Configurations](@ref)
  for more details.

Like [`report_file`](@ref), this function will look for `$CONFIG_FILE_NAME` configuration file in the directory of `filename`,
  and search _up_ the file tree until any `$CONFIG_FILE_NAME` is (or isn't) found.
When found, the configurations specified in the file will overwrite the given `jetconfigs`.
See [Configuration File](@ref) for more details.

!!! note
    Like [`report_file`](@ref), this function will enable the toplevel logger by default
      with the default logging level (see [Logging Configurations](@ref) for more details).
"""
function report_and_watch_file(args...; kwargs...)
    if @isdefined(Revise)
        _report_and_watch_file(args...; kwargs...)
    else
        init_revise!()
        @invokelatest _report_and_watch_file(args...; kwargs...)
    end
end

# HACK to avoid Revise loading overhead when just using `@report_call`, etc.
function init_revise!()
    @eval (@__MODULE__) using Revise
end

_report_and_watch_file(args...; kwargs...) = _report_and_watch_file(stdout::IO, args...; kwargs...)
function _report_and_watch_file(io::IO,
                                filename::AbstractString,
                                args...;
                                # enable info top-level logger by default for watch mode
                                toplevel_logger::Union{Nothing,IO} = IOContext(io, LOGGER_LEVEL_KEY => INFO_LOGGER_LEVEL),
                                jetconfigs...)
    config = WatchConfig(; jetconfigs...)

    included_files, _ = report_file(io, filename, args...;
                                    toplevel_logger,
                                    jetconfigs...)

    interrupted = false
    while !interrupted
        try
            Revise.entr(collect(included_files), config.revise_modules; all = config.revise_all) do
                println(io)
                included_files′, _ = report_file(io, filename, args...;
                                                 toplevel_logger,
                                                 jetconfigs...)
                if any(∉(included_files), included_files′)
                    # refresh watch files
                    throw(InsufficientWatches(included_files′))
                end
                return nothing
            end
            interrupted = true # `InterruptException` was gracefully handled within `entr`, shutdown watch mode
        catch err
            # handle "expected" errors, keep running

            if isa(err, InsufficientWatches)
                included_files = err.included_files
                continue
            elseif isa(err, LoadError) ||
                   (isa(err, ErrorException) && startswith(err.msg, "lowering returned an error")) ||
                   isa(err, Revise.ReviseEvalException)
                continue

            # async errors
            elseif isa(err, CompositeException)
                errs = err.exceptions
                i = findfirst(e->isa(e, TaskFailedException), errs)
                if !isnothing(i)
                    tfe = errs[i]::TaskFailedException
                    res = tfe.task.result
                    if isa(res, InsufficientWatches)
                        included_files = res.included_files
                        continue
                    elseif isa(res, LoadError) ||
                           (isa(res, ErrorException) && startswith(res.msg, "lowering returned an error")) ||
                           isa(res, Revise.ReviseEvalException)
                        continue
                    end
                end
            end

            # fatal uncaught error happened in Revise.jl
            rethrow(err)
        end
    end
end

struct InsufficientWatches <: Exception
    included_files::Set{String}
end
