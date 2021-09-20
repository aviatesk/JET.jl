# [Configurations](@id JET-configurations)

JET analysis can be flexibly fine-tuned.
Any entry point explained in [JET's default error analysis](@ref jetanalysis-entry) and [the optimization analysis](@ref optanalysis-entry)
can accept any of the configuration parameters described below as keyword arguments (or optional parameters for the interactive macros).
For example, if you want to analyze `path/to/file.jl` with specifying [`target_defined_modules` configuration](@ref toplevel-config)
and also logs inference process into `stdout`, you can do:
```julia
report_file("path/to/file.jl";
            target_defined_modules = true,
            inference_logger = stdout)
```

!!! note
    Please ignore the names of documented objects appearing below, like "[`JET._get_configured_reports`](@ref)".
    They are just remnants of documentation internals, and you will never directly interact with them.


## [Configurations for Analysis Result](@id result-config)

```@docs
JET._get_configured_reports
```


## [Configurations for Top-level Analysis](@id toplevel-config)

```@docs
JET.ToplevelConfig
```


## [Configurations for Abstract Interpretation](@id abstractinterpret-config)

```@docs
JET.JETInferenceParams
```


## [Print Configurations](@id print-config)

```@docs
JET.PrintConfig
```


## [Configurations for VSCode Integration](@id vscode-config)

```@docs
JET.VSCode.VSCodeConfig
```


## [Watch Configurations](@id watch-config)

```@docs
JET.WatchConfig
```


## [Logging Configurations](@id logging-config)

```@docs
JET.JETLogger
```


## [Configuration File](@id config-file)

```@docs
JET.parse_config_file
```
