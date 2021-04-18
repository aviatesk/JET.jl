# [Configurations](@id JET configurations)

JET analysis can be flexibly fine-tuned.
Any entry point explained in [Usages](@ref) can accept any of the configuration parameter described below as keyword
arguments (or optional parameters for macros).
For example, if you want to analyze `some/awesome/code.jl` with turning on `strict_condition_check` configuration and
also logs inference process into `stdout`, your can do:
```julia
report_file("some/awesome/code.jl";
            strict_condition_check = true,
            inference_logger = stdout)
```

!!! note
    Please ignore the names of documented objects below, like "[`JET.ToplevelConfig`](@ref)".
    They are just remnants of documentation internals, and you will never directly interact with them.

## Configurations for Top-level Analysis

```@docs
JET.ToplevelConfig
```


## Configurations for Abstract Interpretation

```@docs
JET.JETAnalysisParams
JET.JETInferenceParams
```


## Print Configurations

```@docs
JET.PrintConfig
```


## Watch Configurations

```@docs
JET.WatchConfig
```


## Logging Configurations

```@docs
JET.JETLogger
```


## Configuration File

```@docs
JET.parse_config_file
```
