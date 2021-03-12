# [Configurations](@id JET configurations)

JET analysis can be flexibly fine-tuned with the following configurations.
Any entry point explained in [Usages](@ref) can accept any of the configuration parameter described below as keyword
arguments (or optional parameters for macros).
For example, if you want to analyze `your_awesome_code.jl` with turning on `strict_condition_check` configuration and
also logs inference process into `stdout`, your can do:
```julia
report_file(your_awesome_code; strict_condition_check = true, inference_logger = stdout)
```

!!! note
    Please ignore the names of documented objects below, like `JETAnalysisParams`.
    They are just remnants of documentation internals, and you will never directly interact with them.


## Configurations for Abstract Interpretation

!!! warning
    Currently JET doesn't invalidate the previous analysis results cached with the different analysis configurations
    (https://github.com/aviatesk/JET.jl/issues/130).
    So you need to refresh Julia session to get updated analysis results with different inference configurations.

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
