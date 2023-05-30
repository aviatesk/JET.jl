# [General Configurations](@id general-configurations)

JET can be fine-tuned very flexibly.
Any entry point explained in [JET's default error analysis](@ref jetanalysis-entry) and [the optimization analysis](@ref optanalysis-entry)
can accept any of the configuration parameters described below as keyword arguments (or optional parameters for the interactive macros).
For example, you can analyze the call of `sum("julia")` with the [`fullpath`](@ref print-config) configuration enabled as like:
```julia
@report_call fullpath=true sum("julia")
```
or equivalently:
```julia
report_call(sum, (String,); fullpath=true)
```
Similarly you can analyze a top-level script `path/to/file.jl` with specifying [`target_defined_modules` configuration](@ref toplevel-config) as follows:
```julia
report_file("path/to/file.jl";
            target_defined_modules = true)
```

!!! note
    Please ignore the names of documented objects appearing below, like "[`JET.configured_reports`](@ref)".
    They are just remnants of documentation internals, and you will never directly interact with them.


## [Configurations for Analysis Result](@id result-config)

```@docs
JET.configured_reports
```

## [Configurations for Top-level Analysis](@id toplevel-config)

```@docs
JET.ToplevelConfig
```

## [Configurations for Abstract Interpretation](@id abstractinterpret-config)

```@docs
JET.JETInferenceParams
Core.Compiler.InferenceParams
Core.Compiler.OptimizationParams
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

## [Configuration File](@id config-file)

```@docs
JET.parse_config_file
```
