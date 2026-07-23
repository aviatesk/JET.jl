# [General configurations](@id general-configurations)

JET offers extensive customization options through its configuration system.
All entry points covered in
[JET's default error analysis](@ref jetanalysis-entry) and
[the optimization analysis](@ref optanalysis-entry) accept the configuration
parameters outlined below as keyword arguments (or optional parameters for
interactive macros). For instance, you can analyze the call `sum("julia")` with
the [`fullpath`](@ref print-config) configuration enabled:

```julia
@report_call fullpath=true sum("julia")
```

Or equivalently:

```julia
report_call(sum, (String,); fullpath=true)
```

You can also analyze a top-level script `path/to/file.jl` while specifying the
[`target_modules` configuration](@ref result-config):

```julia
report_file("path/to/file.jl";
            target_modules = (Main,))
```

!!! note
    The documented objects listed below (such as
    "`JET.configured_reports`") represent internal configuration structures.
    While you won't interact with these objects directly, their documentation
    describes the available configuration options that you can pass as keyword
    arguments to JET's analysis functions.

## [Configurations for analysis result](@id result-config)

```@docs
JET.configured_reports
```

## [Configurations for top-level analysis](@id toplevel-config)

```@docs
JET.ToplevelConfig
```

## [Print configurations](@id print-config)

```@docs
JET.PrintConfig
```

## [Configurations for VSCode integration](@id vscode-config)

```@docs
JET.VSCode.VSCodeConfig
```
