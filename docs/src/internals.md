# Internals of JET.jl

## [Abstract interpretation](@id abstractinterpret)

In order to perform type-level program analysis, JET.jl uses
`Compiler.AbstractInterpreter` interface, and customizes its abstract
interpretation by overloading a subset of `Compiler` functions, that are
originally developed for Julia compiler's type inference and optimizations that
aim at generating efficient native code for CPU execution.

[`JET.AbstractAnalyzer`](@ref) overloads a subset of `Compiler` methods to
implement JET's core functionality, including interprocedural propagation of
error reports and caching of analysis results. Each plugin analyzer, such as
[`JET.JETAnalyzer`](@ref), overloads additional `Compiler` methods to
implement its own analysis on top of the `AbstractAnalyzer` infrastructure.

Most of these overloads use
[`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke)
to call the corresponding methods for `AbstractInterpreter`. The actual
`AbstractAnalyzer` instance is still passed as the interpreter argument, so
calls made from within those original methods can dispatch back to
analyzer-specific overloads.

### How `AbstractAnalyzer` manages caches

```@docs
JET.AnalysisResult
JET.CachedAnalysisResult
JET.AnalysisToken
```

## [Top-level analysis](@id toplevel)

```@docs
JET.virtual_process
JET.VirtualProcessResult
JET.virtualize_module_context
JET.ConcreteInterpreter
JET.partially_interpret!
```

## [Analysis result](@id analysis-result)

```@docs
JET.JETToplevelResult
JET.JETCallResult
```

### [Splitting and filtering reports](@id optanalysis-splitting)

Both `JETToplevelResult` and `JETCallResult` can be split into individual
failures for integration with tools like Cthulhu:

```@docs
JET.get_reports
JET.reportkey
```

## Error report interface

```@docs
JET.VirtualFrame
JET.VirtualStackTrace
JET.Signature
JET.InferenceErrorReport
JET.ToplevelErrorReport
```
