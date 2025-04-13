# Internals of JET.jl

## [Abstract Interpretation](@id abstractinterpret)

In order to perform type-level program analysis, JET.jl uses
[`Base.Compiler.AbstractInterpreter` interface](https://github.com/JuliaLang/julia/blob/master/base/compiler/types.jl),
and customizes its abstract interpretation by overloading a subset of `Base.Compiler` functions, that are originally
developed for Julia compiler's type inference and optimizations that aim at generating efficient native code for CPU execution.

[`JET.AbstractAnalyzer`](@ref) overloads a set of `Base.Compiler` functions to implement the "core" functionalities
of JET's analysis, including inter-procedural error report propagation and caching of the analysis result.
And each plugin analyzer (e.g. [`JET.JETAnalyzer`](@ref)) will overload more `Base.Compiler` functions so that it can
perform its own program analysis on top of the core `AbstractAnalyzer` infrastructure.

Most overloads use the [`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke) reflection, which allows
`AbstractAnalyzer` to dispatch to the original `AbstractInterpreter`'s abstract interpretation methods while still
passing `AbstractAnalyzer` to the subsequent (maybe overloaded) callees.

### How `AbstractAnalyzer` manages caches

```@docs
JET.AnalysisResult
JET.CachedAnalysisResult
JET.AnalysisToken
```

## [Top-level Analysis](@id toplevel)

```@docs
JET.virtual_process
JET.VirtualProcessResult
JET.virtualize_module_context
JET.ConcreteInterpreter
JET.partially_interpret!
```

### How top-level analysis is bridged to `AbstractAnalyzer`

```@docs
JET.AbstractGlobal
```

## [Analysis Result](@id analysis-result)

```@docs
JET.JETToplevelResult
JET.JETCallResult
```

### [Splitting and filtering reports](@id optanalysis-splitting)

Both `JETToplevelResult` and `JETCallResult` can be split into individual failures for integration with tools like Cthulhu:
```@docs
JET.get_reports
JET.reportkey
```

## Error Report Interface

```@docs
JET.VirtualFrame
JET.VirtualStackTrace
JET.Signature
JET.InferenceErrorReport
JET.ToplevelErrorReport
```
