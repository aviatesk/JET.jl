# Internals of JET.jl

## [Abstract Interpretation](@id abstractinterpret)

In order to perform type level program analysis, JET.jl uses
[`Core.Compiler.AbstractInterpreter` interface](https://github.com/JuliaLang/julia/blob/master/base/compiler/types.jl),
and customizes its abstract interpretation by overloading subset of `Core.Compiler` functions, that are originally
developed for Julia compiler's type inference and optimizations that aim at generating efficient native code for CPU execution.

[`JET.AbstractAnalyzer`](@ref) overloads a set of `Core.Compiler` functions to implement the "core" functionalities
of JET's analysis, including inter-procedural error report propagation and caching of analysis result.
And each plugin analyzer (e.g. [`JET.JETAnalyzer`](@ref)) will overload more `Core.Compiler` functions so that it can
perform its own program analysis on top of the core `AbstractAnalyzer` infrastructure.

Most overloads use the [`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke) reflection, which allows
`AbstractAnalyzer` to dispatch to the original `AbstractInterpreter`'s abstract interpretation methods while still
passing `AbstractAnalyzer` to the subsequent (maybe overloaded) callees (see [`JET.@invoke`](@ref) macro).

```@docs
JET.JETResult
JET.islineage
JET.bail_out_toplevel_call
JET.bail_out_call
JET.add_call_backedges!
JET.const_prop_entry_heuristic
JET.analyze_task_parallel_code!
```

### How `AbstractAnalyzer` manages caches

```@docs
JET.JET_CACHE
JET.JETCachedResult
JET.inlining_policy
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


## Error Report Interface

```@docs
JET.VirtualFrame
JET.VirtualStackTrace
JET.Signature
JET.InferenceErrorReport
JET.ToplevelErrorReport
```


## Utilities

```@docs
JET.@invoke
JET.@withmixedhash
JET.@jetconfigurable
```
