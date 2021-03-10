# JET Internals

## Toplevel Analysis

```@docs
JET.virtual_process!
JET.partially_interpret!
JET.ConcreteInterpreter
```

## Abstract Interpretation

JET.jl overloads functions from Juila's [`Core.Compiler`](https://github.com/JuliaLang/julia/tree/master/base/compiler) module, which are intended for its native JIT type inference.

They're overloaded on `JETInterpreter` so that `typeinf(::JETInterpreter, ::InferenceState)` will do abstract interpretation tuned for JET.jl's type error analysis.
Most overloads are done by using [`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke), which allows us to call down to and reuse the original `NativeInterpreter`'s abstract interpretation methods while passing `JETInterpreter` for subsequent (maybe overloaded) callees (see [`JET.@invoke`](@ref) macro).

```@docs
JET.bail_out_toplevel_call
JET.bail_out_call
JET.add_call_backedges!
JET.const_prop_entry_heuristic
JET.analyze_task_parallel_code!
JET.is_from_same_frame
JET.AbstractGlobal
```

## Utilities

```@docs
JET.@invoke
JET.@invokelatest
JET.@withmixedhash
JET.@jetconfigurable
```
