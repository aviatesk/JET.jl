## behind the moar for performance ...

TypeProfiler employs Julia's type inference for bug reports.


## TODOs

- [ ] recursive type level frame creation
- [ ] profiling
  * what to report ?
  * how to handle `Any`, `Undefined`, etc
  * etc ...
- [ ] toplevel executions
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module
- [ ] `typeof′(::Type{T}) where {T} = T`: dangerous ?
- [ ] avoid recursive frame creation
- performance improvements
  * [ ] avoid dynamic dispatches
  * [ ] cache frames (signature based)
- [ ] get independent from `JuliaInterpreter`