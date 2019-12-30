## behind the moar for performance ...

TypeProfiler employs Julia's type inference for bug reports.


## TODOs

- [ ] recursive type level frame creation / method call
- [ ] profiling
  * what to report ?
  * how to handle `Any`, `Undefined`, etc
  * etc ...
- [ ] toplevel executions
- [ ] be more careful of type handlings
  * `typeof′(::Type{T}) where {T} = T`: too dangerous
- performance improvements
  * [ ] avoid dynamic dispatches
- [ ] get independent from `JuliaInterpreter` ?