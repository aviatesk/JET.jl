## behind the moar for performance ...

TypeProfiler employs Julia's type inference for bug reports.


### TODOs

in order of priority:
- [ ] toplevel executions
- [ ] profiling
  * what to report ?
    + `Union{}` or no method found would be obvious errors
  * how to handle `Any`, `Undefined`, etc
  * etc ...
- [ ] multiple state merging
  * there can be multiple applicable methods
  * Maybe I need to rewrap most functionalities in `JuliaInterpreter`
- [ ] handle recursive type level frame creation
- [ ] get independent from `JuliaInterpreter`
- performance improvements
  * [ ] avoid dynamic dispatches
  * [ ] cache frames (signature based)
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module