## behind the moar for performance ...

TypeProfiler employs Julia's type inference for bug reports.


### TODOs

in order of priority:

- [x] show profiled results: TODO: more nicer printing
- [ ] escape recursive calls
- [ ] toplevel executions
  * handle untyped IRs
  * splitting expressions like JuliaIntepreter.jl does
- [ ] more reports
  * [ ] `UndefVarError`
  * [ ] method ambiguity error
  * more and more ...
- [ ] improve multiple applicable methods handling
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module
- [ ] support generated functions

### Ideas

- report performance pitfalls
- somehow profiles possible exceptions ?