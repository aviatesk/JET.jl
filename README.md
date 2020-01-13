## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.


### TODOs

in order of priority:

- [x] show profiled results: TODO: more nicer printing
- [x] escape recursive calls
- [ ] be more sensible type treating: succeed to profile `print` call
- [ ] toplevel executions
  * handle untyped IRs
  * splitting expressions like JuliaIntepreter.jl does
- [ ] more reports
  * [ ] `UndefVarError`
    + report `:throw_undef_if_not` ? (lots of false positives as is)
    + special case `getfield`, `fieldtype`
  * [ ] method ambiguity error
  * more and more ...
- [ ] improve multiple applicable methods handling
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module
- [ ] support generated functions

### Ideas

- report performance pitfalls
- somehow profiles possible exceptions ?