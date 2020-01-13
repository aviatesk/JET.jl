## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.


### TODOs

in order of priority:

- [x] show profiled results: TODO: prettier printing
- [x] escape recursive calls
- [ ] more sensible type handling
  * succeed to profile
    + [ ] `print`
    + [ ] `sort`
- [ ] toplevel executions
  * handle untyped IRs
  * split expressions as JuliaIntepreter.jl does
- [ ] more reports
  * [ ] `UndefVarError`
    + report `:throw_undef_if_not` ? (includes lots of false positives as is)
    + special case `getfield`, `fieldtype`
  * [ ] method ambiguity error
  * more and more ...
- [ ] improve multiple applicable methods handling
- [ ] support generated functions
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module

### Ideas

- report performance pitfalls
- somehow profiles possible exceptions ?