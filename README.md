## behind the moar for performance ...

TypeProfiler employs Julia's type inference for bug reports.


### TODOs

in order of priority:

- [ ] show profiled results
- [ ] escape recursive calls
- [ ] toplevel executions
  * show handle untyped IRs
- [ ] improve multiple applicable methods handling
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module
- [ ] support generated functions
- somehow profiles possible exceptions ?