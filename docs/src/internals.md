# Internals of JET.jl

## [Abstract interpretation](@id abstractinterpret)

In order to perform type-level program analysis, JET.jl uses the
`Compiler.AbstractInterpreter` interface and customizes its abstract
interpretation by overloading a subset of the `Compiler` functions, which were
originally developed for the Julia compiler's type inference and for
optimizations that aim at generating efficient native code for CPU execution.

[`JET.AbstractAnalyzer`](@ref) overloads a subset of `Compiler` methods to
implement JET's core functionality, including interprocedural propagation of
error reports and caching of analysis results. Each plugin analyzer, such as
[`JET.JETAnalyzer`](@ref), overloads additional `Compiler` methods to
implement its own analysis on top of the `AbstractAnalyzer` infrastructure.

Most of these overloads use
[`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke)
to call the corresponding methods for `AbstractInterpreter`. The actual
`AbstractAnalyzer` instance is still passed as the interpreter argument, so
calls made from within those original methods can dispatch back to
analyzer-specific overloads.

### How `AbstractAnalyzer` manages caches

```@docs
JET.AnalysisResult
JET.CachedAnalysisResult
JET.AnalysisToken
```

## [Top-level analysis](@id toplevel)

```@docs
JET.virtual_process
JET.VirtualProcessResult
JET.virtualize_module_context
JET.ConcreteInterpreter
JET.partially_interpret!
```

### Fail-fast top-level processing

Top-level processing stops entirely at the first fatal parsing, macro
expansion, lowering, concrete execution, dependency, or include failure. No
later statements, modules, or files are processed, and
`analyze_from_definitions` does not run after a fatal failure. The result is
shown under `Top-level analysis failed`, explaining that analysis did not
complete and asking the user to fix the error and rerun it.

User code can still catch ordinary exceptions during concrete execution, but
JET's timeout and missing-concretization abort signals cannot be caught by
user code. Internal invariant and extension contract violations propagate as
exceptions and abort the analysis routine; they are not diagnostic reports
or user-catchable interpreted errors. This fail-fast contract does not fix
broader `include` return-value or catch semantics.

!!! warning "Known limitation: cleanup on interruption"
    Forced interpreter unwinding skips active user `finally` blocks as well as
    `catch` handlers. This includes timeout and missing-concretization aborts,
    fatal failures while processing included files, and internal errors. Side
    effects of concrete execution are not rolled back: for example,
    `cd(...) do` can leave the working directory changed, and `lock(...) do`
    can leave a lock held after analysis exits. Run analysis in a separate
    Julia process when preserving the interactive process's state is
    important; do not rely on interpreted cleanup running after interruption.

### Nonfatal top-level warnings

`VirtualProcessResult.toplevel_warning_reports` has type
`Vector{ToplevelWarningReport}` and stores nonfatal JET warnings separately
from `toplevel_error_report`. These warnings do not stop processing.

- `ToplevelWarningReport` is the abstract type for nonfatal top-level warnings.
- `ParseWarningReport` represents a parser warning, not a fatal parse error.
- `UnsupportedFeatureReport` describes an unsupported feature, including
  `include(mapexpr, filename)`: JET analyzes the included file without
  applying `mapexpr` and records a warning.

JET-generated warning reports are not also emitted as `@warn` logs. Logging
from user code or dependencies, including `@warn`, is unchanged and is not
captured in `toplevel_warning_reports`.

## [Analysis result](@id analysis-result)

```@docs
JET.JETToplevelResult
JET.JETCallResult
```

### Migrating to the singular top-level error field

`VirtualProcessResult.toplevel_error_report` has type
`Union{Nothing,ToplevelErrorReport}`. It is `nothing` on success or the first
fatal top-level error on failure. For a `JETToplevelResult` named `result`,
access it through `result.res.toplevel_error_report`.

The legacy `.toplevel_error_reports` property emits `Base.depwarn` and returns
a fresh empty or one-element `Vector` on each access. It will be removed in a
future release. Migrate direct consumers as follows:

- Replace `isempty(res.toplevel_error_reports)` with
  `isnothing(res.toplevel_error_report)`.
- Use `res.toplevel_error_report` directly when it is not `nothing`, rather
  than indexing or iterating over the legacy vector.
- For a vector of reports from a `JETToplevelResult`, use
  `JET.get_reports(result)` instead. Mutating the legacy vector does not
  update the stored error.

### [Splitting and filtering reports](@id optanalysis-splitting)

Both `JETToplevelResult` and `JETCallResult` can be split into individual
reports for integration with tools like Cthulhu:

```@docs
JET.get_reports
JET.reportkey
```

`JET.get_reports(result::JETToplevelResult)` continues to return a vector.
On fatal top-level failure it contains only the single fatal error, not any
warnings or inference reports collected before the failure. On successful
top-level processing it contains warnings first, followed by filtered
inference reports. When warnings are present, this is a mixed report vector;
consumers must not assume every element is an `InferenceErrorReport`.

Module filtering (`target_modules` and `ignored_modules`) and `report_config`
filtering apply only to inference reports. Warnings are retained regardless
of these filters.

## Report interface

```@docs
JET.VirtualFrame
JET.VirtualStackTrace
JET.Signature
JET.InferenceErrorReport
JET.ToplevelErrorReport
JET.ToplevelWarningReport
JET.ParseWarningReport
JET.UnsupportedFeatureReport
```
