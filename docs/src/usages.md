# [How to Use JET.jl](@id usages)


## [Entry Points into Analysis](@id entry-points)

JET can analyze your "top-level" code.
This means you can just give your Julia file or code to JET and get error reports.
[`report_file`](@ref), [`report_and_watch_file`](@ref), [`report_package`](@ref) and [`report_text`](@ref) are the main entry points for that.

JET will analyze your code "half-statically" – JET will selectively interpret "top-level definitions" (like a function definition)
and try to simulate Julia's top-level code execution, while it tries to avoid executing any other parts of code like function calls
but analyzes them by [abstract interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation) instead (and this is a part where JET statically analyzes your code).
If you're interested in how JET selects "top-level definitions", please see [`JET.virtual_process`](@ref).

!!! warning
    Because JET will actually interpret "top-level definitions" in your code, it certainly _runs_ your code.
    So we should note that JET can cause some side effects from your code; for example JET will try to expand all the
    macros used in your code, and so the side effects involved with macro expansions will also happen in JET's analysis process.

```@docs
report_file
report_and_watch_file
report_package
report_text
```


## [Testing, Interactive Usage](@id interactive-entries)

There are utilities for checking JET analysis in a running Julia session like REPL or such.

```@docs
report_call
@report_call
```
