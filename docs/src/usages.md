# [How to use JET.jl](@id Usages)

!!! note
    JET's analysis entry points follow the naming conventions below:
    - `report_xxx`: runs analysis, and then prints the collected error points
    - `analyze_xxx`: just runs analysis, and returns the final state of the analysis
    The `report_xxx` entries are for general users, while `analyze_xxx` is mainly for internal usages or debugging purposes.


## [Entry Points into Analysis](@id Analysis entry points)

JET can analyze your "top-level" code.
This means your can just give your Julia file or code to JET and get error reports.
[`report_and_watch_file`](@ref), [`report_file`](@ref) and [`report_text`](@ref) are the main entry points for that.

JET will analyze your code "half-statically" – JET will selectively interpret "top-level definitions" (like a function definition)
and try to simulate Julia's top-level code execution, while it tries to avoid executing any other parts of code like function calls,
but analyze them using [abstract interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation) (this is a part where JET "statically" analyzes your code).
If you're interested in how JET selects "top-level definitions", please see [`JET.virtual_process!`](@ref).

!!! warning
    Because JET will actually interpret "top-level definitions" in your code, it certainly _runs_ your code.
    So we should note that JET can cause some side effects from your code; for example JET will try to expand all the
    macros used in your code, and so the side effects involved with macro expansions will also happen in JET's analysis process.

```@docs
report_text
report_file
report_and_watch_file
```


## Testing, Interactive Usage

There are utilities for checking JET analysis in a running Julia session like REPL or such.

!!! warning
    They are supposed to be used for testing of JET or some quick check, and you're not expected to use JET in the same
    Julia session where you "seriously" run your code.
    This is because JET analysis itself will create code cache, which isn't necessarily same as the code that Julia's naitve
    compiler does. In particular, JET currently disables inlining for reasons and it can have a potent impact on the
    performance of your code in actual execution.

    JET in the future will offer a more "proper" UI to render analysis result to users, whose process isn't supposed to
    interact with actual user's runtime in any way (I'm thinking of IDE/CI integration or such).
    Then these utilities will only be used for testing of JET itself, and not be really user-facing.

```@docs
report_call
@report_call
analyze_call
@analyze_call
```
