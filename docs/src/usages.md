# [How to use JET.jl](@id Usages)

!!! note
    JET's analysis entry points follow the naming conventions below:
    - `analyze_xxx`: just run analysis, return the final state of analysis
    - `report_xxx`: run analysis, and then renders collected error points
    The `report_xxx` entries are for general users, while `analyze_xxx` is mainly for internal usages or debugging purposes.


## [Toplevel Entry Points](@id Toplevel)

JET can analyze your toplevel code.
This means your can just give your Julia file or code to JET and get error reports.
[`report_and_watch_file`](@ref), [`report_file`](@ref) and [`report_text`](@ref) are the main entry points for that.

JET will analyze your code "half-statically" – JET will selectively interpret "toplevel definitions" (like a function definition)
and try to simulate Julia's toplevel code execution, while it won't execute any other parts of code like function calls,
but analyze them using abstract interpretation (this is a part where JET "statically" analyzes your code).
If you're interested in how JET selects "toplevel definitions", please see [`JET.virtual_process!`](@ref).

!!! warning
    Because JET will actually interpret "toplevel definitions" in your code, it certainly _runs_ your code.
    So we should note that JET can involve some side effect; for example JET will try to expand all the macros within your
    code, and so side effects involved with the macro expansions will also happen in JET analysis.

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
    This is because JET analysis itself will produce code, which isn't necessarily same as the code that Julia's naitve
    compiler produces. In particular, JET currently disables inlining for reasons and it can have a potent impact on the
    performance of your code in actual execution.

    JET in the future will offer a "proper" UI to render JET analysis result to users, whose process isn't supposed to
    interact with actual user's runtime in any way (I'm thinking of IDE/CI integration or such).
    Then these utilities will only be used for JET's testing.

```@docs
report_call
@report_call
analyze_call
@analyze_call
```
