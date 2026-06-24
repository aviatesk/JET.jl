---
name: write-test
description: >-
  Use when adding or modifying JET tests. Covers component placement,
  independently runnable test modules, shared test utilities, `@testset`
  organization, and focused assertions for JET reports.
---

# Write JET tests

Use this skill when adding new JET tests or modifying existing test code.

## Choose the test location

Place tests beside the component they cover and include new files from
[`test/runtests.jl`](../../../test/runtests.jl). Existing component directories
include:

- `test/abstractinterpret/`
- `test/analyzers/`
- `test/toplevel/`
- `test/ui/`

Use a root-level file under `test/` for cross-component integration or
miscellaneous behavior when that matches the neighboring tests.

## Test file structure

New test files should normally define an independent module with a `test_`
prefix matching the file name. For example,
`test/toplevel/test_feature.jl` should follow this structure:

```julia
module test_feature

include("../setup.jl")

@testset "feature behavior" begin
    let source = "x = 1"
        result = report_text(source)
        @test isempty(result.res.toplevel_error_reports)
    end
    let source = "x ="
        result = report_text(source)
        @test !isempty(result.res.toplevel_error_reports)
    end
end

end # module test_feature
```

Include it from `test/runtests.jl` under the relevant component:

```julia
@testset "toplevel" begin
    @testset "feature.jl" include("toplevel/test_feature.jl")
end
```

Most existing tests follow this convention. Do not copy the known exception in
`test/abstractinterpret/test_inferenceerrorreport.jl`, which currently depends
on running in `Main`.

## Shared test utilities

Subdirectory tests that need JET's report helpers generally include
[`test/setup.jl`](../../../test/setup.jl) with `include("../setup.jl")`.
It loads `Test`, JET, and the helpers from `test/interactive_utils.jl`.

Do not include the shared setup automatically when direct `using JET, Test` or
narrow imports are sufficient.

## Organize test code

Use concise behavior-oriented `@testset` names. Group related cases into
coherent `@testset` blocks so `testrunner` can select them by name. Nest
`@testset` blocks when a larger behavior area has useful subgroups.

Keep `using`, `import`, helper functions, macros, and type definitions at module
scope unless local placement is specifically required.

When an `@testset` contains multiple test units, use a separate `let` block for
each unit to prevent variable collisions. A `let` block is unnecessary when the
`@testset` contains only one unit because there is no later unit with which its
variables can collide. A helper function or `do` block also provides local
scope.

Prefer assertions on report types, fields, counts, and semantic properties.
Reserve output-string assertions for formatting and UI behavior, such as tests
under `test/ui/`, unless the exact rendered message is the behavior under test.

## After writing tests

Use the [`run-test`](../run-test/SKILL.md) workflow and run the most specific
relevant test before broader validation.
