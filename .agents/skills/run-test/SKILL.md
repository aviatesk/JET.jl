---
name: run-test
description: >-
  Use when choosing or running JET tests after code changes. Prefer the most
  specific relevant test, use TestRunner for focused iteration, and reserve the
  full suite for broad validation.
---

# Run JET tests

Use this skill when validating JET changes or deciding which test command to
run.

## Choose the narrowest useful test

If the user names a test file or testset, run it first. Otherwise, use
[`test/runtests.jl`](../../../test/runtests.jl) and nearby tests to identify the
smallest relevant target.

Most test files can run independently. For example:

```bash
julia --startup-file=no --project=test test/ui/test_print.jl
```

- `--startup-file=no` avoids loading unrelated startup utilities.
- `--project=test` selects JET's test environment and its dependencies.
- Run commands from the repository root so relative paths and the workspace
  resolve correctly.

JET tests are organized by component. Common locations include:

- `test/abstractinterpret/`
- `test/analyzers/`
- `test/toplevel/`
- `test/ui/`

There are also integration and miscellaneous tests directly under `test/`.
Follow `test/runtests.jl` rather than assuming every test has a root-level path.

## Focused iteration with TestRunner.jl

When `testrunner --help` succeeds and the target `@testset` is clear, use
[TestRunner.jl](https://github.com/aviatesk/TestRunner.jl) for faster iteration:

```bash
testrunner --project=test \
  test/toplevel/test_virtualprocess.jl \
  "syntax error reports"
```

TestRunner.jl is experimental. If it fails for a TestRunner-specific reason,
fall back to running the complete test file independently.

## Full-suite validation

Avoid the full suite unless:

- Changes affect multiple components or shared compiler infrastructure.
- The user explicitly requests it.
- No narrower test provides sufficient coverage.

Use the package test lifecycle when matching CI or performing final broad
validation:

```bash
julia --startup-file=no -e 'using Pkg; Pkg.test()'
```

JET's workspace also supports running `test/runtests.jl` directly:

```bash
julia --startup-file=no --project=test test/runtests.jl
```

## Environment failures

If a failure looks environment-related, first confirm that the command ran from
the repository root. Do not modify `Project.toml` or `test/Project.toml` to fix
the environment. Report the unresolved failure and ask the user for guidance.

## Reporting

In the final response, report the exact command that ran and whether it passed,
failed, or timed out. If tests were skipped, explain why.
