---
name: commit
description: >-
  Use when writing a commit message, and MUST invoke before creating any git
  commit. Provides commit message format and safety rules.
---

# Message guideline

## Title format

Use "component: Brief summary" format and imperative mood for the commit
title.

Examples:

- "toplevel: Support versioned module expressions"
- "optanalyzer: Analyze runtime dispatches on `IRCode`"
- "test: Integrate JET failures with Test recording"

## Body

Write a body by default, including for small, self-contained changes. Do not
treat a descriptive title as a reason to omit the body. When little explanation
is needed, briefly state the motivation and implementation.

Organize body paragraphs in this order and omit any paragraph that is not
relevant:

1. Explain the concrete problem, limitation, or goal motivating the change.
   For user-facing work, describe the resulting capability or behavior. For
   internal work, explain the engineering reason without inventing a
   user-visible impact. Include a small code example when it adds clarity.
2. Explain the approach used to implement the change.
3. Mention important caveats, follow-up work, performance notes, or test
   coverage when relevant.

Write body paragraphs as explanatory prose with explicit subjects:

- Prefer a concrete subject such as the affected component or newly introduced
  type.
- Use `This change` or `This commit` when describing the patch as a whole.
- Use `The implementation` when explaining the mechanism.
- Do not omit a subject merely to avoid `we` or `I`.

Use backticks for code elements such as function names, variables, and paths.

## Line length

Ensure the maximum line length never exceeds 72 characters.

## GitHub references

When referencing external GitHub PRs or issues, use proper GitHub interlinking
format: "owner/repo#123".

## Co-author trailer

If you wrote code yourself, include a co-author trailer at the end of the
commit message, for example:

`Co-Authored-By: GPT-5.6 Sol <noreply@openai.com>`

Adjust the model name as appropriate. When simply asked to write a commit
message without having written the code, do not add the trailer.

## Examples

The examples below are adapted from JET history. Co-author trailers are
omitted.

### Behavior change

Reference: `2d06d461fe1d9304972bdb4887af688d0edab4f0`

```
optanalyzer: Analyze runtime dispatches on `IRCode`

`OptAnalyzer` converted `OptimizationState` to `CodeInfo` before
Compiler cache transformation. This interfered with the Julia 1.13
inlining-cost lifecycle and required restoring the cost manually.

`OptAnalyzer` now analyzes the final `IRCode` after
`Compiler.optimize` instead. A lightweight state adapter provides report
signatures and locations. The implementation recomputes frame
eligibility instead of maintaining FIFO state.

This change preserves Julia 1.12 compatibility while allowing Compiler
to own cache transformation and inlining-cost computation.
```

### Bug fix

Reference: `9bc9057b1c257b8905e8c8b4d888c88cc89e9378`

```
toplevel: Support versioned module expressions

`virtual_process` assumed that a module body was always the third
argument of a `module` expression. Julia 1.14 syntax adds a syntax
version argument, shifting the module body and preventing definitions
inside the module from being analyzed correctly.

The implementation now takes the module body from the final expression
argument. This change supports both legacy and versioned module layouts.
```

# Safety guidelines

Before committing user-facing changes, use the
[`changelog`](../changelog/SKILL.md) skill to decide whether `CHANGELOG.md`
needs an entry.

See the ["Git operations" section in AGENTS.md][git-operations].

[git-operations]: ../../../AGENTS.md#git-operations
