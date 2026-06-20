# Formatting

## Code formatting

- When writing Julia code, use _4 whitespaces_ for indentation and try to keep
  the maximum line length under _92 characters_.
- AI agents must not run automated formatters unless explicitly requested by a
  human in the current conversation.
  This includes file-wide or project-wide formatting commands and
  editor-integrated formatting tools.
  When editing code, preserve the surrounding formatting and make only minimal
  local edits. If formatting seems necessary, ask before applying it.

## Markdown formatting

When writing Markdown text, use _2 whitespaces_ for indentation and try to
keep the maximum line length under _80 characters_.
- Exception: `CHANGELOG.md` is exempt from line length rules since it is
  used for GitHub release notes, where hard line breaks disrupt rendering.
- Additionally, prioritize simple text style and limit unnecessary decorations
  (e.g. `**`) to only truly necessary locations. This is a style that should
  generally be aimed for, but pay particular attention when writing Markdown.
- Headers should use sentence case (only the first word capitalized), not
  title case. For example:
  - Good: `## Conclusion and alternative approaches`
  - Bad: `## Conclusion And Alternative Approaches`

## Commit message formatting

When writing commit messages, follow the format "component: Brief summary" for
the title.

In the body, write paragraphs in this order:

1. Explain the concrete problem or user-visible limitation being fixed.
   If appropriate, include a small code example when it makes the issue
   clearer.
2. Explain the approach used to fix the problem.
3. Mention important caveats, follow-up work, performance notes, or test
   coverage when relevant.

Use backticks for code elements (function names, variables, file paths, etc.)
to improve readability.

Ensure that the maximum line length never exceeds 72 characters.
When referencing external GitHub PRs or issues, use proper GitHub interlinking
format (e.g., "owner/repo#123" for PRs/issues).
Finally, if you write code yourself, include a co-author trailer at the end
of the commit message, e.g.: `Co-Authored-By: GPT-5.5 <noreply@openai.com>`
(adjust the model name as appropriate). However, when simply asked to write
a commit message, there's no need to add that trailer.

# File names

For file names, use `-` (hyphen) as the word separator by default.
However, if the file name corresponds directly to Julia code (e.g., a module
name), use `_` (underscore) instead, since Julia identifiers cannot contain
hyphens (unless we use `var"..."`). For example, test files like
`test_virtualprocess.jl` define a module `module test_virtualprocess`,
so they use underscores.

# Coding rules

- When writing functions, use the most restrictive signature type practical so
  JET can catch unintended errors in itself. Loose signatures are fine while 
  prototyping, but committed code should specify expected argument types unless 
  generic behavior is intentional. When unsure, prefer the more restrictive 
  signature.

- For function calls with keyword arguments, use an explicit `;` for clarity.
  For example, code like this:
  ```julia
  ...
  Position(; line=i-1, character=m.match.offset-1)
  ...
  ```
  is preferred over:
  ```julia
  ...
  Position(line=i-1, character=m.match.offset-1)
  ...
  ```

- Avoid unnecessary logs:
  Don't clutter the language server log with excessive information.
  If you must use print debugging, generally use `@info`/`@warn` behind the
  `JET_DEV_MODE` flag, like this:
  ```julia
  if JET_DEV_MODE
      @info ...
  end
  ```

## Comments guideline

Default to no comments. Add comments only when they explain non-obvious
behavior, constraints, invariants, rationale, or genuine hacks. Do not restate
implementation flow.

Docstrings are fine for general utilities when they clarify behavior.

The same applies to tests: concise `@testset` descriptions and behavior-level
comments are fine when they clarify what is being tested. Explain test setup
only when it encodes a non-obvious constraint or hack.

# Running test code
Please make sure to test new code when you wrote.

If explicit test file or code is provided, prioritize running that.
Otherwise, you can run the entire test suite for the JET project by executing
`using Pkg; Pkg.test()` from the root directory of this repository.

For example, if you receive a prompt like this:
> Improve the printed message of top-level error reports.
> Use test/ui/test_print.jl for the test cases.

The command you should run is:
```bash
julia --startup-file=no -e 'using Test; @testset "test_print" include("test/ui/test_print.jl")'
```
Note that the usage of the `--startup-file=no` flag, which avoids loading
unnecessary startup utilities.

# Test code structure
Test code should be written in files that define independent module spaces with
a `test_` prefix.
Then include these files from [`test/runtests.jl`](./test/runtests.jl).
This ensures that these files can be run independently from the REPL.
For example, test code for the "completion" feature would be in a file like
this:
> test/test_virtualprocess.jl
```julia
module test_virtualprocess
using Test # Each module space needs to explicitly declare the code needed for execution
...
end # module test_virtualprocess
```
And `test/test_virtualprocess.jl` is included from `test/runtests.jl` like this:
> test/runtests.jl
```julia
@testset "JET.jl" begin
   ...
   @testset "virtualprocess" include("test_virtualprocess.jl")
   ...
end
```

In each test file, you are encouraged to use `@testset "testset name"` to
organize our tests cleanly. For code clarity, unless specifically necessary,
avoid using `using`, `import`, and `struct` definitions  inside `@testset`
blocks, and instead place them at the top level.

Also, you are encouraged to use `let`-blocks to ensure that names aren't
unintentionally reused between multiple test cases.
For example, here is what good test code looks like:
```julia
module test_virtualprocess

using Test # Each module space needs to explicitly declare the code needed for execution
using JET: some_func

function test_with()
    ...
end
function testcase_util(s::AbstractString)
    ...
end
function with_testcase(s::AbstractString)
    ...
end

@testset "some_func" begin
    let s = "..."
        ret = some_func(testcase_util(s))
        @test test_with(ret)
    end
    let s = "..."
        ret = some_func(testcase_util(s))
        @test test_with(ret)
    end

    # or `let` is unnecessary when testing with function scope
    with_testcase(s) do case
        ret = some_func(case)
        @test test_with(ret)
    end
end

end # module test_virtualprocess
```
Additionally, by using `@testset` as shown above, not only are tests hierarchized,
but through integration with [TestRunner.jl](https://github.com/aviatesk/TestRunner.jl),
you can also selectively execute specific `@testset`s, without executing the
entire test file or test suite.
If you're using this language server for development as well, you can run tests
from code lenses or code actions within test files. If you need to run them from
the command line, you can use commands like the following
(assuming the `testrunner` executable is installed):
```bash
testrunner --verbose test/test_virtualprocess "some_func"
```
Note that TestRunner.jl is still experimental.
The most reliable way to run tests is still to execute test files standalone.

# Environment-related issues
For AI agents: **NEVER MODIFY [Project.toml](./Project.toml) OR  [test/Project.toml](./test/Project.toml) BY YOURSELF**.
If you encounter errors that seem to be environment-related when running tests,
in most cases this is due to working directory issues, so first `cd` to the root directory of this project
and re-run the tests. Never attempt to fix environment-related issues yourself.
If you cannot resolve the problem, inform the human engineer and ask for instructions.

# About modifications to code you've written
If you, as an AI agent, add or modify code, and the user appears to have made
further manual changes to that code after your response, please respect those
modifications as much as possible.
For example, if the user has deleted a function you wrote, do not reintroduce
that function in subsequent code generation.
If you believe that changes made by the user are potentially problematic,
please clearly explain your concerns and ask the user for clarification.
