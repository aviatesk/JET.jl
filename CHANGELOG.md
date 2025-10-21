# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- links start -->
[Unreleased]: https://github.com/aviatesk/JET.jl/compare/v0.10.12...HEAD
[0.10.12]: https://github.com/aviatesk/JET.jl/compare/v0.10.11...v0.10.12
[0.10.11]: https://github.com/aviatesk/JET.jl/compare/v0.10.10...v0.10.11
[0.10.10]: https://github.com/aviatesk/JET.jl/compare/v0.10.9...v0.10.10
[0.10.9]: https://github.com/aviatesk/JET.jl/compare/v0.10.8...v0.10.9
[0.10.8]: https://github.com/aviatesk/JET.jl/compare/v0.10.7...v0.10.8
[0.10.7]: https://github.com/aviatesk/JET.jl/compare/v0.10.6...v0.10.7
[0.10.6]: https://github.com/aviatesk/JET.jl/compare/v0.10.5...v0.10.6
[0.10.5]: https://github.com/aviatesk/JET.jl/compare/v0.10.4...v0.10.5
[0.10.4]: https://github.com/aviatesk/JET.jl/compare/v0.10.3...v0.10.4
[0.10.3]: https://github.com/aviatesk/JET.jl/compare/v0.10.2...v0.10.3
[0.10.2]: https://github.com/aviatesk/JET.jl/compare/v0.10.1...v0.10.2
[0.10.1]: https://github.com/aviatesk/JET.jl/compare/v0.10.0...v0.10.1
[0.10.0]: https://github.com/aviatesk/JET.jl/compare/v0.9.18...v0.10.0
[0.9.18]: https://github.com/aviatesk/JET.jl/compare/v0.9.17...v0.9.18
[0.9.17]: https://github.com/aviatesk/JET.jl/compare/v0.9.16...v0.9.17
[0.9.16]: https://github.com/aviatesk/JET.jl/compare/v0.9.15...v0.9.16
[0.9.15]: https://github.com/aviatesk/JET.jl/compare/v0.9.14...v0.9.15
[0.9.14]: https://github.com/aviatesk/JET.jl/compare/v0.9.13...v0.9.14
[0.9.13]: https://github.com/aviatesk/JET.jl/compare/v0.9.12...v0.9.13
[0.9.12]: https://github.com/aviatesk/JET.jl/compare/v0.9.11...v0.9.12
[0.9.11]: https://github.com/aviatesk/JET.jl/compare/v0.9.10...v0.9.11
[0.9.10]: https://github.com/aviatesk/JET.jl/compare/v0.9.9...v0.9.10
[0.9.9]: https://github.com/aviatesk/JET.jl/compare/v0.9.8...v0.9.9
[0.9.8]: https://github.com/aviatesk/JET.jl/compare/v0.9.7...v0.9.8
[0.9.7]: https://github.com/aviatesk/JET.jl/compare/v0.9.6...v0.9.7
[0.9.6]: https://github.com/aviatesk/JET.jl/compare/v0.9.5...v0.9.6
[0.9.5]: https://github.com/aviatesk/JET.jl/compare/v0.9.4...v0.9.5
[0.9.4]: https://github.com/aviatesk/JET.jl/compare/v0.9.3...v0.9.4
[0.9.3]: https://github.com/aviatesk/JET.jl/compare/v0.9.2...v0.9.3
[0.9.2]: https://github.com/aviatesk/JET.jl/compare/v0.9.1...v0.9.2
[0.9.1]: https://github.com/aviatesk/JET.jl/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/aviatesk/JET.jl/compare/v0.8.29...v0.9.0
[0.8.9]: https://github.com/aviatesk/JET.jl/compare/v0.8.8...v0.8.9
[0.8.8]: https://github.com/aviatesk/JET.jl/compare/v0.8.7...v0.8.8
[0.8.7]: https://github.com/aviatesk/JET.jl/compare/v0.8.6...v0.8.7
[0.8.6]: https://github.com/aviatesk/JET.jl/compare/v0.8.5...v0.8.6
[0.8.5]: https://github.com/aviatesk/JET.jl/compare/v0.8.4...v0.8.5
[0.8.4]: https://github.com/aviatesk/JET.jl/compare/v0.8.3...v0.8.4
[0.8.3]: https://github.com/aviatesk/JET.jl/compare/v0.8.2...v0.8.3
[0.8.2]: https://github.com/aviatesk/JET.jl/compare/v0.8.1...v0.8.2
[0.8.1]: https://github.com/aviatesk/JET.jl/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/aviatesk/JET.jl/compare/v0.7.15...v0.8.0
<!-- links end -->

## [Unreleased]
### Changed
- **Major improvement to `report_package`**: Switched to a Revise.jl-based
  implementation that brings significant improvements (aviatesk/JET.jl#763):
  - **Incremental analysis**: Analysis results are now cached and reused across
    multiple runs. When you analyze the same package again, only methods
    affected by code changes are re-analyzed, while unchanged methods reuse
    their cached results. This dramatically reduces analysis time for iterative
    development workflows.
    > E.g. Incremental analysis on JET itself

    https://github.com/user-attachments/assets/b1906040-1c95-443e-948e-d8959262f272
  - **Improved robustness**: The new implementation leverages Revise's
    battle-tested infrastructure for tracking package definitions, providing
    much more reliable analysis coverage across diverse code patterns compared
    to the previous custom code loading mechanism.
  - **Breaking change**: `report_package` now requires a `Module` argument
    instead of accepting a package name as `AbstractString`. Users must now
    load the target package before analysis. See the deprecated section for
    more details.
  - **Limitation**: As a trade-off of the Revise-based approach, `report_package`
    can no longer analyze packages that fail to load. Previously, `report_package`
    could atl least report top-level errors for such packages (though it
    couldn't perform inference-based analysis). Now users must fix loading
    errors before applying JET analysis via `report_package`. These errors are
    typically straightforward to fix by examining the error output from
    `using MyPkg` or `Pkg.precompile()`.
  - The previous default configurations `analyze_from_definitions=true` and
    `concretization_patterns=[:(x_)]` are no longer needed or used, as the
    Revise-based approach does not require JET's own code loading mechanism.
- Revise.jl is now a required dependency instead of an optional weak
  dependency. This means `watch_package` no longer requires manually loading
  Revise with `using Revise` before use.
- Enabled the [ad-hoc concrete evaluation](https://github.com/JuliaLang/julia/pull/59908)
  in `JETAnalyzer` for Julia v1.13 and higher, reducing false positives in more general cases
- **Module filtering behavior change**: `target_modules` and `ignored_modules`
  now include submodules by default (aviatesk/JET.jl#628, aviatesk/JET.jl#772):
  - **Submodule-inclusive filtering**: When a `Module` object is passed to
    `target_modules` or `ignored_modules`, JET now matches not only that exact
    module but also all of its submodules. This provides more intuitive
    filtering behavior in most use cases.
  - **Breaking change**: This changes the filtering behavior when
    `target_modules` or `ignored_modules` accept an iterator of modules.
    To preserve the previous exact-match behavior (matching only the specified
    module without its submodules), wrap the module with `LastFrameModuleExact`
    or `AnyFrameModuleExact`:
    ```julia
    # New default (v0.11+): matches MyPackage and all its submodules
    report_call(f, args...; target_modules=(MyPackage,))

    # Previous behavior (v0.10): matched only MyPackage exactly
    # To preserve this in v0.11+, use:
    report_call(f, args...; target_modules=(LastFrameModuleExact(MyPackage),))
    ```
  - New matcher types:
    - `ReportMatcher`: abstract interface type for `JET.match_report`
    - `LastFrameModuleExact`: exact match in last frame only
    - `AnyFrameModuleExact`: exact match in any frame

### Added
- **Symbol-based module filtering**: `target_modules` and `ignored_modules` now
  accept `Symbol` in addition to `Module` objects (aviatesk/JET.jl#602, aviatesk/JET.jl#773).
  This allows filtering by module name without requiring the module as a dependency:
  ```julia
  # Filter by module name without loading CUDA package
  report_call(f, args...; ignored_modules=(:CUDA,))

  # Equivalent to passing the Module object (if CUDA is loaded)
  report_call(f, args...; ignored_modules=(CUDA,))
  ```
  All matcher types (`LastFrameModule`, `AnyFrameModule`, `LastFrameModuleExact`,
  `AnyFrameModuleExact`) now accept `Union{Module,Symbol}`.

### Deprecated
- `report_package(::AbstractString)`, `report_package([::Nothing])`:
  The old signatures accepting a package name as a string, or no arguments are
  deprecated. Load the package first and pass the `Module` instead:
  ```julia-repl
  # Preferred (v0.11):
  julia> using MyPackage
  julia> report_package(MyPackage)

  # Deprecated (v0.10):
  julia> report_package("MyPackage")
  julia> report_package()
  ```
- `target_defined_modules` configuration: Use the more flexible `target_modules`
  configuration instead. To limit error reports to your package's module
  context (filtering out errors from dependencies), use:
  ```julia
  report_package(MyPackage; target_modules=(MyPackage,))
  ```

## [0.10.12]
### Fixed
- Enabled more concrete evaluation in `report_call`, fixing false positive reports
  such as those reported for `@report_call 1.0 > π` (aviatesk/JET.jl#767)
- Fixed error reporting for `setglobal!` on variables with incompatible types,
  fixing a test failure on v1.12 (aviatesk/JET.jl#768).

## [0.10.11]
### Fixed
- Fixed compatibility with `Base.@deprecate_binding` macro, which before
  could trigger an internal error in packages using it (aviatesk/JET.jl#733, aviatesk/JET.jl#765).

## [0.10.10]
### Fixed
- Fix locking issue with `Base.require_lock` that occurs in Julia 1.12.1 (aviatesk/JET.jl#762)

## [0.10.9]
### Fixed
- Fixed method error from the `LoweringErrorReport` constructor.
- Added support for macros that return `:toplevel` expressions, fixing
  wrong error reports from e.g. [`@enumx`](https://github.com/fredrikekre/EnumX.jl)
  macros (aviatesk/JET.jl#748, aviatesk/JET.jl#759).

## [0.10.8]
### Added
- ui: Add `sourceinfo` configuration option to control file path display in
  error reports. The new option supports five modes:
  - `:full` - Expand all file paths to absolute paths
  - `:default` - Show paths as-is, prefixing `./` only for relative paths
  - `:compact` - Show basename only for absolute paths, relative paths unchanged
  - `:minimal` - Show only module information, omit file paths
  - `:none` - Omit location information entirely (treated as `:compact` for
    toplevel errors where location is essential)

### Deprecated
- ui: The `fullpath` configuration option is now deprecated in favor of
  `sourceinfo`. Use `sourceinfo=:full` instead of `fullpath=true`.

## [0.10.7]
### Changed
- Precompilation of JET has been re-enabled. This should significantly improve
  startup times:
  ```bash
  $ julia --startup-file=no -e '
    macro simpletime(ex)
        :(let s = time()
            $(ex)
            e = time()
            elapsed = round(e - s; digits=2)
            println("  Elapsed: $elapsed sec")
        end)
    end
    @simpletime using JET
    @simpletime @report_call sum("julia")
    @simpletime report_file(normpath(pkgdir(JET), "demo.jl"); toplevel_logger=nothing)'
  ```
  ```
  v0.10.6
    Elapsed: 1.25 sec
    Elapsed: 10.08 sec
    Elapsed: 11.43 sec
  This version
    Elapsed: 1.33 sec
    Elapsed: 0.01 sec
    Elapsed: 0.14 sec
  ```

  This makes the precompilation during installation take longer.
  If you're developing JET itself, we recommend having:
  > LocalPreferences.toml
  ```toml
  [JET]
  precompile_workload = false
  ```
- `report_call` and `report_package` now reports undefined local variables.
  Error reports should be obtained for local variables that can be undefined
  in cases like the following:
  ```julia-repl
  julia> report_call((Int,)) do x
             local y
             return sin(y) # `y` is not defined here
         end
  ═════ 1 possible error found ═════
  ┌ (::var"#8#9")(x::Int64) @ Main ./REPL[7]:3
  │ local variable `y` is not defined: y
  └────────────────────

  julia> report_call((Bool,Int,)) do c, x
             if c
                 y = x
             end
             return sin(y) # `y` may be undefined here
         end
  ═════ 1 possible error found ═════
  ┌ (::var"#5#6")(c::Bool, x::Int64) @ Main ./REPL[6]:5
  │ local variable `y` may be undefined: y::Int64
  └────────────────────
  ```
- Improved accuracy of `VirtualProcessResult.analyzed_files`.
- JET now requires JuliaSyntax v1.0.
- JET's pre-defined analyzers (`JETAnalyzer` and `OptAnalyzer`) and interpreters
  (`JETConcreteInterpreter`) now use fixed world ages for improved robustness
  against invalidations that may be caused by loading external packages
  (aviatesk/JET.jl#732).
  This behavior is enabled by default (more specifically, it is enabled when the
  `JET_DEV_MODE` is turned off). You can configure this behavior by setting
  the new `JET.use_fixed_world = !JET.JET_DEV_MODE` preference.
- Updated depedency versions (allowing JET to be used with CodeTracking v2).
- For errors that occur during JET's top-level analysis, it is now possible to
  distinguish between those that occur during macro expansion
  (`JET.MacroExpansionErrorReport`) and those that occur during lowering
  (`JET.LoweringErrorReport`) (aviatesk/JET.jl#737).
### Removed
- **BREAKING**: The `ReportPass` interface has been removed. The `[@]report_call`
  entrypoints continue to support the `mode::Symbol` option with values
  `:basic`, `:typo`, or `:sound` (aviatesk/JET.jl#731).
  - Advanced analysis customization that was previously possible using the
    `ReportPass` interface can now be achieved by the `JET.JETAnalyzer` type,
    which is now an abstract type.
  - For `OptAnalyzer`, such customization is no longer possible, but for most
    use cases, simply configuring `function_filter` is sufficient.
- **WARNING**: Support for Cthulhu extension has been temporarily removed.
  This is because JET requires JuliaSyntax@1.0, while Cthulhu still requires JuliaSyntax@0.4.
  We will restore extension support once Cthulhu (or more precisely, TypedSyntax.jl) supports JuliaSyntax@1.0.
- Removed the `include_callback` functionality that was added in [0.9.14].
  External consumers should subtype `JET.ConcreteInterpreter` and implement
  their own customized interpretation logic (aviatesk/JET.jl#721).
### Added (Internal)
- Added the ability for external users of JET to customize virtualprocess.jl.
  Similar to the design of `JuliaInterpreter.Interpreter` and `Base.Compiler.AbstractInterpreter`,
  the new `JET.ConcreteInterpreter <: JuliaInterpreter.Interpreter` interface is designed,
  allowing external packages to subtype it and customize the behavior of `virtual_process(interp::JET.ConcreteInterpreter, ...)`.
  Please note that this is still undocumented and is a highly experimental interface.
  Currently, it is being experimentally used in the [JETLS](https://github.com/aviatesk/JETLS.jl) project. (aviatesk/JET.jl#721).
- Introduced `ToplevelAbstractAnalyzer` interface type that extends `AbstractAnalyzer` to provide
  clear separation between analyzers that support top-level analysis and those that don't.
  This architectural improvement ensures type safety by restricting `virtual_process` usage to
  analyzers that explicitly extend `ToplevelAbstractAnalyzer`, while keeping non-toplevel
  analyzers like `OptAnalyzer` free from toplevel-specific logic (aviatesk/JET.jl#722).
- Added `typeinf_world(analyzer::AbstractAnalyzer)` optional interface for
  controlling the world age used during type inference (aviatesk/JET.jl#721).
- Added `interpret_world(interp::ConcreteInterpreter)` optional interface for
  controlling the world age used during interpretation (aviatesk/JET.jl#721).

## [0.10.6]
### Changed
- Started to use the new [Compiler.jl](https://github.com/JuliaLang/BaseCompiler.jl) stdlib
  as the base compiler, allowing easier switch of the compiler implementation (aviatesk/JET.jl#710).
- `JETInterface` now includes the `ToplevelErrorReport` interface.

## [0.10.5]
### Changed
- Updated JuliaInterpreter to v0.10

## [0.10.4]
### Changed
- Includes internal dependency updates

## [0.10.3]

Reimplemented top-level analysis features like `report_package`.
By updating to this version, you can use these features on Julia v1.12.

However, to use JET v0.10.3, you need to use Julia version `v"1.12.0-beta2"` or later.
~~As of April 19, 2025, you need to build Julia from [this branch](https://github.com/JuliaLang/julia/pull/58009)
for using this 1.12 beta version.~~
Updated (April 26, 2025): Julia version `v"1.12.0-beta2"` is available at
[this link](https://julialang.org/downloads/#upcoming_release) now.

Also, please be aware that some minor features haven't been fully tested yet.
~~In particular, there might be issues related to https://github.com/timholy/Revise.jl/issues/903.~~
Updated (April 23, 2025): This issue has been fixed on the latest version of Revise.jl.

## [0.10.2]
### Changed
- JET is now compatible with JuliaSyntax v1.0

## [0.10.1]
### Fixed
- Improved the robustness of the pass to report captured variables in `report_opt`.
  Specifically, this fixes an internal error that occurred when running
  `report_opt(JSON3.read, (String,Type{CustomType}))`.

## [0.10.0]

> [!WARNING]
> **v0.10.0 is a transitional release with limited functionality**:
> - **Compatibility**: JET v0.10.0 supports Julia v1.12 but is incompatible with v1.11.
>   Users on v1.11 should continue using JET v0.9, which will only receive bug fixes.
> - **Functionality**: This version provides basic local analysis features but lacks
>   fully functional top-level analysis capabilities. These will be addressed in future updates.
> - **Future Plans**: Development will focus on stabilizing v0.10 and refactoring JET
>   for integration with the new language server project. For stable use, stick with v0.9.

JET v0.10.0 introduces compatibility with Julia v1.12, addressing significant changes in
the runtime and compiler systems. However, maintaining compatibility with Julia v1.11 was
deemed infeasible, leading to the decision to drop support for it in this release.
Users on Julia v1.11 should remain on JET v0.9, which will continue to receive bug fixes
but no new features.

This release is a stepping stone toward full compatibility with Julia v1.12.
Due to the urgency of supporting Julia v1.12 for the [PkgEval](https://github.com/JuliaCI/PkgEval.jl)
process, JET v0.10.0 was released despite its limitations:
- **Working Features**: Basic local analysis features, such as `[@]report_call` and
  `[@]report_opt`, are expected to be functional.
- **Non-Functional Features**: Top-level analysis features, such as `report_package` and
  `report_file`, are not yet verified and will be updated in future patch releases.

### Future Development:
- Updates to the v0.10 series will focus on improving stability and functionality.
- Extensive refactoring is planned to support JET's integration into the [JETLS](https://github.com/aviatesk/JETLS.jl) project.
- Stability on Julia v1.12 may remain uncertain until these updates are complete.
  Users requiring stable functionality should continue using the v0.9 series.

### Changed
- Dropped support for Julia v1.11.

## [0.9.18]
### Change
- Allowed [the PkgEval infrastructure](https://github.com/JuliaCI/PkgEval.jl) to try to load
  JET always (aviatesk/JET.jl#690)

## [0.9.17]
### Changed
- Even when JET fails to be loaded on nightly versions, stub functions mimicking JET’s API
  are now defined. These stubs raise an error with an appropriate message when executed.
  (aviatesk/JET.jl#688, aviatesk/JET.jl#689)

## [0.9.16]
### Changed
- JET is now able to show multiple syntax errors at once, e.g.,
  > multisyntaxerrors.jl
  ```julia
  function f(W,X,Y)
      s = 0
      for i = 1:10
          s += g(W[i]*f(X[end-1] + Y[end÷2+]),
                 W[i+1]*f(X[end-2] + Y[end÷2]) +,
                 W[i+2]*f(X[end-3] + Y[end÷2-3]))
      end
      return s
  end
  ```
  ```julia
  julia> report_file("multisyntaxerrors.jl")
  [...]
  ═════ 2 toplevel errors found ═════
  ┌ @ multisyntaxerrors.jl:4
  │ # Error @ multisyntaxerrors.jl:4:42
  │     for i = 1:10
  │         s += g(W[i]*f(X[end-1] + Y[end÷2+]),
  │ #                                        ╙ ── unexpected `]`
  └──────────────────────
  ┌ @ multisyntaxerrors.jl:5
  │ # Error @ multisyntaxerrors.jl:5:47
  │         s += g(W[i]*f(X[end-1] + Y[end÷2+]),
  │                W[i+1]*f(X[end-2] + Y[end÷2]) +,
  │ #                                             ╙ ── unexpected `,`
  └──────────────────────
  ```
  (aviatesk/JET.jl#687)

## [0.9.15]
### Changed
- JET.jl now will not be loaded on nightly version by default. This ensures that JETremains
  at least loadable on nightly builds, where JET's compatibility is not guaranteed.
  If you want to load JET on a nightly version, set the `JET_DEV_MODE` configuration of
  Preferences.jl to `true` and load it as usual (aviatesk/JET.jl#684, aviatesk/JET.jl#686).
- JET now fully uses [JuliaSyntax.jl](https://github.com/JuliaLang/JuliaSyntax.jl) for reporting syntax errors (aviatesk/JET.jl#685).

## [0.9.14]
### Added
- Added `include_callback` to the virtual process, allowing custom callback logic during code inclusion (aviatesk/JET.jl#683).

## [0.9.13]
- Includes internal updates.

## [0.9.12]
### Fixed
- Fixed a broadcasting-related issue in `@report_call` (which caused false error reports) by
  allowing concrete evaluation for `typejoin` (aviatesk/JET.jl#669, aviatesk/JET.jl#670).

## [0.9.11]
### Fixed
- Fixed an exception thrown by `report_package` on Julia 1.11.1 (aviatesk/JET.jl#668).

## [0.9.10]
### Changed
- The improved statement selection logic implemented in v0.9.9 is now ported to
  LoweredCodeUtils.jl@3.0.2, so that it can shared by JET.jl and Revise.jl.

## [0.9.9]
### Added
- Added [reference documentation](https://aviatesk.github.io/JET.jl/dev/internals/#optanalysis-splitting)
  on JET’s analysis report-splitting feature (aviatesk/JET.jl#652).
### Changed
- Implemented an improved control-flow graph analysis and statement selection logic,
  enhancing JET’s top-level analysis accuracy (aviatesk/JET.jl#654).

## [0.9.8]
### Added
- An extension that integrates `@report_opt` with Cthulhu (aviatesk/JET.jl#648)
- `reportkey` for trimming multiple reports that resolve to the same runtime-dispatch caller/callee pair (aviatesk/JET.jl#648)

## [0.9.7]
- Updated dependencies, made minor refactorings.

## [0.9.6]
### Fixed
- `report_opt` no longer raises reports from callees on `throw` code path when the
  `skip_unoptimized_throw_blocks::Bool=true` configuration is enabled (aviatesk/JET.jl#643).

## [0.9.5]
### Added
- `analyze_from_definitions` can now be specified as `entry_point_name::Symbol` to make
  JET's top-level analyses start analysis using the interpreted method signature whose name
  is equal to `entry_point_name` as the analysis entry point. For example, when analyzing a
  script that specifies its entry point using the new `@main` special macro, you can specify
  `report_file(script_name; analyze_from_definitions=:main)` to automatically start the
  analysis from the `main(args)` function.

## [0.9.4]
### Changed
- Made some adjustments to the warning text in the README.

## [0.9.3]
### Added
- A simple logo badge for JET.jl is now available (thanks to @MilesCranmer!).
  You can add the line `[![](https://img.shields.io/badge/%F0%9F%9B%A9%EF%B8%8F_tested_with-JET.jl-233f9a)](https://github.com/aviatesk/JET.jl)`
  to your package's README to display the logo image [![](https://img.shields.io/badge/%F0%9F%9B%A9%EF%B8%8F_tested_with-JET.jl-233f9a)](https://github.com/aviatesk/JET.jl)
  that shows your package uses JET.jl for code quality checks (aviatesk/JET.jl#635).
- JET's top-level analyses such as `report_package` and `report_file` can now handle the
  new `public` keyword that is introduced in v1.11. (aviatesk/JET.jl#637)

## [0.9.2]
### Fixed
- Allow overly deep relative module paths when analyzing a package with `report_package`
  (aviatesk/JET.jl#619, aviatesk/JET.jl#633)

## [0.9.1]
### Fixed
- Fixed the issue where the line numbers of methods whose locations were revised by Revise
  were not being updated (aviatesk/JET.jl#513).

## [0.9.0]
### Added
- A new configuration `stacktrace_types_limit::Union{Nothing,Int}=nothing` has been added.
  It's turned on by default and limits deeply nested types when JET prints reports.
  If you prefer the old behavior, set `stacktrace_types_limit=0` (aviatesk/JET.jl#601).
### Changed
- Revise.jl-related features are now implemented as a package extension, so in order to use
  `watch_file`, you need to load Revise.jl into your session first
  (aviatesk/JET.jl#624, aviatesk/JET.jl#625).
- The compatibility with LoweredCodeUtils has been raised to version 2.4 and later, so it's
  now possible to use the latest version of Revise with JET again. Additionally, the
  top-level statement selection algorithm internally used by top-level analysis functions
  like `report_file` has been significantly improved.

## [0.8.9]
### Fixed
- `report_package` now supports the `using Module: Inner.object` syntax
  (aviatesk/JET.jl#554, aviatesk/JET.jl#555).
- Various internal improvements.

## [0.8.8]
### Fixed
- `report_package` now supports the `import Module as Alias` syntax
  (aviatesk/JET.jl#521, aviatesk/JET.jl#553).

## [0.8.7]
### Changed
- Changed the default `toplevel_logger` configuration for `test_package` to `nothing`.
  `test_package` no longer emits logs like `[toplevel-info] analyzing from top-level definitions (xxx/yyy)`
  (aviatesk/JET.jl#550).

## [0.8.6]
### Fixed
- Fixed the default `ignore_missing_comparison` configuration for `report_package`.

## [0.8.5]
### Fixed
- Fixed `report_package` so that it does not produce noisy error reports from reducing on
  potentially empty collections.

## [0.8.4]
### Added
- Made the `(x == y)::Union{Missing,Bool} → Any` widening behavior for `report_package`
  (that was added in aviatesk/JET.jl#542) configurable. Specify `report_package("TargetPkg", ignore_missing_comparison=false)`
  if `TargetPkg` handles `missing` (aviatesk/JET.jl#547).

## [0.8.3]
### Changed
- Generalized the `(x == y)::Union{Missing,Bool} → Any` widening behavior for
  `report_package` that was added in aviatesk/JET.jl#542 to other comparison operators
  (e.g. `in`) (aviatesk/JET.jl#545).

## [0.8.2]
### Changed
- JET now ignores the possibility of a poorly-inferred `x == y` call returning `missing`
  during the `report_package` analysis. Refer to issue aviatesk/JET.jl#542 for reasons
  justifying this behavior. Essentially, `report_package` often relies on poor input
  argument type information at the beginning of analysis, leading to noisy error reports
  for function definitions like:
  ```julia
  struct MyToken end
  ismytoken(x) = x == MyToken() ? true : false
  ```
  This error is arguably just noise when the target package does not handle `missing`.
  `report_package` is designed as an entry point for easy analysis, even at the cost of
  accuracy, so it is not sound from the beginning. Hence, it might be beneficial to simply
  ignore such noise.

  However note that in interactive entry points like `report_call`, where concrete input
  argument types are available, this behavior should be turned off. This is because,
  if the code, when given specific input argument types, results in a `Union{Bool,Missing}`
  possibility, it likely signifies an inferrability issue or the code really needs to handle
  `missing`
  (aviatesk/JET.jl#541, aviatesk/JET.jl#542).

## [0.8.1]
### Fixed
- `report_package` now supports the `using MyPkg` syntax (without specifying relative module
  path `...`) from inner modules of `MyPkg` (aviatesk/JET.jl#539, aviatesk/JET.jl#540).

## [0.8.0]
### Added
- `report_call` and `report_opt` can now analyze `mi::MethodInstance`.
  This feature allows JET to analyze method instances collected by
  `MethodAnalysis.methodinstances`.
  See the [documentation](https://aviatesk.github.io/JET.jl/dev/tutorial/#Analyze-packages-using-a-representative-workload)
  for the details.
  (aviatesk/JET.jl#510)
- This CHANGELOG.md has been added and will be updated (aviatesk/JET.jl#536).
### Changed
- JET's tree-like view, which represents inference stacktrace leading to each error point,
  now closely resembles the stacktrace displayed by Julia Base upon exception.
  The new view should be more intuitive for general users and additionally, the type
  information of arguments of each frame are nicely truncated, [as in Julia Base](https://github.com/JuliaLang/julia/pull/49795).
  > Before
  ```julia
  julia> @report_call sum([])
  ═════ 1 possible error found ═════
  ┌ @ reducedim.jl:996 Base.:(var"#sum#821")(:, pairs(NamedTuple()), #self#, a)
  │┌ @ reducedim.jl:996 Base._sum(a, dims)
  ││┌ @ reducedim.jl:1000 Base.:(var"#_sum#823")(pairs(NamedTuple()), #self#, a, _3)
  │││┌ @ reducedim.jl:1000 Base._sum(identity, a, :)
  ││││┌ @ reducedim.jl:1001 Base.:(var"#_sum#824")(pairs(NamedTuple()), #self#, f, a, _4)
  │││││┌ @ reducedim.jl:1001 mapreduce(f, Base.add_sum, a)
  ││││││┌ @ reducedim.jl:357 Base.:(var"#mapreduce#814")(:, Base._InitialValue(), #self#, f, op, A)
  │││││││┌ @ reducedim.jl:357 Base._mapreduce_dim(f, op, init, A, dims)
  ││││││││┌ @ reducedim.jl:365 Base._mapreduce(f, op, IndexStyle(A), A)
  │││││││││┌ @ reduce.jl:432 Base.mapreduce_empty_iter(f, op, A, Base.IteratorEltype(A))
  ││││││││││┌ @ reduce.jl:380 Base.reduce_empty_iter(Base.MappingRF(f, op), itr, ItrEltype)
  │││││││││││┌ @ reduce.jl:384 Base.reduce_empty(op, eltype(itr))
  ││││││││││││┌ @ reduce.jl:361 Base.mapreduce_empty(op.f, op.rf, T)
  │││││││││││││┌ @ reduce.jl:372 Base.reduce_empty(op, T)
  ││││││││││││││┌ @ reduce.jl:352 Base.reduce_empty(+, T)
  │││││││││││││││┌ @ reduce.jl:343 zero(T)
  ││││││││││││││││┌ @ missing.jl:106 Base.throw(Base.MethodError(zero, tuple(Base.Any)))
  │││││││││││││││││ MethodError: no method matching zero(::Type{Any}): Base.throw(Base.MethodError(zero, tuple(Base.Any)::Tuple{DataType})::MethodError)
  ││││││││││││││││└──────────────────
  ```
  > After
  ```julia
  julia> @report_call sum([])
  ═════ 1 possible error found ═════
  ┌ sum(a::Vector{Any}) @ Base ./reducedim.jl:996
  │┌ sum(a::Vector{Any}; dims::Colon, kw::@Kwargs{}) @ Base ./reducedim.jl:996
  ││┌ _sum(a::Vector{Any}, ::Colon) @ Base ./reducedim.jl:1000
  │││┌ _sum(a::Vector{Any}, ::Colon; kw::@Kwargs{}) @ Base ./reducedim.jl:1000
  ││││┌ _sum(f::typeof(identity), a::Vector{Any}, ::Colon) @ Base ./reducedim.jl:1001
  │││││┌ _sum(f::typeof(identity), a::Vector{Any}, ::Colon; kw::@Kwargs{}) @ Base ./reducedim.jl:1001
  ││││││┌ mapreduce(f::typeof(identity), op::typeof(Base.add_sum), A::Vector{Any}) @ Base ./reducedim.jl:357
  │││││││┌ mapreduce(f::typeof(identity), op::typeof(Base.add_sum), A::Vector{Any}; dims::Colon, init::Base._InitialValue) @ Base ./reducedim.jl:357
  ││││││││┌ _mapreduce_dim(f::typeof(identity), op::typeof(Base.add_sum), ::Base._InitialValue, A::Vector{Any}, ::Colon) @ Base ./reducedim.jl:365
  │││││││││┌ _mapreduce(f::typeof(identity), op::typeof(Base.add_sum), ::IndexLinear, A::Vector{Any}) @ Base ./reduce.jl:432
  ││││││││││┌ mapreduce_empty_iter(f::typeof(identity), op::typeof(Base.add_sum), itr::Vector{Any}, ItrEltype::Base.HasEltype) @ Base ./reduce.jl:380
  │││││││││││┌ reduce_empty_iter(op::Base.MappingRF{typeof(identity), typeof(Base.add_sum)}, itr::Vector{Any}, ::Base.HasEltype) @ Base ./reduce.jl:384
  ││││││││││││┌ reduce_empty(op::Base.MappingRF{typeof(identity), typeof(Base.add_sum)}, ::Type{Any}) @ Base ./reduce.jl:361
  │││││││││││││┌ mapreduce_empty(::typeof(identity), op::typeof(Base.add_sum), T::Type{Any}) @ Base ./reduce.jl:372
  ││││││││││││││┌ reduce_empty(::typeof(Base.add_sum), ::Type{Any}) @ Base ./reduce.jl:352
  │││││││││││││││┌ reduce_empty(::typeof(+), ::Type{Any}) @ Base ./reduce.jl:343
  ││││││││││││││││┌ zero(::Type{Any}) @ Base ./missing.jl:106
  │││││││││││││││││ MethodError: no method matching zero(::Type{Any}): Base.throw(Base.MethodError(zero, tuple(Base.Any)::Tuple{DataType})::MethodError)
  ││││││││││││││││└────────────────────
  ```
  (aviatesk/JET.jl#524)
- A predicate function that is specified as the
  [`function_filter`](https://aviatesk.github.io/JET.jl/dev/optanalysis/#JET.OptAnalyzer)
  configuration now takes a function object instead of its type.
  For instance, the following code
  ```julia
  myfilter(@nospecialize ft) = !(
      ft === typeof(Base.mapreduce_empty) ||
      ft === typeof(Base.reduce_empty))
  @test_opt function_filter=myfilter func(args...)
  ```
  should now be written:
  ```julia
  myfilter(@nospecialize f) = !(
      f === Base.mapreduce_empty ||
      f === Base.reduce_empty)
  @test_opt function_filter=myfilter func(args...)
  ```
  (aviatesk/JET.jl#507).
### Removed
- Dropped the support for Julia 1.8. JET now supports Julia 1.9 and above (aviatesk/JET.jl#527).
- `report_and_watch_file` has been removed. Use `watch_file` instead.
### Fixed
- Concrete evaluation is now enabled within JET's error analysis. This fixes numerous false
  positive error reports, and leads to faster analysis speed (aviatesk/JET.jl#529,
  aviatesk/JET.jl#523, aviatesk/JET.jl#522).
- `report_package` no longer reports error from methods that are intentionally designed to throw, e.g.
  ```julia
  @noinline raise_error(x::T) where T = error(lazy"Missing interface implementation for $T")
  ```
  (aviatesk/JET.jl#532, aviatesk/JET.jl#477).
- `report_package` no longer reports error from methods with keyword arguments that don't
  have default values, e.g.
  ```julia
  struct Bar
      x
  end
  Bar(; x) = Bar(x)
  ```
  (aviatesk/JET.jl#532, aviatesk/JET.jl#478).
- Fixed false error report from `Base.aligned_sizeof` (aviatesk/JET.jl#512,
  aviatesk/JET.jl#514, JuliaLang/julia#49801).
- The optimization analysis has been adjusted to prevent skipping the reporting of runtime
  dispatches within non-compileable but inlineable frames (aviatesk/JET.jl#526).
- The sound error analysis mode has been fixed and now reports if there are any unanalyzed
  function calls, which typically occur due to excessive matching methods (aviatesk/JET.jl#533).
- `report_file` can now handle parameterized type alias definitions (aviatesk/JET.jl#534).
- Extensive refactoring and cleanup has been carried out.
