# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

<!-- links -->

[unreleased]: https://github.com/aviatesk/JET.jl/compare/v0.8.8...HEAD
[0.8.8]: https://github.com/aviatesk/JET.jl/compare/v0.8.7...v0.8.8
[0.8.7]: https://github.com/aviatesk/JET.jl/compare/v0.8.6...v0.8.7
[0.8.6]: https://github.com/aviatesk/JET.jl/compare/v0.8.5...v0.8.6
[0.8.5]: https://github.com/aviatesk/JET.jl/compare/v0.8.4...v0.8.5
[0.8.4]: https://github.com/aviatesk/JET.jl/compare/v0.8.3...v0.8.4
[0.8.3]: https://github.com/aviatesk/JET.jl/compare/v0.8.2...v0.8.3
[0.8.2]: https://github.com/aviatesk/JET.jl/compare/v0.8.1...v0.8.2
[0.8.1]: https://github.com/aviatesk/JET.jl/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/aviatesk/JET.jl/compare/v0.7.15...v0.8.0
