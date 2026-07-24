# JET.jl
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
[![](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml)
[![](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)
[![](https://img.shields.io/badge/%F0%9F%9B%A9%EF%B8%8F_tested_with-JET.jl-233f9a)](https://github.com/aviatesk/JET.jl)

JET employs Julia's type inference system to detect potential bugs and type
instabilities.

> [!NOTE]
> **The latest release series, v0.12, supports full JET functionality
> on Julia v1.12 and v1.13 only.**
>
> The JET version that works with Julia v1.11 is the
> [v0.9 series](https://github.com/aviatesk/JET.jl/tree/release-0.9),
> but note that bug fixes and new features added in later series are not
> necessarily available there.

> [!WARNING]
> Please note that due to JET's tight integration with the Julia compiler,
> the results presented by JET can vary significantly depending on the version
> of Julia you are using.
> Additionally, the implementation of the `Base` module and standard libraries
> bundled with Julia can also affect the results.
>
> Moreover, Julia's compiler plugin system is unstable and changes frequently.
> Each JET release therefore supports full functionality on only a limited set
> of Julia versions. JET may remain installable on newer Julia versions, but
> loads empty stubs by default when full functionality is unavailable.

## Julia compatibility and versioning

JET distinguishes installation compatibility from functional compatibility.

- The latest JET series keeps its Julia upper compat bound open. This allows
  packages with JET as a test dependency to instantiate on Julia pre-releases
  and nightly builds.
- Full JET functionality is loaded only on explicitly supported Julia
  versions. On unsupported future Julia versions, JET loads empty stubs and
  its analysis APIs throw an explanatory error.
- JET minor versions are bumped for ordinary semantic-versioning reasons and
  may also mark a new Julia compatibility generation. Patch releases normally
  stay within the same compatibility generation.
- When support for a new Julia minor is released, older JET releases are
  capped in the official [General](https://github.com/JuliaRegistries/General)
  registry at the last Julia minor they support. This prevents package
  resolution and lower-bound testing from selecting an old, incompatible JET.

Test environments can remain instantiable on unsupported Julia versions, and
JET-specific tests can be skipped at runtime using the `JET_AVAILABLE` constant:

```julia
using JET

if JET.JET_AVAILABLE
    include("jet_tests.jl")
end
```

Setting the `JET_DEV_MODE` preference to `true` forces JET to try loading
full functionality on an unsupported Julia version.

## Quickstart
See more commands, options, and explanations in
[the documentation](https://aviatesk.github.io/JET.jl/dev/).

### Installation
JET is a standard Julia package, so you can install it via Julia's built-in
package manager and use it just like any other package:

```julia-repl noeval
julia> using Pkg; Pkg.add("JET")
[ some output elided ]

julia> using JET
```

> [!IMPORTANT]
> The package manager installs a JET version allowed by registry metadata and
> the compatibility constraints of your environment. This does not necessarily
> mean full JET functionality is supported on your Julia version; check
> `JET.JET_AVAILABLE` after loading JET.
>
> Existing dependencies may also prevent a working JET version from being
> installed.
> This is particularly likely when the version of
> [JuliaInterpreter.jl](https://github.com/JuliaDebug/JuliaInterpreter.jl)
> is incompatible with JET, since JuliaInterpreter is also a dependency of the
> very commonly used package [Revise.jl](https://github.com/timholy/Revise.jl).
> In such cases, the most reliable way to install and use a working JET is to
> set up a temporary environment (e.g., `Pkg.activate(; temp=true)`) and use
> JET there.

### Detect type instability with `@report_opt`
Type instabilities can be detected in function calls using the `@report_opt`
macro, which works similarly to the `@code_warntype` macro.
Note that, because JET relies on Julia's type inference, it cannot see through
unresolved dynamic dispatch: callees reached only through such calls are not
analyzed, so problems inside them go unreported.

```julia-repl
julia> @report_opt foldl(+, Any[]; init=0)
═════ 2 possible errors found ═════
┌ kwcall(::@NamedTuple{init::Int64}, ::typeof(foldl), op::typeof(+), itr::Vector{Any}) @ Base ./reduce.jl:198
│┌ foldl(op::typeof(+), itr::Vector{Any}; kw::@Kwargs{init::Int64}) @ Base ./reduce.jl:198
││┌ kwcall(::@NamedTuple{init::Int64}, ::typeof(mapfoldl), f::typeof(identity), op::typeof(+), itr::Vector{Any}) @ Base ./reduce.jl:175
│││┌ mapfoldl(f::typeof(identity), op::typeof(+), itr::Vector{Any}; init::Int64) @ Base ./reduce.jl:175
││││┌ mapfoldl_impl(f::typeof(identity), op::typeof(+), nt::Int64, itr::Vector{Any}) @ Base ./reduce.jl:44
│││││┌ foldl_impl(op::Base.BottomRF{typeof(+)}, nt::Int64, itr::Vector{Any}) @ Base ./reduce.jl:48
││││││┌ _foldl_impl(op::Base.BottomRF{typeof(+)}, init::Int64, itr::Vector{Any}) @ Base ./reduce.jl:58
│││││││┌ (::Base.BottomRF{typeof(+)})(acc::Int64, x::Any) @ Base ./reduce.jl:86
││││││││ runtime dispatch detected: +(acc::Int64, x::Any)::Any
│││││││└────────────────────
││││││┌ _foldl_impl(op::Base.BottomRF{typeof(+)}, init::Int64, itr::Vector{Any}) @ Base ./reduce.jl:62
│││││││┌ (::Base.BottomRF{typeof(+)})(acc::Any, x::Any) @ Base ./reduce.jl:86
││││││││ runtime dispatch detected: +(acc::Any, x::Any)::Any
│││││││└────────────────────
```

### Detect type errors with `@report_call`
While `@report_opt` detects performance problems, `@report_call` detects
potential bugs: calls that may throw at runtime, such as `MethodError`s.
Since JET cannot see through unresolved dynamic dispatch, fixing the
instabilities reported by `@report_opt` first lets `@report_call` cover more
of your code. That said, `@report_call` is often less noisy than `@report_opt`,
so it is also perfectly reasonable to start with `@report_call` alone.

```julia-repl
julia> @report_call foldl(+, Char[])
═════ 2 possible errors found ═════
┌ foldl(op::typeof(+), itr::Vector{Char}) @ Base ./reduce.jl:198
│┌ foldl(op::typeof(+), itr::Vector{Char}; kw::@Kwargs{}) @ Base ./reduce.jl:198
││┌ mapfoldl(f::typeof(identity), op::typeof(+), itr::Vector{Char}) @ Base ./reduce.jl:175
│││┌ mapfoldl(f::typeof(identity), op::typeof(+), itr::Vector{Char}; init::Base._InitialValue) @ Base ./reduce.jl:175
││││┌ mapfoldl_impl(f::typeof(identity), op::typeof(+), nt::Base._InitialValue, itr::Vector{Char}) @ Base ./reduce.jl:44
│││││┌ foldl_impl(op::Base.BottomRF{typeof(+)}, nt::Base._InitialValue, itr::Vector{Char}) @ Base ./reduce.jl:48
││││││┌ _foldl_impl(op::Base.BottomRF{typeof(+)}, init::Base._InitialValue, itr::Vector{Char}) @ Base ./reduce.jl:62
│││││││┌ (::Base.BottomRF{typeof(+)})(acc::Char, x::Char) @ Base ./reduce.jl:86
││││││││ no matching method found `+(::Char, ::Char)`: (op::Base.BottomRF{typeof(+)}).rf::typeof(+)(acc::Char, x::Char)
│││││││└────────────────────
│││││┌ foldl_impl(op::Base.BottomRF{typeof(+)}, nt::Base._InitialValue, itr::Vector{Char}) @ Base ./reduce.jl:49
││││││┌ reduce_empty_iter(op::Base.BottomRF{typeof(+)}, itr::Vector{Char}) @ Base ./reduce.jl:383
│││││││┌ reduce_empty_iter(op::Base.BottomRF{typeof(+)}, itr::Vector{Char}, ::Base.HasEltype) @ Base ./reduce.jl:384
││││││││┌ reduce_empty(op::Base.BottomRF{typeof(+)}, ::Type{Char}) @ Base ./reduce.jl:360
│││││││││┌ reduce_empty(::typeof(+), ::Type{Char}) @ Base ./reduce.jl:343
││││││││││ no matching method found `zero(::Type{Char})`: zero(T::Type{Char})
│││││││││└────────────────────
```

### Analyze packages with `report_package`
This looks for all method definitions and analyzes function calls based on
their signatures. Note that this is less accurate than `@report_call`, because
the actual input types cannot be known for generic methods.

```julia-repl
julia> using Pkg; Pkg.activate(; temp=true, io=devnull); Pkg.add("AbstractTrees"; io=devnull);

julia> Pkg.status()
Status `/private/var/folders/xh/6zzly9vx71v05_y67nm_s9_c0000gn/T/jl_h07K2m/Project.toml`
  [1520ce14] AbstractTrees v0.4.5

julia> using AbstractTrees

julia> report_package(AbstractTrees)
[toplevel-info] Analyzing top-level definition (progress: 256/256)
[toplevel-info] Analyzed all top-level definitions (all: 256 | analyzed: 256 | cached: 0 | took: 7.116 sec)
[ Info: tracking Base
═════ 7 possible errors found ═════
┌ isroot(root::Any, x::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:102
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(root::Any, x::Any)
└────────────────────
┌ StableNode{T}(x::T, ch::Any) where T @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:260
│┌ collect(::Type{StableNode{_A}} where _A, itr::Any) @ Base ./array.jl:641
││┌ _collect(::Type{StableNode{_A}}, itr::Any, isz::Union{Base.HasLength, Base.HasShape}) where _A @ Base ./array.jl:643
│││┌ _array_for(::Type{StableNode{_A}} where _A, itr::Base.HasLength, isz::Any) @ Base ./array.jl:673
││││┌ _similar_shape(itr::Base.HasLength, ::Base.HasLength) @ Base ./array.jl:657
│││││ no matching method found `length(::Base.HasLength)`: length(itr::Base.HasLength)
││││└────────────────────
││││┌ _similar_shape(itr::Base.HasLength, ::Base.HasShape) @ Base ./array.jl:658
│││││┌ axes(A::Base.HasLength) @ Base ./abstractarray.jl:98
││││││ no matching method found `size(::Base.HasLength)`: size(A::Base.HasLength)
│││││└────────────────────
┌ IndexNode(tree::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:117
│ no matching method found `rootindex(::Any)`: rootindex(tree::Any)
└────────────────────
┌ parent(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:127
│ no matching method found `parentindex(::Any, ::Any)`: pidx = parentindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ nextsibling(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:132
│ no matching method found `nextsiblingindex(::Any, ::Any)`: sidx = nextsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ prevsibling(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:137
│ no matching method found `prevsiblingindex(::Any, ::Any)`: sidx = prevsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────

julia> report_package(AbstractTrees; target_modules=(AbstractTrees,)) # ignore errors that occur outside the AbstractTrees module context
[toplevel-info] Skipped analysis for cached definition (256/256)
[toplevel-info] Analyzed all top-level definitions (all: 256 | analyzed: 0 | cached: 256 | took: 0.036 sec)
═════ 5 possible errors found ═════
┌ isroot(root::Any, x::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:102
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(root::Any, x::Any)
└────────────────────
┌ IndexNode(tree::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:117
│ no matching method found `rootindex(::Any)`: rootindex(tree::Any)
└────────────────────
┌ parent(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:127
│ no matching method found `parentindex(::Any, ::Any)`: pidx = parentindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ nextsibling(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:132
│ no matching method found `nextsiblingindex(::Any, ::Any)`: sidx = nextsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ prevsibling(idx::IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:137
│ no matching method found `prevsiblingindex(::Any, ::Any)`: sidx = prevsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
```

## Limitations
JET explores the functions you call directly as well as their *inferable* callees.
However, if the argument types for a call cannot be inferred, JET does not
analyze the callee. Consequently, a report of `No errors detected` does not
imply that your entire codebase is free of errors.
To increase confidence in JET's results, use `@report_opt` to make sure your
code is inferable.

## Acknowledgements
This project started as my undergraduate thesis at Kyoto University,
supervised by Prof. Takashi Sakuragawa.
It was heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof),
an experimental type understanding/checking tool for Ruby.
The thesis is published at <https://github.com/aviatesk/grad-thesis>,
but currently it's only available in Japanese.
