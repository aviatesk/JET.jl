# JET.jl
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
[![](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml)
[![](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)
[![](https://img.shields.io/badge/%F0%9F%9B%A9%EF%B8%8F_tested_with-JET.jl-233f9a)](https://github.com/aviatesk/JET.jl)

JET employs Julia's type inference system to detect potential bugs and type instabilities.

> [!NOTE]
> **The current latest version, v0.10 series, is only compatible with
> [Julia v1.12](https://julialang.org/downloads/#current_stable_release) only**
>
> The JET version that works with v1.11 is the [v0.9 series](https://github.com/aviatesk/JET.jl/tree/release-0.9),
> but please note that bug fixes and new features added in the v0.10 series may not necessarily be available.

> [!WARNING]
> Please note that due to JET's tight integration with the Julia compiler, the results
> presented by JET can vary significantly depending on the version of Julia you are using.
> Additionally, the implementation of the `Base` module and standard libraries bundled with
> Julia can also affect the results.
>
> Moreover, the Julia compiler's plugin system is still unstable and its interface changes
> frequently, so each version of JET is compatible with only limited versions of Julia.
> The Julia package manager will automatically select and install the latest version of JET
> that is compatible with your Julia version. However, if you are using the nightly version
> of Julia, please note that a compatible version of JET may not have been released yet,
> and JET installed via the Julia package manager may not function properly.

## Quickstart
See more commands, options and explanations in [the documentation](https://aviatesk.github.io/JET.jl/dev/).

### Installation
JET is a standard Julia package.
So you can just install it via Julia's built-in package manager and use it just like any other package:

```julia-repl noeval
julia> using Pkg; Pkg.add("JET")
[ some output elided ]

julia> using JET
```

> [!IMPORTANT]
> The package manager will install the latest version of JET available for your Julia version.
> However, depending on the versions of dependency packages already installed
> in your environment, a working version of JET may not be installed.
>
> This can particularly occur when the version of [JuliaInterpreter.jl](https://github.com/JuliaDebug/JuliaInterpreter.jl)
> is incompatible with JET, since JuliaInterpreter is also a dependency of the
> very commonly used package [Revise.jl](https://github.com/timholy/Revise.jl).
>
> In such cases, the most reliable way to install and use a working JET is to
> set up a temporary environment (e.g., `Pkg.temp()`) and use JET there.

### Detect type instability with `@report_opt`
Type instabilities can be detected in function calls using the `@report_opt` macro, which works similar to the `@code_warntype` macro.
Note that, because JET relies on Julia's type inference, if a chain of inference is broken due to dynamic dispatch, then all downstream function calls will be unknown to the compiler, and so JET cannot analyze them.

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
This works best on type stable code, so use `@report_opt` liberally before using `@report_call`.
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
This looks for all method definitions and analyses function calls based on their signatures. Note that this is less accurate than `@report_call`, because the actual input types cannot be known for generic methods.

```julia-repl
julia> using Pkg; Pkg.activate(; temp=true, io=devnull); Pkg.add("AbstractTrees"; io=devnull);

julia> Pkg.status()
Status `/private/var/folders/xh/6zzly9vx71v05_y67nm_s9_c0000gn/T/jl_h07K2m/Project.toml`
  [1520ce14] AbstractTrees v0.4.5

ulia> report_package(AbstractTrees)
[toplevel-info] Analyzing top-level definition (progress: 256/256)
[toplevel-info] Analyzed all top-level definitions (all: 256 | analyzed: 256 | cached: 0 | took: 7.116 sec)
[ Info: tracking Base
═════ 7 possible errors found ═════
┌ isroot(root::Any, x::Any) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:102
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(root::Any, x::Any)
└────────────────────
┌ StableNode{T}(x::T, ch::Any) where T @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:260
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
┌ IndexNode(tree::Any) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:117
│ no matching method found `rootindex(::Any)`: rootindex(tree::Any)
└────────────────────
┌ parent(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:127
│ no matching method found `parentindex(::Any, ::Any)`: pidx = parentindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ nextsibling(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:132
│ no matching method found `nextsiblingindex(::Any, ::Any)`: sidx = nextsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ prevsibling(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:137
│ no matching method found `prevsiblingindex(::Any, ::Any)`: sidx = prevsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────

julia> report_package(AbstractTrees; target_modules=(AbstractTrees,)) # ignore errors that occur outside the AbstractTrees module context
[toplevel-info] Skipped analysis for cached definition (256/256)
[toplevel-info] Analyzed all top-level definitions (all: 256 | analyzed: 0 | cached: 256 | took: 0.036 sec)
═════ 5 possible errors found ═════
┌ isroot(root::Any, x::Any) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/base.jl:102
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(root::Any, x::Any)
└────────────────────
┌ IndexNode(tree::Any) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:117
│ no matching method found `rootindex(::Any)`: rootindex(tree::Any)
└────────────────────
┌ parent(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:127
│ no matching method found `parentindex(::Any, ::Any)`: pidx = parentindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ nextsibling(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:132
│ no matching method found `nextsiblingindex(::Any, ::Any)`: sidx = nextsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
┌ prevsibling(idx::IndexNode) @ AbstractTrees /Users/aviatesk/.julia/packages/AbstractTrees/Ftf8W/src/indexing.jl:137
│ no matching method found `prevsiblingindex(::Any, ::Any)`: sidx = prevsiblingindex((idx::IndexNode).tree::Any, (idx::IndexNode).index::Any)
└────────────────────
```

## Limitations
JET explores the functions you call directly as well as their *inferable* callees. However, if the argument types for a call cannot be inferred, JET does not analyze the callee. Consequently, a report of `No errors detected` does not imply that your entire codebase is free of errors. To increase the confidence in JET's results use `@report_opt` to make sure your code is inferrible.

JET integrates with [SnoopCompile](https://github.com/timholy/SnoopCompile.jl), and you can sometimes use SnoopCompile to collect the data to perform more comprehensive analyses. SnoopCompile's limitation is that it only collects data for calls that have not been previously inferred, so you must perform this type of analysis in a fresh session.

See [SnoopCompile's JET-integration documentation](https://timholy.github.io/SnoopCompile.jl/stable/jet/) for further details.

## Acknowledgement
This project started as my undergrad thesis project at Kyoto University, supervised by Prof. Takashi Sakuragawa.
We were heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof), an experimental type understanding/checking tool for Ruby.
The grad thesis about this project is published at <https://github.com/aviatesk/grad-thesis>, but currently, it's only available in Japanese.
