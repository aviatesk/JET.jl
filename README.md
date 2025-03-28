# JET.jl
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
[![](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml/badge.svg)](https://github.com/aviatesk/JET.jl/actions/workflows/ci.yml)
[![](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)
[![](https://img.shields.io/badge/%F0%9F%9B%A9%EF%B8%8F_tested_with-JET.jl-233f9a)](https://github.com/aviatesk/JET.jl)

JET employs Julia's type inference system to detect potential bugs and type instabilities.

> [!NOTE]
> Current development is based on the `release-0.10` branch.
> This branch focuses on stabilizing JET v0.10 for Julia v1.12, and preparing for its
> integration with [the new language server project](https://github.com/aviatesk/JETLS.jl).

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
  [1520ce14] AbstractTrees v0.4.4

julia> report_package("AbstractTrees")
[ some output elided ]
═════ 7 possible errors found ═════
┌ isroot(root::Any, x::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/base.jl:102
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(root::Any, x::Any)
└────────────────────
┌ AbstractTrees.IndexNode(tree::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/indexing.jl:117
│ no matching method found `rootindex(::Any)`: rootindex(tree::Any)
└────────────────────
┌ parent(idx::AbstractTrees.IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/indexing.jl:127
│ no matching method found `parentindex(::Any, ::Any)`: pidx = parentindex((idx::AbstractTrees.IndexNode).tree::Any, (idx::AbstractTrees.IndexNode).index::Any)
└────────────────────
┌ nextsibling(idx::AbstractTrees.IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/indexing.jl:132
│ no matching method found `nextsiblingindex(::Any, ::Any)`: sidx = nextsiblingindex((idx::AbstractTrees.IndexNode).tree::Any, (idx::AbstractTrees.IndexNode).index::Any)
└────────────────────
┌ prevsibling(idx::AbstractTrees.IndexNode) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/indexing.jl:137
│ no matching method found `prevsiblingindex(::Any, ::Any)`: sidx = prevsiblingindex((idx::AbstractTrees.IndexNode).tree::Any, (idx::AbstractTrees.IndexNode).index::Any)
└────────────────────
┌ prevsibling(csr::AbstractTrees.IndexedCursor) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/cursors.jl:234
│ no matching method found `getindex(::Nothing, ::Int64)` (1/2 union split): (AbstractTrees.parent(csr::AbstractTrees.IndexedCursor)::Union{Nothing, AbstractTrees.IndexedCursor})[idx::Int64]
└────────────────────
┌ (::AbstractTrees.var"#17#18")(n::Any) @ AbstractTrees ~/.julia/packages/AbstractTrees/EUx8s/src/iteration.jl:323
│ no matching method found `parent(::Any, ::Any)`: AbstractTrees.parent(getfield(#self#::AbstractTrees.var"#17#18", :tree)::Any, n::Any)
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
