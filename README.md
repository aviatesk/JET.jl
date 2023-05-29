# JET.jl

[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://aviatesk.github.io/JET.jl/stable/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
![CI](https://github.com/aviatesk/JET.jl/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)

JET employs Julia's type inference system to detect potential bugs and type instabilities.

:bangbang:
    JET is tightly coupled to the Julia compiler, and so each JET release supports a limited range of Julia versions. See the `Project.toml` file for the range of supported Julia versions. The Julia package manager should install a version of JET compatible with the Julia version you are running.
    If you want to use JET on unreleased version of Julia where compatibility with JET is yet unknown, clone this git repository and `dev` it, such that Julia compatibility is ignored.

:bangbang:
    Also note that the tight coupling of JET and the Julia compiler means that JET results can vary depending on your Julia version.
    In general, the newer your Julia version is, the more accurately and quickly you can expect JET to analyze your code,
    assuming the Julia compiler keeps evolving all the time from now on.

## Quickstart
See more commands, options and explanations in the documentation.

### Detect type instability with `@report_opt`
Type instabilities can be detected in function calls using the `@report_opt` macro, which works similar to the `@code_warntype` macro.
Note that, because JET relies on Julia's type inference, if a chain of inference is broken due to dynamic dispatch, then all downstream function calls will be unknown to the compiler, and so JET cannot analyze them.

```julia
julia> @report_opt foldl(+, Any[]; init=0)
═════ 2 possible errors found ═════
┌ @ reduce.jl:198 Base.:(var"#foldl#295")(kw..., _3, op, itr)
│┌ @ reduce.jl:198 Core.kwcall(merge(Base.NamedTuple(), kw), mapfoldl, identity, op, itr)
││┌ @ reduce.jl:175 Base.:(var"#mapfoldl#294")(_8, _3, f, op, itr)
│││┌ @ reduce.jl:175 Base.mapfoldl_impl(f, op, init, itr)
││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
│││││┌ @ reduce.jl:48 v = Base._foldl_impl(op, nt, itr)
││││││┌ @ reduce.jl:58 v = op(init, y[1])
│││││││┌ @ reduce.jl:86 +(acc, x)
││││││││ runtime dispatch detected: +(acc::Int64, x::Any)::Any
│││││││└────────────────
││││││┌ @ reduce.jl:62 v = op(v, y[1])
│││││││┌ @ reduce.jl:86 +(acc, x)
││││││││ runtime dispatch detected: +(acc::Any, x::Any)::Any
│││││││└────────────────
```

### Detect type errors with `@report_call`
This works best on type stable code, so use `@report_opt` liberally before using `@report_call`.
```julia
julia> @report_call foldl(+, Char[])
═════ 2 possible errors found ═════
┌ @ reduce.jl:198 Base.:(var"#foldl#291")(pairs(NamedTuple()), #self#, op, itr)
│┌ @ reduce.jl:198 mapfoldl(identity, op, itr)
││┌ @ reduce.jl:175 Base.:(var"#mapfoldl#290")(Base._InitialValue(), #self#, f, op, itr)
│││┌ @ reduce.jl:175 Base.mapfoldl_impl(f, op, init, itr)
││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
│││││┌ @ reduce.jl:48 v = Base._foldl_impl(op, nt, itr)
││││││┌ @ reduce.jl:62 v = op(v, y[1])
│││││││┌ @ reduce.jl:86 op.rf(acc, x)
││││││││ no matching method found `+(::Char, ::Char)`: (op::Base.BottomRF{typeof(+)}).rf::typeof(+)(acc::Char, x::Char)
│││││││└────────────────
│││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op, itr)
││││││┌ @ reduce.jl:383 Base.reduce_empty_iter(op, itr, Base.IteratorEltype(itr))
│││││││┌ @ reduce.jl:384 Base.reduce_empty(op, eltype(itr))
││││││││┌ @ reduce.jl:360 Base.reduce_empty(op.rf, T)
│││││││││┌ @ reduce.jl:343 zero(T)
││││││││││ no matching method found `zero(::Type{Char})`: zero(T::Type{Char})
│││││││││└─────────────────
```

### Analyze packages with `report_package`
This looks for all method definitions and analyses function calls based on their signatures. Note that this is less accurate than `@report_call`, because the actual input types cannot be known for generic methods.

```julia
julia> using AbstractTrees

julia> report_package(AbstractTrees)
 [ some output elided ]
═════ 4 possible errors found ═════
┌ @ ~/.julia/packages/AbstractTrees/x9S7q/src/base.jl:260 AbstractTrees.collect(Core.apply_type(StableNode, T), ch)
│┌ @ array.jl:647 Base._collect(T, itr, Base.IteratorSize(itr))
││┌ @ array.jl:649 Base._array_for(T, isz, Base._similar_shape(itr, isz))
│││┌ @ array.jl:679 Base._similar_shape(itr, isz)
││││┌ @ array.jl:664 axes(itr)
│││││┌ @ abstractarray.jl:95 size(A)
││││││ no matching method found `size(::Base.HasLength)`: size(A::Base.HasLength)
│││││└───────────────────────
││││┌ @ array.jl:663 length(itr)
│││││ no matching method found `length(::Base.HasLength)`: length(itr::Base.HasLength)
││││└────────────────
┌ @ ~/.julia/packages/AbstractTrees/x9S7q/src/indexing.jl:137 AbstractTrees.idx.tree
│ `AbstractTrees.idx` is not defined
└───────────────────────────────────────────────────────────────────────
┌ @ ~/.julia/packages/AbstractTrees/x9S7q/src/indexing.jl:137 AbstractTrees.idx.index
│ `AbstractTrees.idx` is not defined
└───────────────────────────────────────────────────────────────────────
```

## Limitations
JET explores the functions you call directly as well as their *inferable* callees. However, if the argument types for a call cannot be inferred, JET does not analyze the callee. Consequently, a report of `No errors detected` does not imply that your entire codebase is free of errors. To increase the confidence in JET's results use `@report_opt` to make sure your code is inferrible.

JET integrates with [SnoopCompile](https://github.com/timholy/SnoopCompile.jl), and you can sometimes use SnoopCompile to collect the data to perform more comprehensive analyses. SnoopCompile's limitation is that it only collects data for calls that have not been previously inferred, so you must perform this type of analysis in a fresh session.

See [SnoopCompile's JET-integration documentation](https://timholy.github.io/SnoopCompile.jl/stable/jet/) for further details.

## Acknowledgement

This project started as my undergrad thesis project at Kyoto University, supervised by Prof. Takashi Sakuragawa.
We were heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof), an experimental type understanding/checking tool for Ruby.
The grad thesis about this project is published at <https://github.com/aviatesk/grad-thesis>, but currently, it's only available in Japanese.
