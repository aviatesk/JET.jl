[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://aviatesk.github.io/JET.jl/stable/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
![CI](https://github.com/aviatesk/JET.jl/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)

### behind the moar for performance ...

JET.jl employs Julia's type inference for bug reports.

!!! note
    The latest version of JET requires Julia versions **1.7 and higher**;
    JET is tested against [the current stable release](https://julialang.org/downloads/#current_stable_release) as well as [nightly version](https://julialang.org/downloads/nightlies/). \
    Also note that JET deeply relies on the type inference routine implemented in [the Julia compiler](https://github.com/JuliaLang/julia/tree/master/base/compiler),
    and so the analysis result can vary depending on your Julia version.
    In general, the newer your Julia version is, more accurately and quickly your can expect JET to analyze your code,
    assuming the Julia compiler keeps evolving all the time from now on.

!!! tip
    Hold on tight and fasten your seatbelt while JET is analyzing your code for the first time.
    Once the caches get accumulated, subsequent analysis will run at ✈ speed.


## Demo

Say you have this strange and buggy file and want to know where to fix:

> demo.jl

```julia
# demo
# ====

# fibonacci
# ---------

fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)

fib(1000)   # never terminates in ordinal execution
fib(m)      # undef var
fib("1000") # obvious type error


# language features
# -----------------

# user-defined types, macros
struct Ty{T}
    fld::T
end

function foo(a)
    v = Ty(a)
    return bar(v)
end

# macros will be expanded
@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2)
foo("1") # `String` can't be converted to `Number`

# even staged programming
# adapted from https://github.com/JuliaLang/julia/blob/9f665c19e076ab37cbca2d0cc99283b82e99c26f/base/namedtuple.jl#L253-L264
@generated function badmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
    names = Base.merge_names(an, bn)
    types = Base.merge_types(names, a, b)
    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(names[n]))) for n in 1:length(names) ] # missing quote, just ends up with under vars
    :( NamedTuple{$names,$types}(($(vals...),)) )
end

badmerge((x=1,y=2), (y=3,z=1))
```

You can have JET.jl detect possible errors:

```julia
julia> using JET

julia> report_and_watch_file("demo.jl"; annotate_types = true)
[toplevel-info] virtualized the context of Main (took 0.013 sec)
[toplevel-info] entered into demo.jl
[toplevel-info]  exited from demo.jl (took 3.254 sec)
═════ 7 possible errors found ═════
┌ @ demo.jl:10 fib(m)
│ variable m is not defined: fib(m)
└──────────────
┌ @ demo.jl:11 fib("1000")
│┌ @ demo.jl:7 ≤(n::String, 2)
││┌ @ operators.jl:401 Base.<(x::String, y::Int64)
│││┌ @ operators.jl:352 Base.isless(x::String, y::Int64)
││││ no matching method found for call signature (Tuple{typeof(isless), String, Int64}): Base.isless(x::String, y::Int64)
│││└────────────────────
┌ @ demo.jl:32 foo(1.2)
│┌ @ demo.jl:24 bar(v::Ty{Float64})
││┌ @ demo.jl:29 Base.getproperty(v::Ty{Float64}, :fdl::Symbol)
│││┌ @ Base.jl:33 Base.getfield(x::Ty{Float64}, f::Symbol)
││││ type Ty{Float64} has no field fdl
│││└──────────────
┌ @ demo.jl:33 foo("1")
│┌ @ demo.jl:24 bar(v::Ty{String})
││┌ @ demo.jl:30 convert(Number, Base.getproperty(v::Ty{String}, :fld::Symbol)::String)
│││ no matching method found for call signature (Tuple{typeof(convert), Type{Number}, String}): convert(Number, Base.getproperty(v::Ty{String}, :fld::Symbol)::String)
││└──────────────
┌ @ demo.jl:44 badmerge(Core.apply_type(Core.NamedTuple, Core.tuple(:x::Symbol, :y::Symbol)::Tuple{Symbol, Symbol})::Type{NamedTuple{(:x, :y)}}(Core.tuple(1, 2)::Tuple{Int64, Int64})::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, Core.apply_type(Core.NamedTuple, Core.tuple(:y::Symbol, :z::Symbol)::Tuple{Symbol, Symbol})::Type{NamedTuple{(:y, :z)}}(Core.tuple(3, 1)::Tuple{Int64, Int64})::NamedTuple{(:y, :z), Tuple{Int64, Int64}})
│┌ @ demo.jl:37 getfield(a::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, x)
││ variable x is not defined: getfield(a::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, x)
│└──────────────
│┌ @ demo.jl:37 getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, y)
││ variable y is not defined: getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, y)
│└──────────────
│┌ @ demo.jl:37 getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, z)
││ variable z is not defined: getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, z)
│└──────────────
```

Hooray !
JET.jl found possible error points (e.g. `MethodError: no method matching isless(::String, ::Int64)`) given toplevel call signatures of generic functions (e.g. `fib("1000")`).

Note that JET can find these errors while demo.jl is so inefficient (especially the `fib` implementation) that it would never terminate in actual execution.
That is possible because JET analyzes code only on _type level_.
This technique is often called "abstract interpretation" and JET internally uses Julia's native type inference implementation, so it can analyze code as fast/correctly as Julia's code generation.

Lastly let's apply the following diff to demo.jl so that it works nicely:

> fix-demo.jl.diff

```diff
diff --git a/demo.jl b/demo.jl
index f868d2f..634e130 100644
--- a/demo.jl
+++ b/demo.jl
@@ -4,11 +4,21 @@
 # fibonacci
 # ---------

-fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)
+# cache, cache, cache
+function fib(n::T) where {T<:Number}
+    cache = Dict(zero(T)=>zero(T), one(T)=>one(T))
+    return _fib(n, cache)
+end
+_fib(n, cache) = if haskey(cache, n)
+    cache[n]
+else
+    cache[n] = _fib(n-1, cache) + _fib(n-2, cache)
+end

-fib(1000)   # never terminates in ordinal execution
-fib(m)      # undef var
-fib("1000") # obvious type error
+fib(BigInt(1000)) # will terminate in ordinal execution as well
+m = 1000          # define m
+fib(m)
+fib(parse(Int, "1000"))


 # language features
@@ -26,19 +36,19 @@ end

 # macros will be expanded
 @inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
-@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
+@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fld) # typo fixed
 @inline bar(v::Ty)                      = bar(convert(Number, v.fld))

 foo(1.2)
-foo("1") # `String` can't be converted to `Number`
+foo('1') # `Char` will be converted to `UInt32`

 # even staged programming
 # adapted from https://github.com/JuliaLang/julia/blob/9f665c19e076ab37cbca2d0cc99283b82e99c26f/base/namedtuple.jl#L253-L264
-@generated function badmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
+@generated function goodmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
     names = Base.merge_names(an, bn)
     types = Base.merge_types(names, a, b)
-    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(names[n]))) for n in 1:length(names) ] # missing quote, just ends up with under vars
+    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(QuoteNode(names[n])))) for n in 1:length(names) ] # names quoted, should work as expected
     :( NamedTuple{$names,$types}(($(vals...),)) )
 end

-badmerge((x=1,y=2), (y=3,z=1))
+goodmerge((x=1,y=2), (y=3,z=1))
```

If you apply the diff (i.e. update and save the demo.jl), JET will automatically re-trigger analysis, and this time, won't complain anything:

> `git apply fix-demo.jl.diff`

```julia
[toplevel-info] virtualized the context of Main (took 0.004 sec)
[toplevel-info] entered into demo.jl
[toplevel-info]  exited from demo.jl (took 3.061 sec)
No errors !
```


## Roadmap

- **more accuracy**: abstract interpretation can be improved yet more
  * switch to new type lattice design: to simplify the addition of further improvements
  * design and introduce a more proper backward-analysis pass, e.g. effect and escape analysis: to enable further constraint propagation
- **more performance**: JET should be much faster
  * aggregate similar errors more smartly and aggressively
  * introduce parallelism to Julia's native type inference routine
- **better documentation**: especially JET needs a specification of its error reports
- **editor/IDE integration**: GUI would definitely be more appropriate for showing JET's analysis result
  * **smarter code dependency tracking**: the watch mode currently re-analyzes the whole code on each update, which is the most robust and least efficient option. When integrated with an IDE, fancier incremental analysis based on smarter code dependency tracking like what [Revise.jl](https://github.com/timholy/Revise.jl) does would be needed
  * **LSP support**: ideally I hope to extend JET to provide some of LSP features other than diagnostics, e.g. auto-completions, rename refactor, taking type-level information into account


## Acknowledgement

This project started as my undergrad thesis project at Kyoto University, supervised by Prof. Takashi Sakuragawa.
We were heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof), an experimental type understanding/checking tool for Ruby.
The grad thesis about this project is published at <https://github.com/aviatesk/grad-thesis>, but currently it's only available in Japanese.
