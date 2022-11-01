# JET.jl

[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://aviatesk.github.io/JET.jl/stable/)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://aviatesk.github.io/JET.jl/dev/)
![CI](https://github.com/aviatesk/JET.jl/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)

JET.jl employs Julia's type inference system to detect potential bugs.

!!! note
    The latest version of JET requires Julia versions **1.8** and higher;
    JET is tested against [the current stable release](https://julialang.org/downloads/#current_stable_release) as well as [nightly version](https://julialang.org/downloads/nightlies/). \
    Also note that JET deeply relies on the type inference routine implemented in [the Julia compiler](https://github.com/JuliaLang/julia/tree/master/base/compiler),
    and so the analysis result can vary depending on your Julia version.
    In general, the newer your Julia version is, the more accurately and quickly you can expect JET to analyze your code,
    assuming the Julia compiler keeps evolving all the time from now on.

!!! tip
    Hold on tight and fasten your seatbelt while JET is analyzing your code for the first time.
    Once the caches get accumulated, subsequent analysis will run at ✈ speed.


## Demo

Say you have this strange and buggy file and want to know where to fix:

> demo.jl

```julia
# JET.jl demonstration
# ====================

# JET can find simple errors:

fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)

fib(1000)   # => never terminates
fib(m)      # => ERROR: UndefVarError: `m` not defined
fib("1000") # => ERROR: MethodError: no method matching isless(::String, ::Int64)

# JET supports all Julia language features:

# it supports user-defined types and functions
struct Ty{T}
    fld::T
end
function foo(a)
    v = Ty(a)
    return bar(v)
end

# it can analyze code with macros
@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2) # => ERROR: type Ty has no field fdl
foo("1") # => ERROR: MethodError: Cannot `convert` an object of type String to an object of type Number

# even staged code can be analyzed
# (adapted from https://github.com/JuliaLang/julia/blob/9f665c19e076ab37cbca2d0cc99283b82e99c26f/base/namedtuple.jl#L253-L264)
@generated function badmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
    names = Base.merge_names(an, bn)
    types = Base.merge_types(names, a, b)
    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(names[n]))) for n in 1:length(names) ] # missing quote, just ends up with under vars
    :( NamedTuple{$names,$types}(($(vals...),)) )
end

badmerge((x=1,y=2), (y=3,z=1)) # => ERROR: UndefVarError: `x` not defined
```

You can have JET.jl detect possible errors:

```julia
julia> using JET

julia> report_and_watch_file("demo.jl"; annotate_types=true)
[toplevel-info] virtualized the context of Main (took 0.184 sec)
[toplevel-info] entered into demo.jl
[toplevel-info]  exited from demo.jl (took 7.523 sec)
┌ @ demo.jl:9 fib(m)
│ `m` is not defined
└─────────────
┌ @ demo.jl:10 fib("1000")
│┌ @ demo.jl:6 n::String :≤ 2
││┌ @ operators.jl:392 x::String < y::Int64
│││┌ @ operators.jl:343 isless(x::String, y::Int64)
││││ no matching method found `isless(::String, ::Int64)`: isless(x::String, y::Int64)
│││└────────────────────
┌ @ demo.jl:28 foo(1.2)
│┌ @ demo.jl:20 bar(v)
││┌ @ demo.jl:25 (v::Ty{Float64}).fdl
│││┌ @ Base.jl:37 Base.getfield(x::Ty{Float64}, f::Symbol)
││││ type Ty{Float64} has no field fdl
│││└──────────────
┌ @ demo.jl:29 foo("1")
│┌ @ demo.jl:20 bar(v)
││┌ @ demo.jl:26 convert(Number, (v::Ty{String}).fld::String)
│││ no matching method found `convert(::Type{Number}, ::String)`: convert(Number, (v::Ty{String}).fld::String)
││└──────────────
┌ @ demo.jl:40 badmerge(NamedTuple{(:x, :y)}(tuple(1, 2)::Tuple{Int64, Int64})::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, NamedTuple{(:y, :z)}(tuple(3, 1)::Tuple{Int64, Int64})::NamedTuple{(:y, :z), Tuple{Int64, Int64}})
│┌ @ demo.jl:33 getfield(a::NamedTuple{(:x, :y), Tuple{Int64, Int64}}, x)
││ `x` is not defined
│└──────────────
│┌ @ demo.jl:33 getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, y)
││ `y` is not defined
│└──────────────
│┌ @ demo.jl:33 getfield(b::NamedTuple{(:y, :z), Tuple{Int64, Int64}}, z)
││ `z` is not defined
│└──────────────
```

Hooray!
JET.jl found possible error points (e.g. `MethodError: no method matching isless(::String, ::Int64)`) given top-level call signatures of generic functions (e.g. `fib("1000")`).

Note that JET can find these errors while demo.jl is so inefficient (especially the `fib` implementation) that it would never terminate in actual execution.
That is possible because JET analyzes code only on _type level_.
This technique is often called "abstract interpretation" and JET internally uses Julia's native type inference implementation, so it can analyze code as fast/correctly as Julia's code generation.

Lastly, let's apply the following diff to demo.jl so that it works nicely:

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
[toplevel-info] virtualized the context of Main (took 0.002 sec)
[toplevel-info] entered into demo.jl
[toplevel-info]  exited from demo.jl (took 1.004 sec)
No errors detected
```


## Limitations

JET explores the functions you call directly as well as their *inferrable* callees. However, if the argument types for a call cannot be inferred, JET does not analyze the callee. Consequently, a report of `No errors detected` does not imply that your entire codebase is free of errors.

JET integrates with [SnoopCompile](https://github.com/timholy/SnoopCompile.jl), and you can sometimes use SnoopCompile to collect the data to perform more comprehensive analyses. SnoopCompile's limitation is that it only collects data for calls that have not been previously inferred, so you must perform this type of analysis in a fresh session.

See [SnoopCompile's JET-integration documentation](https://timholy.github.io/SnoopCompile.jl/stable/jet/) for further details.


## Acknowledgement

This project started as my undergrad thesis project at Kyoto University, supervised by Prof. Takashi Sakuragawa.
We were heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof), an experimental type understanding/checking tool for Ruby.
The grad thesis about this project is published at <https://github.com/aviatesk/grad-thesis>, but currently, it's only available in Japanese.
