![CI](https://github.com/aviatesk/JET.jl/workflows/CI/badge.svg)
[![codecov](https://codecov.io/gh/aviatesk/JET.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aviatesk/JET.jl)

## behind the moar for performance ...

JET.jl employs Julia's type inference for bug reports.

!!! note
    JET.jl needs Julia versions 1.6 and higher;
    as such I recommend you give a try on this package with [using nightly version](https://julialang.org/downloads/nightlies/)
    or [building Julia from source](https://github.com/JuliaLang/julia). \
    Also note that JET deeply relies on the type inference routine implemented in [Julia's compiler](https://github.com/JuliaLang/julia/tree/master/base/compiler),
    and so JET analysis result can vary depending on your Julia version.
    In general, the newer your Julia version is, your can expect JET to analyze your code more accurately and quickly,
    because Julia's compiler is rapidly-advancing, literally day by day.


### demo

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

# user-defined types
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
```

You can have JET.jl detect possible errors:

```julia
julia> using JET

julia> profile_and_watch_file("demo.jl"; annotate_types = true)
[toplevel-info] analysis entered into demo.jl
[toplevel-info] analysis from demo.jl finished in 3.455 sec
═════ 4 possible errors found ═════
┌ @ demo.jl:10 fib(m)
│ variable m is not defined: fib(m)
└──────────────
┌ @ demo.jl:11 fib("1000")
│┌ @ demo.jl:7 ≤(n::String, 2)
││┌ @ operators.jl:385 Base.<(x::String, y::Int64)
│││┌ @ operators.jl:336 Base.isless(x::String, y::Int64)
││││ no matching method found for call signature: Base.isless(x::String, y::Int64)
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
│││ no matching method found for call signature: convert(Number, Base.getproperty(v::Ty{String}, :fld::Symbol)::String)
││└──────────────
```

Hooray !
JET.jl found possible error points (e.g. `MethodError: no method matching isless(::String, ::Int64)`) given toplevel call signatures of generic functions (e.g. `fib("1000")`).

Note that JET can find these errors while demo.jl is so inefficient (especially the `fib` implementation) that it would never terminate in actual execution.
That is possible because JET analyzes code only on _type level_.
This technique is often called "abstract interpretation" and JET internally uses Julia's native type inference implementation, so it can analyze code as fast/correctly as Julia's code generation.

Lastly let's apply the following diff to demo.jl so that it works nicely:

> fix-demo.jl.diff

```diff
diff --git a/demo.jl b/demo-fixed.jl
index d2b188a..1d1b3da 100644
--- a/demo.jl
+++ b/demo.jl
@@ -5,11 +5,21 @@
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
@@ -27,8 +37,8 @@ end

 # macros will be expanded
 @inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
-@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
+@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fld) # typo fixed
 @inline bar(v::Ty)                      = bar(convert(Number, v.fld))

 foo(1.2)
-foo("1") # `String` can't be converted to `Number`
+foo('1') # `Char` will be converted to `UInt32`
```

If you apply the diff (i.e. update and save the demo.jl), JET will automatically re-trigger profiling, and this time, won't complain anything:

> `git apply fix-demo.jl.diff`

```julia
[toplevel-info] analysis entered into demo.jl
[toplevel-info] analysis from demo.jl finished in 3.423 sec
No errors !
```


### TODOs

- documentation: WIP at <https://github.com/aviatesk/JET.jl/pull/127>
- release: see <https://github.com/aviatesk/JET.jl/issues/121>
- remove `Core.eval(CC, ...)` monkey patches (<https://github.com/JuliaLang/julia/pull/39439>, <https://github.com/JuliaLang/julia/pull/39305>, see also "developer note" section)
- more accurate error reports in general
  * enable constant propagation on union-split signatures (<https://github.com/JuliaLang/julia/pull/39305>)
- provide editor/IDE integrations for "watch" mode (<https://github.com/aviatesk/JET.jl/pull/85> will be a starting point)
- support package profiling (issue <https://github.com/aviatesk/JET.jl/issues/76>, something like PR <https://github.com/aviatesk/JET.jl/pull/101> can be a starting point)
- implement a "global" version of type inference algorithm (see [this comment](https://github.com/aviatesk/JET.jl/blob/80fd0d9586a01dda7e460ccd71d3d9f33189d4ba/src/abstractinterpretation.jl#L517-L523))
- performance linting (report performance pitfalls, i.e. report an error when there're too many methods matched)
- ideally, I want to extend JET.jl to provide some of LSPs other than diagnostics, e.g. providers of completions, rename refactor, etc.


### developer note

JET.jl overloads functions from Juila's `Core.Compiler` module, which are intended for its native JIT type inference.

They're overloaded on `JETInterpreter` so that `typeinf(::JETInterpreter, ::InferenceState)` will do abstract interpretation tuned for JET.jl's type error analysis.
Most overloads are done by using `invoke`, which allows us to call down to and reuse the original `NativeInterpreter`'s abstract interpretation methods while passing `JETInterpreter` for subsequent (maybe overloaded) callees (see `@invoke` macro).

But sometimes we can't just use `@invoke` and have to change/discard some logics that are hard-coded within original native function.
In such cases, currently JET.jl copy-and-pasted the original body of the overloaded function and applies monkey patches.
I'm planning to remove those monkey patches by adding some tweaks to Julia's compiler code itself, but for now, in order to keep the least maintainability, we do:
- use syntactic hacks (`#=== ... ===#`) to indicate the locations and purposes of each patch
- each overload is directly evaluated in the `Core.Compiler` module so that we don't need to maintain miscellaneous imports
- as such, the overloads are done within `__init__` hook; there are wrapper functions whose name starts with `overload_`  for each overloading and the wrappers are registered to `push_inithook!`
- the docstrings of the wrappers tell the purposes of each overload


### acknowledgement

This project started as my grad thesis project at Kyoto University, supervised by Prof. Takashi Sakuragawa.
We were heavily inspired by [ruby/typeprof](https://github.com/ruby/typeprof), an experimental type understanding/checking tool for Ruby.
The grad thesis about this project is published at <https://github.com/aviatesk/grad-thesis>, but currently it's only available in Japanese.
