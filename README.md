## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.

!!! note
    TypeProfiler.jl needs Julia v1.6, especially [this commit](https://github.com/JuliaLang/julia/commit/d5cf73ffffbab40ae06cc1ec99cac9d8e3d2b6a2);
    as such I recommend you give a try on this package with [nighlty](https://julialang.org/downloads/nightlies/)
    or [building from source](https://github.com/JuliaLang/julia) after the commit.


### demo

Say you have this strange and buggy file and want to know where to fix:

> demo.jl

```julia
# fibonacci
# ---------

fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)

fib("1000") # obvious errors
fib(m)      # undef var
fib(1000)   # never terminates in ordinal execution


# language features
# -----------------

struct Ty{T}
    fld::T
end

function foo(a)
    v = Ty(a)
    return bar(v)
end

@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2)
foo("1") # `String` can't be converted to `Number`
```

You can have TypeProfiler.jl detect possible errors:

```julia
julia> using TypeProfiler

julia> profile_file("demo.jl")
6 errors found

┌ @ demo.jl:8 top-level scope
│┌ @ demo.jl:8 Main.fib("1000")
││┌ @ operators.jl:326 Main.≤(n::String, 2)
│││┌ @ operators.jl:277 Base.<(x::String, y::Int64)
││││ no matching method found for signature: Base.isless(x::String, y::Int64)
│││└
│┌ @ demo.jl:8 Main.fib("1000")
││ no matching method found for signature: Main.-(n::String, 1)
│└
│┌ @ demo.jl:8 Main.fib("1000")
││ no matching method found for signature: Main.-(n::String, 2)
│└
┌ @ demo.jl:8 top-level scope
│ variable Main.m is not defined: Main.fib(Main.m)
└
│┌ @ demo.jl:23 Main.foo(1.2)
││┌ @ demo.jl:30 Main.bar(v::Union{})
│││┌ @ Base.jl:33 Base.getproperty(v::Main.Ty{Float64}, :fdl::Symbol)
││││ invalid builtin function call: Base.getfield(x::Main.Ty{Float64}, f::Symbol)
│││└
│┌ @ demo.jl:23 Main.foo("1")
││┌ @ demo.jl:31 Main.bar(v::Union{})
│││ no matching method found for signature: Main.convert(Main.Number, %2::String)
││└
```

Hooray !
TypeProfiler.jl tries to find possible error points (e.g. `MethodError: no method matching isless(::String, ::Int64)`) given toplevel call sigutures of generic functions (e.g. `fib("1000")`).

Note that TypeProfiler.jl can find these errors while demo.jl is so ridiculous (especially the `fib(1000)` part) that it never terminates in actual execution.
This is because TypeProfiler.jl profiles code only on _type level_.
This technique is often called "abstract interpretation" and TypeProfiler.jl internally uses Julia's native abstract interpreter implementation (for its JIT compiling), so it can profile code as fast/correctly as our Julia's code generation.

Lastly let's modify demo.jl so that it works nicely:

> demo.jl

```julia
# fibonacci
# ---------

# cache, cache, cache
function fib(n::T) where {T<:Number}
    cache = Dict(zero(T)=>zero(T), one(T)=>one(T))
    return _fib(n, cache)
end
_fib(n, cache) = if haskey(cache, n)
    cache[n]
else
    cache[n] = _fib(n-1, cache) + _fib(n-2, cache)
end

fib(BigInt(1000))


# language features
# -----------------

struct Ty{T}
    fld::T
end

function foo(a)
    v = Ty(a)
    return bar(v)
end

@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fld) # typo fixed
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2)
foo('1') # `Char` will be converted to `UInt32`
```

Now TP won't complain anything:

```julia
julia> profile_file("_demo_fixed.jl")
No errors !
```


### TODOs

- watch mode
- handle `import/using`, `include` etc.
- more reports
  * more correct error reports in general
  * report some cases of `throw`, e.g. `rand('1')::ArgumentError("Sampler for this object is not defined")`
- balance between Julia's inference approach and error profiling ?
  - Julia's type inference allows abstract type (like `Any`) to slip into the inference process by various heuristics, in order to ensure its termination and obtain the performance
  - but this is somewhat unideal in the context of bug reports, since the stance would be _"better safe than sorry"_, meaning we ideally want to find all the possible errors while revealing some uncertainty Julia's inference accepts
- report performance pitfalls ?
