## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.

!!! note
    TypeProfiler.jl needs Julia v1.6, especially [this commit](https://github.com/JuliaLang/julia/commit/d5cf73ffffbab40ae06cc1ec99cac9d8e3d2b6a2);
    as such I recommend you give a try on this package with Julia built from source after the commit.

```julia
julia> using TypeProfiler

julia> fib(n) = n ≤ 2 ? n : fib(n - 1) + fib(n - 2)
fib (generic function with 1 method)

julia> @profile_call fib(100000) # computation explodes otherwise
No errors !
 Int64

julia> @profile_call fib(100000.)
No errors !
 Float64

julia> @profile_call fib("100000") # report errors
1 error found
┌ @ none:1 within `fib(n) in Main at none:1`
│┌ @ operators.jl:326 <=(x, y) = (x < y) | (x == y)
││┌ @ operators.jl:277 <(x, y) = isless(x, y)
│││ no matching method found for signature: isless(::String, ::Int64)
││└
 Union{}

julia> @profile_call sum("julia")
2 errors found
┌ @ reduce.jl:528 sum(a; kw...) = sum(identity, a; kw...)
│┌ @ reduce.jl:528 sum(a; kw...) = sum(identity, a; kw...)
││┌ @ reduce.jl:501 sum(f, a; kw...) = mapreduce(f, add_sum, a; kw...)
│││┌ @ reduce.jl:501 sum(f, a; kw...) = mapreduce(f, add_sum, a; kw...)
││││┌ @ reduce.jl:287 mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
│││││┌ @ reduce.jl:287 mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
││││││┌ @ reduce.jl:160 mapfoldl(f, op, itr; init=_InitialValue()) = mapfoldl_impl(f, op, init, itr)
│││││││┌ @ reduce.jl:160 mapfoldl(f, op, itr; init=_InitialValue()) = mapfoldl_impl(f, op, init, itr)
││││││││┌ @ reduce.jl:42 function mapfoldl_impl(f::F, op::OP, nt, itr) where {F,OP}
│││││││││┌ @ reduce.jl:47 function foldl_impl(op::OP, nt, itr) where {OP}
││││││││││┌ @ reduce.jl:53 function _foldl_impl(op::OP, init, itr) where {OP}
│││││││││││┌ @ reduce.jl:81 @inline (op::BottomRF)(acc, x) = op.rf(acc, x)
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Char, ::Char)
││││││││││││└
││││││││││┌ @ reduce.jl:354 @inline reduce_empty_iter(op, itr) = reduce_empty_iter(op, itr, IteratorEltype(itr))
│││││││││││┌ @ reduce.jl:355 @inline reduce_empty_iter(op, itr, ::HasEltype) = reduce_empty(op, eltype(itr))
││││││││││││┌ @ reduce.jl:328 reduce_empty(op::BottomRF, ::Type{T}) where {T} = reduce_empty(op.rf, T)
│││││││││││││┌ @ reduce.jl:320 reduce_empty(::typeof(add_sum), ::Type{T}) where {T} = reduce_empty(+, T)
││││││││││││││┌ @ reduce.jl:311 reduce_empty(::typeof(+), ::Type{T}) where {T} = zero(T)
│││││││││││││││ no matching method found for signature: zero(::Type{Char})
││││││││││││││└
 Char

julia> ary = Union{Symbol,Int}[:one, 1]

julia> @profile_call sum(ary) # can profile on abstract-typed inputs
14 errors found
┌ @ reducedim.jl:867 @inline ($fname)(a::AbstractArray; dims=:, kw...) = ($_fname)(a, dims; kw...)
│┌ @ reducedim.jl:867 @inline ($fname)(a::AbstractArray; dims=:, kw...) = ($_fname)(a, dims; kw...)
││┌ @ reducedim.jl:871 ($_fname)(a, ::Colon; kw...) = ($_fname)(identity, a, :; kw...)
│││┌ @ reducedim.jl:871 ($_fname)(a, ::Colon; kw...) = ($_fname)(identity, a, :; kw...)
││││┌ @ reducedim.jl:872 ($_fname)(f, a, ::Colon; kw...) = mapreduce(f, $op, a; kw...)
│││││┌ @ reducedim.jl:872 ($_fname)(f, a, ::Colon; kw...) = mapreduce(f, $op, a; kw...)
││││││┌ @ reducedim.jl:310 mapreduce(f, op, A::AbstractArrayOrBroadcasted; dims=:, init=_InitialValue()) =
│││││││┌ @ reducedim.jl:310 mapreduce(f, op, A::AbstractArrayOrBroadcasted; dims=:, init=_InitialValue()) =
││││││││┌ @ reducedim.jl:318 _mapreduce_dim(f, op, ::_InitialValue, A::AbstractArrayOrBroadcasted, ::Colon) =
│││││││││┌ @ reduce.jl:396 function _mapreduce(f, op, ::IndexLinear, A::AbstractArrayOrBroadcasted)
││││││││││┌ @ reduce.jl:351 mapreduce_empty_iter(f, op, itr, ItrEltype) =
│││││││││││┌ @ reduce.jl:355 @inline reduce_empty_iter(op, itr, ::HasEltype) = reduce_empty(op, eltype(itr))
││││││││││││┌ @ reduce.jl:329 reduce_empty(op::MappingRF, ::Type{T}) where {T} = mapreduce_empty(op.f, op.rf, T)
│││││││││││││┌ @ reduce.jl:343 mapreduce_empty(::typeof(identity), op, T) = reduce_empty(op, T)
││││││││││││││┌ @ reduce.jl:320 reduce_empty(::typeof(add_sum), ::Type{T}) where {T} = reduce_empty(+, T)
│││││││││││││││┌ @ reduce.jl:311 reduce_empty(::typeof(+), ::Type{T}) where {T} = zero(T)
││││││││││││││││ no matching method found for signature: zero(::Type{Union{Int64, Symbol}})
│││││││││││││││└
││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││ no matching method found for signature: +(::Symbol, ::Int64)
││││││││││└
││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││└
││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││ no matching method found for signature: +(::Symbol, ::Symbol)
││││││││││└
││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││└
││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││└
││││││││││┌ @ reduce.jl:257 mapreduce_impl(f, op, A::AbstractArrayOrBroadcasted, ifirst::Integer, ilast::Integer) =
│││││││││││┌ @ reduce.jl:233 @noinline function mapreduce_impl(f, op, A::AbstractArrayOrBroadcasted,
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Symbol, ::Int64)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Symbol, ::Symbol)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Symbol, ::Int64)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Int64, ::Symbol)
││││││││││││└
││││││││││││┌ @ reduce.jl:24 add_sum(x, y) = x + y
│││││││││││││ no matching method found for signature: +(::Symbol, ::Symbol)
││││││││││││└
 Union{Int64, Symbol}
```

### TODOs

in order of priority:
- setup a type-level virtual machine and enable profiling on toplevel code
- more reports
  * invalid built-in function calls
  * report some cases of `throw`, e.g. `rand('1')::ArgumentError("Sampler for this object is not defined")`
- balance between Julia's inference approach and error profiling ?
  - Julia's type inference allows abstract type (like `Any`) to slip into the inference process by various heuristics, in order to ensure its termination and obtain the performance
  - but this is somewhat unideal in the context of bug reports, since the stance would be _"better safe than sorry"_, meaning we ideally want to find all the possible errors while revealing some uncertainty Julia's inference accepts
- maybe we need some additional setup for supporting generated functions
- report performance pitfalls ?
