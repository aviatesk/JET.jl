## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.

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
```

### TODOs

in order of priority:

- [x] show profiled results
- [x] escape recursive calls
- [ ] bug fixes: `TypeVar` etc. can serious errors within the current implementation
- [ ] toplevel executions
  - handle untyped IRs while splitting expressions as JuliaIntepreter.jl does
  - then, TP will be able to profile a file directly like an usual static linter
- [ ] more reports
  - [ ] `UndefVarError`
    - [x] report `GlobalRef` for really undefined variable
    - report `:throw_undef_if_not` ? (includes lots of false positives as is)
  - [ ] method ambiguity error
  - more and more ...
- [ ] don't trace into "primitive" functions in `Core` and `Base`: with the similar approach to https://github.com/FluxML/Mjolnir.jl/tree/9435d98673752cec4e222e31a6b9f38edcd7d5e0/src/lib
- [ ] balance between Julia's inference approach and error profiling
  - Julia's type inference allows abstract type (like `Any`) to slip into the inference process by various heuristics, in order to ensure its termination and obtain the performance
  - but this is obviously unideal for TP, since our basic stance is _"better safe than sorry"_, meaning ideally we want to find all the possible errors while revealing some uncertainty Julia's inference accepts
  - nevertheless, as far as TP relies on the Julia's inference, we need to achieve the conservative error profiling in the existence of abstract types, _somehow_
  - as a consequence, TP will be able to profile, e.g.:
    - [ ] `print`
    - [ ] `sort`
- [ ] support generated functions
- [ ] replace `Core` types: enables profiling things in `Core.Compiler` module

### Ideas

- report performance pitfalls
- somehow profiles possible exceptions ?
