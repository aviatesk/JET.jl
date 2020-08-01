## behind the moar for performance ...

TypeProfiler.jl employs Julia's type inference for bug reports.

```julia
julia> fib(n) = n ≤ 2 ? n : fib(n - 1) + fib(n - 2)
fib (generic function with 1 method)

julia> @profile_call fib(100000) # computation explodes otherwise
No errors !
Int64

julia> @profile_call fib(100000.)
No errors !
Float64

julia> @profile_call fib("100000") # report errors
1 errors found
┌ @ none:1 within `fib`
│┌ @ operators.jl:317 <=(x, y) = (x < y) | (x == y)
││┌ @ operators.jl:268 <(x, y) = isless(x, y)
│││ no method matching signature: isless(::String, ::Int64)
││└
TypeProfiler.Unknown

julia> @profile_call sum("julia") # can find errors that would happen in "deeper" calls
3 errors found
┌ @ reduce.jl:500 sum(a) = sum(identity, a)
│┌ @ reduce.jl:483 sum(f, a) = mapreduce(f, add_sum, a)
││┌ @ reduce.jl:280 mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
│││┌ @ reduce.jl:280 mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
││││┌ @ reduce.jl:157 mapfoldl(f, op, itr; kw...) = mapfoldl_impl(f, op, kw.data, itr)
│││││┌ @ reduce.jl:157 mapfoldl(f, op, itr; kw...) = mapfoldl_impl(f, op, kw.data, itr)
││││││┌ @ reduce.jl:41 return foldl_impl(op′, nt, itr′)
│││││││┌ @ reduce.jl:45 v = _foldl_impl(op, get(nt, :init, _InitialValue()), itr)
││││││││┌ @ namedtuple.jl:271 get(nt::NamedTuple, key::Union{Integer, Symbol}, default) = haskey(nt, key) ? getfield(nt, key) : default
│││││││││ invalid builtin function call: getfield(::NamedTuple{(),Tuple{}}, ::Symbol)
││││││││└
││││││││┌ @ reduce.jl:59 v = op(v, y[1])
│││││││││┌ @ reduce.jl:78 @inline (op::BottomRF)(acc, x) = op.rf(acc, x)
││││││││││┌ @ reduce.jl:21 add_sum(x, y) = x + y
│││││││││││ no method matching signature: +(::Char, ::Char)
││││││││││└
│││││││┌ @ reduce.jl:46 v isa _InitialValue && return reduce_empty_iter(op, itr)
││││││││┌ @ reduce.jl:343 @inline reduce_empty_iter(op, itr) = reduce_empty_iter(op, itr, IteratorEltype(itr))
│││││││││┌ @ reduce.jl:344 @inline reduce_empty_iter(op, itr, ::HasEltype) = reduce_empty(op, eltype(itr))
││││││││││┌ @ reduce.jl:317 reduce_empty(op::BottomRF, T) = reduce_empty(op.rf, T)
│││││││││││┌ @ reduce.jl:310 reduce_empty(::typeof(add_sum), T) = reduce_empty(+, T)
││││││││││││┌ @ reduce.jl:303 reduce_empty(::typeof(+), T) = zero(T)
│││││││││││││ no method matching signature: zero(::Type{Char})
││││││││││││└
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
