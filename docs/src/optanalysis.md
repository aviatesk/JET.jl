# [Optimization Analysis](@id optanalysis)

Successful type inference and optimization is key to high-performing Julia programs.
But as mentioned in [the performance tips](https://docs.julialang.org/en/v1/manual/performance-tips/), there are some
chances where Julia can not infer the types of your program very well and can not optimize it well accordingly.

While there are many possibilities of "type-instabilities", like usage of non-constant global variable most notably,
probably the most tricky one would be ["captured variable"](https://docs.julialang.org/en/v1/manual/performance-tips/#man-performance-captured)
– Julia can not really well infer the type of variable that is observed and modified by both inner function and enclosing one.
And such type instabilities can lead to various optimization failures. One of the most common barrier to the performance
is known as "runtime dispatch", which happens when a matching method can't be resolved by the compiler due to the lack
of type information and it is looked up at runtime instead. Since runtime dispatch is caused by poor type information,
it often indicates the compiler could not do other optimizations including inlining and scalar replacements of aggregates.

In order to avoid such problems, we usually inspect output of [`code_typed`](https://docs.julialang.org/en/v1/base/base/#Base.code_typed)
or its family, and check if there is anywhere type is not well inferred and optimization was not successful.
But the problem is that one needs to have enough knowledge about the inference and optimization in order to interpret
the output. Another problem is that they can only present the "final" output of the inference and optimization, and we
can not inspect the entire call graph and may miss to find where a problem actually happened and how the type-instability
has been propagated.
There is a nice package called [Cthulhu.jl](https://github.com/JuliaDebug/Cthulhu.jl), which allows us to look at
the outputs of `code_typed` by _descending_ into a call tree, recursively and interactively. The workflow with Cthulhu
is much more efficient and powerful, but still, it requires much familiarity with Julia compiler and it tends to be tedious.

So, why not automate it ?
JET implements such an analyzer that investigates optimized representation of your program and _automatically_ detects
anywhere the compiler failed in optimization. Especially, it can find where Julia creates captured variables, where
runtime dispatch will happen, and where Julia gives up the optimization work due to unresolvable recursive function call.


## [Quick Start](@id optanalysis-quick-start)

JET exports [`@report_opt`](@ref), which analyzes the entire call graph of a given generic function call,
and then reports detected performance pitfalls:
```@repl quickstart
using JET
```

As a first example, let's see how we can find and fix runtime dispatches using JET:
```@repl quickstart
n = rand(Int); # non-constant global variable
make_vals(n) = n ≥ 0 ? (zero(n):n) : (n:zero(n));
function sumup(f)
    # this function uses the non-constant global variable `n` here
    # and it makes every succeeding operations type-unstable
    vals = make_vals(n)
    s = zero(eltype(vals))
    for v in vals
        s += f(v)
    end
    return s
end;
@report_opt sumup(sin) # runtime dispatches will be reported
```

JET's analysis result will be dynamically updated when we (re-)define functions[^1], and we can "hot-fix" the runtime
dispatches within the same running Julia session like this:
```@repl quickstart
# we can pass parameters as a function argument instead, and then everything will be type-stable
function sumup(f, n)
    vals = make_vals(n)
    s = zero(eltype(vals))
    for v in vals
        # NOTE here we may get union type like `s::Union{Int,Float64}`,
        # but Julia can optimize away such small unions (thus no runtime dispatch)
        s += f(v)
    end
    return s
end;

@report_opt sumup(sin, rand(Int)) # now runtime dispatch free !
```
[^1]: Technically, it's fully integrated with [Julia's method invalidation system](https://julialang.org/blog/2020/08/invalidations/).

`@report_opt` can also report existence of captured variables, which are really better to be eliminated within
performance-sensitive context:
```@repl quickstart
# the examples below are all adapted from https://docs.julialang.org/en/v1/manual/performance-tips/#man-performance-captured
function abmult(r::Int)
    if r < 0
        r = -r
    end
    # the closure assigned to `f` make the variable `r` captured
    f = x -> x * r
    return f
end;
@report_opt abmult(42)

function abmult(r0::Int)
    # we can improve the type stability of the variable `r` like this,
    # but it is still captured
    r::Int = r0
    if r < 0
        r = -r
    end
    f = x -> x * r
    return f
end;
@report_opt abmult(42)

function abmult(r::Int)
    if r < 0
        r = -r
    end
    # we can try to eliminate the capturing
    # and now this function would be most high-performing
    f = let r = r
        x -> x * r
    end
    return f
end;
@report_opt abmult(42)
```

With the [`target_modules`](@ref result-config) configuration, we can easily limit the analysis scope to a specific module context:
```@repl quickstart
# problem: when ∑1/n exceeds `x` ?
function compute(x)
    r = 1
    s = 0.0
    n = 1
    @time while r < x
        s += 1/n
        if s ≥ r
            # `println` call is full of runtime dispatches for good reasons
            # and we're not interested in type-instabilities within this call
            # since we know it's only called few times
            println("round $r/$x has been finished")
            r += 1
        end
        n += 1
    end
    return n, s
end

@report_opt compute(30) # bunch of reports will be reported from the `println` call

@report_opt target_modules=(@__MODULE__,) compute(30) # focus on what we wrote, and no error should be reported
```

There is also [`function_filter`](@ref optanalysis-config), which can ignore specific function call.

[`@test_opt`](@ref) can be used to assert that a given function call is free from the performance pitfalls.
It is fully integrated with [`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/)'s unit-testing infrastructure,
and we can use it as like other `Test` macros e.g. `@test`:
```@repl quickstart
@test_opt sumup(cos)

@test_opt target_modules=(@__MODULE__,) compute(30)

using Test

@testset "check type-stabilities" begin
    @test_opt sumup(cos) # should fail

    n = rand(Int)
    @test_opt sumup(cos, n) # should pass

    @test_opt target_modules=(@__MODULE__,) compute(30) # should pass

    @test_opt broken=true compute(30) # should pass with the "broken" annotation
end
```


## [Entry Points](@id optanalysis-entry)

### [Interactive Entry Points](@id optanalysis-interactive-entry)

The optimization analysis offers interactive entry points that can be used in the same way as [`@report_call`](@ref) and [`report_call`](@ref):
```@docs
JET.@report_opt
JET.report_opt
```

### [`Test` Integration](@id optanalysis-test-integration)

As with [the default error analysis](@ref jetanalysis), the optimization analysis also offers the integration with
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/):
```@docs
JET.@test_opt
JET.test_opt
```

### [Top-level Entry Points](@id optanalysis-toplevel-entry)

By default, JET doesn't offer top-level entry points for the optimization analysis, because it's usually used for only a
selective portion of your program.
But if you want you can just use [`report_file`](@ref) or similar top-level entry points with specifying
`analyzer = OptAnalyzer` configuration in order to apply the optimization analysis on top-level script,
e.g. `report_file("path/to/file.jl"; analyzer = OptAnalyzer)`.


## [Configurations](@id optanalysis-config)

In addition to [general configurations](@ref JET-configurations), the optimization analysis can take the following specific configurations:
```@docs
JET.OptAnalyzer
```
