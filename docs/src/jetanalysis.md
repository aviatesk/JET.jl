# [Error Analysis](@id jetanalysis)

Julia's type system is quite expressive and its type inference is strong enough to generate
fairly optimized code from a highly generic program written in a concise syntax.
But as opposed to other statically-compiled languages, Julia by design does _not_ error nor
warn anything even if it detects possible errors during its compilation process no matter
how serious they are. In essence, Julia achieves highly generic and composable programming
by delaying all the errors and warnings to the runtime.

This is a core design choice of the language. On the one hand, Julia's dynamism
allows it to work in places where data types can not be fully decided ahead of runtime
(e.g. when the program is duck-typed with generic pieces of code, or when the program
consumes some data that is only known at runtime). On the other hand, with Julia, it's
not straightforward to have such modern development experiences that a static language can
typically offer, as like static type checking and rich IDE features.

JET is a trial to get the best of both worlds: can we have a sufficiently useful static
checking without losing all the beauty of Julia's dynamism and composability?
JET's approach is very different from
["gradual typing"](https://en.wikipedia.org/wiki/Gradual_typing),
that is a common technique to bring static analysis into a dynamic language, as used for
e.g. [mypy](https://github.com/python/mypy) for Python and
[TypeScript](https://www.typescriptlang.org/) for JavaScript.
Rather, JET's static analysis is powered by Julia's builtin type inference system, that
is based on a technique called
["abstract interpretation"](https://en.wikipedia.org/wiki/Abstract_interpretation).
This way JET can analyze just a normal Julia program and smartly detect possible
errors statically, without requiring any additional setups like scattering type annotations
just for the sake of analysis but preserving original polymorphism and composability of
the program, as effectively as the Julia compiler can optimize your Julia program.

## [Quick Start](@id jetanalysis-quick-start)

First of all, you need to install JET: JET is an ordinary Julia package, so you can install it via
Julia's built-in package manager and use it as like other packages.
```@repl quickstart
; # ] add JET # install JET via the built-in package manager

using JET
```

Let's start with the simplest example: how JET can find anything wrong with `sum("julia")`?
[`@report_call`](@ref) and [`report_call`](@ref) analyzes a given function call and report
back possible problems. They can be used in a similar way as
[`@code_typed`](https://docs.julialang.org/en/v1/stdlib/InteractiveUtils/#InteractiveUtils.@code_typed)
and [`code_typed`](https://docs.julialang.org/en/v1/base/base/#Base.code_typed).
Those [interactive entry points](@ref jetanalysis-interactive-entry) are the easiest
way to use JET:
```@repl quickstart
@report_call sum("julia")
```

So JET found two possible problems. Now let's see how they can occur in _actual execution_:
```@repl quickstart
sum("julia") # will lead to `MethodError: +(::Char, ::Char)`
sum("") # will lead to `MethodError: zero(Type{Char})`
```

We should note that `@report_call sum("julia")` could detect both of those two different
errors that can happen at runtime. This is because `@report_call` does a static analysis —
it analyzes the function call in a way that does not rely on one instance of runtime
execution, but rather it reasons about all the possible executions!
This is one of the biggest advantages of static analysis because other alternatives to
check software qualities like "testing" usually rely on _some_ runtime execution and they
can only cover a subset of all the possible executions.

As mentioned above, JET is designed to work with _just a normal_ Julia program.
Let's define new arbitrary functions and run JET on it:
```@repl quickstart
function foo(s0)
    a = []
    for s in split(s0)
        push!(a, bar(s))
    end
    return sum(a)
end

bar(s::String) = parse(Int, s)

@report_call foo("1 2 3")
```

Now let's fix this problematic code.
First, we can fix the definition of `bar` so that it accepts generic `AbstractString` input.
JET's analysis result can be dynamically updated when we refine a function definition,
and so we just need to add a new `bar(::AbstractString)` definition.
As for the second error, let's assume, for some reason, we're not interested in fixing it and
we want to ignore errors that may happen within `Base`. Then we can use the
[`target_modules`](@ref result-config) configuration to limit the analysis scope to
the current module context to ignore the possible error that may happen within `sum(a)`[^1].

[^1]: We used `target_modules` just for the sake of demonstration. To make it more
      idiomatic, we should initialize `a` as typed vector `a = Int[]`, and then we won't
      get any problem from `sum(a)` even without the `target_modules` configuration.

```@repl quickstart
# hot fix the definition of `bar`
bar(s::AbstractString) = parse(Int, s)

# now no errors should be reported !
@report_call target_modules=(@__MODULE__,) foo("1 2 3")
```

So far, we have used the default error analysis pass, which collects problems according to
one specific (somewhat opinionated) definition of "errors" (see the [`JET.BasicPass`](@ref) for more details).
JET offers other error reporting passes, including the "sound" error detection ([`JET.SoundPass`](@ref))
as well as the simpler "typo" detection pass ([`JET.TypoPass`](@ref))[^2].
They can be switched using the `mode` configuration:

[^2]: JET offers the framework to define your own abstract interpretation-based analysis.
      See [`AbstractAnalyzer`-Framework](@ref) if interested.

```@repl quickstart
function myifelse(cond, a, b)
    if cond
        return a
    else
        return b
    end
end

# the default analysis pass doesn't report "non-boolean (T) used in boolean context" error
# as far as there is a possibility when the condition "can" be bool (NOTE: Bool <: Integer)
report_call(myifelse, (Integer, Int, Int))

# the sound analyzer doesn't permit such a case: it requires the type of a conditional value to be `Bool` strictly
report_call(myifelse, (Integer, Int, Int); mode=:sound)

function strange_sum(a)
    if rand(Bool)
        undefsum(a)
    else
        sum(a)
    end
end

# the default analysis pass will report both problems:
# - `undefsum` is not defined
# - `sum(a::Vector{Any})` can throw when `a` is empty
@report_call strange_sum([])

# the typo detection pass will only report the "typo"
@report_call mode=:typo strange_sum([])
```

We can use [`@test_call`](@ref) and [`test_call`](@ref) to assert that your program is free
from problems that `@report_call` can detect.
They work nicely with [`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/)'s
unit-testing infrastructure:
```@repl quickstart
@test_call target_modules=(@__MODULE__,) foo("1 2 3")

using Test

# we can get the nice summery using `@testset` !
@testset "JET testset" begin
    @test_call target_modules=(@__MODULE__,) foo("1 2 3") # should pass

    test_call(myifelse, (Integer, Int, Int); mode=:sound)

    @test_call broken=true foo("1 2 3") # `broken` and `skip` options are supported

    @test foo("1 2 3") == 6 # of course other `Test` macros can be used in the same place
end
```

JET uses JET itself in its test pipeline: JET's static analysis has been proven to
be very useful and helped its development a lot.
If interested, take a peek at [JET's `"self check"` testset](https://github.com/aviatesk/JET.jl/blob/master/test/self_check.jl).

Lastly, let's see the example that demonstrates JET can analyze a "top-level" program.
The top-level analysis should be considered as a somewhat experimental feature, and at this moment
you may need additional configurations to run it correctly. Please read the descriptions of
[top-level entry points](@ref jetanalysis-toplevel-entry) and choose an appropriate entry point for
your use case. Here we run [`report_file`](@ref) on [demo.jl](https://github.com/aviatesk/JET.jl/blob/master/demo.jl).
It automatically extracts and loads "definitions" of functions, structs and such,
and then analyzes their "usages" statically:
```@repl quickstart
report_file(normpath(Base.pkgdir(JET), "demo.jl"))
```

## Errors kinds and how to fix them

### `no matching method found`
#### Description
This error occurs when running the code might throw a `MethodError` at runtime.
Similar to normal `MethodErrors`, this happens if a function is being called without a method matching the given argument types.

This is the most common error detected in most Julia code.

#### Example
```@repl nomatch1
f(x::Integer) = x + one(x);
g(x) = f(x);

using JET # hide
@report_call g(1.0)
```

#### How to fix
This error indicates some kind of type error in your code. You fix it like you would fix a regular `MethodError` thrown at runtime.

### `no matching method found (x/y union split)`
#### Description
This error occurs when a variable `x` is inferred to be a union type, and `x` being one or more of the union's members would lead to a `MethodError`. For example, if the compiler infers `x` to be of type `Union{A, B}`, and then a function `f(x)` is called which would lead to a `MethodError` if `x` is a `A`, this error would occur.

More technically, this happens when one or more branches created by the compiler through union splitting contains a `no matching method found` error.

#### Example
Minimal example:
```@repl union1
struct Foo
    x::Union{Int, String}
end

# Errors if x.x isa String.
# The compiler doesn't know if it's a String or Int
f(x) = x.x + 1;

using JET # hide
@report_call f(Foo(1))
```

More common example:
```@repl union1
function pos_after_tab(v::AbstractArray{UInt8})
    # findfirst can return `nothing` on no match
    p = findfirst(isequal(UInt8('\t')), v)
    p + 1
end

@report_call pos_after_tab(codeunits("a\tb"))
```

#### How to fix
This error is unique in that idiomatic Julia code may still lead to this error. For example, in the `pos_after_tab` function above, if the input vector does not have a `'\t'` byte, `p` will be `nothing`, and a `MethodError` will be thrown when `nothing + 1` is attempted.
However, in many situations, the possibility of such a `MethodError` is not a mistake, but rather an idiomatic way of erroring.

There are different possibilities to address this kind of error. Let's take the `pos_after_tab` example:

If you actually _could_ expect `p` to legitimately be `nothing` for valid input (i.e. the input could lack a `'\t'` byte), then your function should be written to take this edge case into account:
```@repl union2
function pos_after_tab(v::AbstractArray{UInt8})
    p = findfirst(isequal(UInt8('\t')), v)
    if p === nothing # handle the nothing case
        return nothing
    else
        return p + 1
    end
end;

using JET # hide
@report_call pos_after_tab(codeunits("a\tb"))
```

By adding the `if p === nothing` check, the compiler will know that the type of `p` must be `Nothing` inside the `if` block, and `Int` in the `else` block. This way, the compiler knows a `MethodError` is not possible, and the error will disappear.

If you expect a `'\t'` byte to always be present, such that `findfirst` always should return an `Int` for valid input, you can add a typeassert in the function to assert that the return value of `findfirst` must be, say, an `Integer`. Then, the compiler will know that if the typeassert passes, the value returned by `findfirst` cannot be `nothing` (and hence in this case must be `Int`):

```@repl union2
function pos_after_tab(v::AbstractArray{UInt8})
    p = findfirst(isequal(UInt8('\t')), v)::Integer
    p + 1
end;

@report_call pos_after_tab(codeunits("a\tb"))
```

The code will still error at runtime due to the typeassert if `findfirst` returns `nothing`, but JET will no longer detect it as an error, because the programmer, by adding the typeassert, explicitly acknowledge that the compiler's inference may not be precise enough, and helps the compiler.

Note that adding a typeassert also improves code quality:
* The programmer's intent to never observe `nothing` is communicated clearly
* After the typeassert passes, `p` is inferred to be `Int` instead of a union, and this more precise type inference generates more efficient code.
* More precise inference reduces the risk of invalidations from the code, improving latency.

A special case occurs when loading `Union`-typed fields from structs.
Julia does not realize that loading the same field multiple times from a mutable struct necessarily returns the same object. Hence, in the following example:

```@repl union3
mutable struct Foo
    x::Union{Int, Nothing}
end

function f(x)
    if x.x === nothing
        nothing
    else
        x.x + 1
    end
end;

using JET # hide
@report_call f(Foo(1))
```

We might reasonably expect the compiler to know that in the `else` branch, `x.x` must be an `Int`, since it just checked that it is not `nothing`. However, the compiler does not know that the value obtained from loading the `x` field in the expression `x.x` on the like with the if statement in this case is the same value as the value obtained when loading the `x` field in the `x.x + 1` statement[^3].
You can solve this issue by assigning `x.x` to a variable:

```@repl union3
function f(x)
    y = x.x
    if y === nothing
        nothing
    else
        y + 1
    end
end;

@report_call f(Foo(1))
```

[^3]: For immutable structs, the Julia compiler can figure out type constraints imposed on
      aliased field loads if you're using Julia version higher than `v"1.10.0-DEV.25"`.

### `X is not defined`
#### Description
This happens when a name `X` is used in a function, but no object named `X` can be found.

#### Example
```@repl defined1
f(x) = foo(x) + 1;

using JET # hide
@report_call f(1)
```

#### How to fix
This error can have a couple of causes:
* `X` is misspelled. If so, correct the typo
* `X` exists, but cannot be reached from the scope of the function.
  If so, pass it in as an argument to the offending function.

### `type T has no field F`
#### Description
This error occurs when `Core.getfield` is (indirectly) called with a nonexisting and hardcoded field name. For example, if an object have a field called `vec` and you type it `vector`.
#### Example
```@repl field1
struct Foo
    my_field
end
f(x) = x.my_feild; # NB: Typo!

using JET # hide
@report_call f(Foo(1))
```

#### How to fix
This error often occurs when the field name is mistyped. Correct the typo.

### `BoundsError: Attempt to access T at index [i]`
#### Description
This error occurs when it is known at compile time that the call will throw a `BoundsError`.
Note that most `BoundsErrors` cannot be predicted at compile time. For the compiler to know a function attempts to access a container out of bounds, both the container length and the index value must be known at compiletime. Hence, the error is detected for a `Tuple` input in the example below, but not for a `Vector` input.

#### Example
```@repl bounds1
get_fourth(x) = x[4]

using JET # hide
@report_call get_fourth((1,2,3))
@report_call get_fourth([1,2,3]) # NB: False negative!
```

#### How to fix
If this error appears, the offending code uses a bad index. Since the error most often occurs when the index is hardcoded, simply fix the index value.

### `may throw [...]`
#### Description
This error indicates that JET detected the possibility of an exception.
By default, JET will not report this error, unless a function is inferred to _always_ throw, AND the exception is not caught in a try statement. In "sound" mode, this error is reported if the function _may_ throw.

#### Example
In this example, the function is known at compile time to throw an uncaught exception, and so is reported by default:
```@repl throw1
f(x) = x isa Integer ? throw("Integer") : nothing;

using JET # hide
@report_call f(1)
```

In this example, it's not known at compile time whether it throws, and therefore, JET reports no errors by default. In sound mode, the error is reported.
```@repl throw2
f(x) = x == 9873984732 ? nothing : throw("Bad value")

using JET # hide
@report_call f(1)
@report_call mode=:sound f(1)
```

In this example, the exception is handled, so JET reports no errors by default. In sound mode, the error is reported:
```@repl throw3
g() = throw();
f() = try
    g()
catch
    nothing
end;
f()

using JET # hide
@report_call f()
@report_call mode=:sound f()
```

## [Entry Points](@id jetanalysis-entry)

### [Interactive Entry Points](@id jetanalysis-interactive-entry)

JET offers interactive analysis entry points that can be used similarly to
[`code_typed`](https://docs.julialang.org/en/v1/base/base/#Base.code_typed) and its family:
```@docs
JET.@report_call
JET.report_call
```

### [`Test` Integration](@id jetanalysis-test-integration)

JET also exports entries that are fully integrated with [`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/)'s unit-testing infrastructure.
It can be used in your test suite to assert your program is free from errors that JET can detect:
```@docs
JET.@test_call
JET.test_call
```

### [Top-level Entry Points](@id jetanalysis-toplevel-entry)

JET can also analyze your "top-level" program: it can just take your Julia script or package
and will report possible errors.

Note that JET will analyze your top-level program "half-statically": JET will selectively
interpret and load "definitions" (like a function or struct definition) and try to
simulate Julia's top-level code execution process.
While it tries to avoid executing any other parts of code like function calls and analyzes
them based on abstract interpretation instead (and this is a part where JET statically analyzes your code).
If you're interested in how JET selects "top-level definitions", please see [`JET.virtual_process`](@ref).

!!! warning
    Because JET will interpret "definitions" in your code, that part of top-level analysis
    certainly _runs_ your code. So we should note that JET can cause some side effects from your code;
    for example, JET will try to expand all the macros used in your code, and so the side effects
    involved with macro expansions will also happen in JET's analysis process.

```@docs
JET.report_file
JET.watch_file
JET.report_package
JET.report_text
```


## [Configurations](@id jetanalysis-config)

In addition to the [general configurations](@ref), the error analysis can take the
following specific configurations:
```@docs
JET.JETAnalyzer
JET.BasicPass
JET.SoundPass
JET.TypoPass
```
