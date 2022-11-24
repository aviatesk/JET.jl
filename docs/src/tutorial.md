# JET tutorial
JET leverages the Julia compiler's inference to check user code for type instability and type errors.
This tutorial will demonstrate how to use JET effectively.
It presupposes the reader has working knowledge of Julia, and understands Julian concepts such as "dynamic dispatch" and "type stability" - see the [Julia documentation](https://docs.julialang.org/en/v1/) for explanation of these concepts.

Because JET relies on the compiler's type inference, it is not able to effectively analyze type unstable code.
Making your code type stable is a prerequisite for effectively using JET's type error analysis.
Therefore, we will begin by showing how to use JET to fix type instabilities.

First of all, you need to install JET: JET is an ordinary Julia package, so you can install it via
Julia's built-in package manager and use it as like other packages.
```@repl tutorial
; # ] add JET # install JET via the built-in package manager

using JET
```

## Detecting type instability with `@report_opt`
JET exports a function [`report_opt`](@ref) and the related macro [`@report_opt`](@ref).
It works similar to the function/macro pair `(@)code_warntype` from Base - except that it automatically analyses all the way down the function chain, and that it only displays any issues found.

For example, suppose we have the function:
```@repl tutorial
add_one_first(x) = first(x) + 1;
```

Any type instabilities of a given function call can be analysed thus:
```@repl tutorial
@report_opt add_one_first([1])
@report_opt add_one_first(Any[1])
```

You can see that `add_one_first` is type stable when called with a `Vector{Int}`, but it leads to dynamic dispatch when called with `Vector{Any}`.

Suppose now we have _two levels_ of type instability, where one type instability "hides behind" another type instability, as in this example:
```@repl tutorial
add_one_first(x) = first(x) + 1;
func_var = add_one_first;
f(x) = func_var(x);
@report_opt f(Any[1])
```
The dynamic dispatch we see here come from the fact that `func_var` is an untyped global variable.
Remember that `add_one_first` was also type unstable when called with a `Vector{Any}`. So why doesn't `@report_opt` report that second type instability from calling `add_one_first(::Vector{Any})`?
The reason is that because the Julia compiler does not know at compile time that `func_var` is equal to `add_one_first`, JET cannot "see through" the first type instability and see that `add_one_first(Any[1])` will eventually be called.

If we fix the first instability by defining the global variable `my_func_var` as `const`:
```@repl tutorial
const my_func_var = add_one_first;
```

Then the compiler knows that `add_one_first` will be called, and the second type instability from this function is revealed:
```@repl tutorial
f(x) = my_func_var(x)
@report_opt f(Any[1])
```

Sometimes, type instability only shows up much deeper into a call chain, several functions deep.
This is not a problem for JET.
In the example below, JET sees type instability ~10 function calls deep:
```@repl tutorial
@report_opt sum(Any[1])
```

As mentioned above, effective use of JET begins with liberal use of `@report_opt` to eliminate or reduce any dynamic dispatch, such that the Julia compiler is not blinded by dynamic dispatch.

After the program has been made as type stable as possible, it's time to use `@report_call` to find type errors.

## Analyse methods with `@report_call`
The function/macro pair [`report_call`](@ref) and [`@report_call`](@ref) works just like `(@)report_opt` - but where the latter reports dynamic dispatch, the former finds type errors.

The `@report_call` macro analyses function calls like so:
```@repl tutorial
@report_call sum(['a'])
```

In this example, JET found two possible type errors:
* If the input vector is empty, the function call will error with a `MethodError` after attempting to call `zero(Char)`.
* If the input vector has two or more elements, the call will error after attempting to call `+(::Char, ::Char)`.
Note that these type errors show up even though the input vector had exactly one element, and so neither of these errors would actually occur at runtime if `sum(['a'])` had been executed.
This happens because JET analyses the code on a _type_ level, only looking at the code generated with the input _types_. It does not analyze what will actually happen with the given input value.

Note also that the two possible errors shown are mutally exclusive - no input will lead to both errors.
Nonetheless, JET is able to detect both possibilities, because it analyses all possible branches in the generated function call.

In contrast, if we analyse the same `sum` method on a `Vector{Int}` instead of `Vector{Char}`:
```@repl tutorial
@report_call sum([1])
```

The two errors above do not appear. These errors do not apply to `Vector{Int}`, because `zero(Int)` is well-defined and so is `+(::Int, ::Int)`.
This illustrates that JET does not analyze _methods_, but rather _function calls with specific types_. More precisely, JET analyses so-called _methodinstances_, since methodinstances are compiled whereas methods are not.

## Analyse whole packages with `report_package`
As shown above, _methods_ cannot be analyzed, but only _methodinstances_, i.e. methods plus the types of their arguments.
Most packages, however, define only methods, and do not contain callsites. That is, they do not have any information about the types that these methods will be called with.

However, JET is able to do limited analysis using only the method signature extracted from the method definition.
For example, if I define this simple function:

```julia
first_plus_n(itr, n::Real) = first(itr) + n;
```

, then _at the very least_, we can guarantee that `itr isa Any` and `n isa Real`.
Hence, JET can analyze the methodinstance `first_plus_n(::Any, ::Real)`, using only the method definition.

The JET function [`report_package`](@ref) extracts all method definitions in a package, and using the extracted signatures, runs `report_call` on them.
For example, the package `BioSymbols` can be analysed like this:

```julia
julia> using JET

julia> report_package(BioSymbols)
[ output elided ]
```

Note that `report_package` is less precise than `@report_call`, because method signatures of idiomatic Julia code are often very generic, so there is less type information in the signature itself than there is when given concrete argument types.

## Usage tips
#### Use `@report_opt` before `@report_call`
JET works best on type-stable code.
Iron out type instabilities using `@report_opt` before using `@report_call`

#### Filtering away false postives
It is common to find that JET finds lots of errors in your functions, which all derive from type instability and type issues in your dependencies.
In fact, type issues from dependencies are often so plentiful they flood your analysis with false positives, which can make working with JET harder.

To reduce false positives, you can use the keywords `ignored_modules` and `target_modules`.Both take an iterable of modules.

The former removes any errors that originate from any of the given modules, while the latter removes any errors _except_ ones originating from these modules.

For example, in the REPL (which is in module `Main`), we can define:
```@repl tutorial
g(x) = first(x) + 1
```

This throws in a `Base` function if we pass `nothing` into it:
```@repl tutorial
@report_call g(nothing)
```

Since the error originates from `Base`, we can filter the error away by ignoring `Base`, or equivalently, we may retain only the ones from `Main`. Note that we pass `(Base,)` as a 1-element Tuple of modules:
```@repl tutorial
@report_call ignored_modules=(Base,) g(nothing)
@report_call target_modules=(@__MODULE__,) g(nothing)
```

The `AnyFrameModule` construct can be used to filter for (or against) any error where _any_ of the function calls in the callchain originates from the given module.
For example, in the example above, the function call begins in `Main` and ends in `Base`, so the callchain includes both modules.
Ignoring `AnyFrameModule(Base)` _or_ `AnyFrameModule(Main)` will then ignore the error:

```@repl tutorial
@report_call ignored_modules=(AnyFrameModule(Base),) g(nothing)
@report_call ignored_modules=(AnyFrameModule(@__MODULE__),) g(nothing)
```

Similarly, the error would be retained if `target_modules` would have been `AnyFrameModule(Base)` _or_ `AnyFrameModule(Main)`.

Beware that this filtering may filter away legitimate problems in your package. In the example above, if your code calls `f(nothing)`, the error may originate from Base, but it's clearly an error in your own code to call `f(nothing)`.
So it is recommended to only filter away modules if they produce so many false positives that it makes using JET difficult.

#### Analyze scripts and apps by using a `main` function
Scripts and apps called from the command line have a single logical entry point. If you wrap the logic in a `main` function, JET can analyse the entire script.

For example, suppose you made this command-line script which added two numbers from the command line:

```julia
a = parse(Int, first(ARGS))
b = parse(Int, last(ARGS))
println(a + b)
```

You could rewrite this as:
```julia
function main()
    a = parse(Int, first(ARGS))
    b = parse(Int, last(ARGS))
    println(a + b)
end

main()
```

, and then analyse the script with JET using `@report_call main()`.
Using a `main` function has the further advantage that it makes it for other people to understand what your script does when invoked.

Alternatively, JET also provides the function [`report_file`](@ref), which you can call like:
```julia
julia> report_file("my_script.jl")
```

, which will be equivalent to checking `@report_call main()`, if the script contains a `main()` call at top level.

#### Analyze packages using a representative workload
As shown above, packages can be analysed with `report_package`.
However, generic type signatures often used in packages lead to imprecise inference and thus imprecise analysis.

To improve analysis, you can create a file `src/workload.jl` in your package, which uses all (or most) functionality of the pacakge.
The function could look like:

```julia
function exercise_mypkg()
    data = MyPkg.load_data()
    transformed = MyPkg.transform_data(data)
    # [ etc ...]
end
```

Because such usage necessarily requires passing concrete types to your functions, calling `@report_call exercise_mypkg()` leads to more precise analysis than `report_package`.

Furthermore, once you have written a function like `exercise_mypkg`, you can use a package like [`SnoopPrecompile`](https://github.com/timholy/SnoopCompile.jl) to precompile the function, which will thus precompile all code exercised in the function, significantly reducing your package's latency.
