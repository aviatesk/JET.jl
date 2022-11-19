# Errors kinds and how to fix them

## `no matching method found`
### Description
This error occurs when running the code might throw a `MethodError` at runtime.
Similar to normal `MethodErrors`, this happens if a function is being called without a method matching the given argument types.

This is the most common error detected in most Julia code.

### Example
```@repl
using JET # hide
f(x::Integer) = x + one(x);
g(x) = f(x);
@report_call g(1.0)
```

### How to fix
This error indicates some kind of type error in your code. You fix it like you would fix a regular `MethodError` thrown at runtime.

## no matching method found (`x`/`y` union split)
### Description
This error occurs when a variable `x` is inferred to be a union type, and `x` being one or more of the union's members would lead to a `MethodError`. For example, if the compiler infers `x` to be of type `Union{A, B}`, and then a function `f(x)` is called which would lead to a `MethodError` if `x` is a `A`, this error would occur.

More technically, this happens when one or more branches created by the compiler through union splitting contains a `no matching method found` error.

### Example
Minimal example:
```@repl union1
using JET # hide
struct Foo
    x::Union{Int, String}
end

# Errors if x.x isa String.
# The compiler doesn't know if it's a String or Int
f(x) = x.x + 1;

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

### How to fix
This error is unique in that idiomatic Julia code may still lead to this error. For example, in the `pos_after_tab` function above, if the input vector does not have a `'\t'` byte, `p` will be `nothing`, and a `MethodError` will be thrown when `nothing + 1` is attempted.
However, in many situations, the possibility of such a `MethodError` is not a mistake, but rather an idiomatic way of erroring.

There are different possiblities to address this kind of error. Let's take the `pos_after_tab` example:

If you actually _could_ expect `p` to legitimately be `nothing` for valid input (i.e. the input could lack a `'\t'` byte), then your function should be written to take this edge case into account:
```@repl union
using JET # hide
function pos_after_tab(v::AbstractArray{UInt8})
    p = findfirst(isequal(UInt8('\t')), v)
    if p === nothing # handle the nothing case
        return nothing
    else
        return p + 1
    end
end;
@report_call pos_after_tab(codeunits("a\tb"))
```

By adding the `if p === nothing` check, the compiler will know that the type of `p` must be `Nothing` inside the `if` block, and `Int` in the `else` block. This way, the compiler knows a `MethodError` is not possible, and the error will disappear.

If you expect a `'\t'` byte to always be present, such that `findfirst` always should return an `Int` for valid input, you can add a typeassert in the function to assert that the return value of `findfirst` must be, say, an `Integer`. Then, the compiler will know that if the typeassert passes, the value returned by `findfirst` cannot be `nothing` (and hence in this case must be `Int`):

```@repl union
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

```@repl union2
using JET # hide
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

@report_call f(Foo(1))
```

We might reasonably expect the compiler to know that in the `else` branch, `x.x` must be an `Int`, since it just checked that it is not `nothing`. However, the compiler does not know that the value obtained from loading the `x` field in the expression `x.x` on the like with the if statement in this case is the same value as the value obtained when loading the `x` field in the `x.x + 1` statement.
You can solve this issue by assigning `x.x` to a variable:

```@repl union2
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

## `X` is not defined
### Description
This happens when a name `X` is used in a function, but no object named `X` can be found.

### Example
```@repl defined
using JET # hide
f(x) = foo(x) + 1;

@report_call f(1)
```

### How to fix
This error can have a couple of causes:
* `X` is misspelled. If so, correct the typo
* `X` exists, but cannot be reached from the scope of the function.
  If so, pass it in as an argument to the offending function.

## type `T` has no field `F`
### Description
This error occurs when `Core.getfield` is (indirectly) called with a nonexisting and hardcoded field name. For example, if an object have a field called `vec` and you type it `vector`.
### Example
```@repl field
using JET # hide
struct Foo
    my_field
end
f(x) = x.my_feild; # NB: Typo!
@report_call f(Foo(1))
```

### How to fix
This error often occurs when the field name is mistyped. Correct the typo.

## BoundsError: Attempt to access `T` at index `[i]`
### Description
This error occurs when it is known at compile time that the call will throw a `BoundsError`.
Note that most `BoundsErrors` cannot be predicted at compile time. For the compiler to know a function attempts to access a container out of bounds, both the container length and the index value must be known at compiletime. Hence, the error is detected for a `Tuple` input in the example below, but not for a `Vector` input.

### Example
```@repl
using JET # hide
get_fourth(x) = x[4]
@report_call get_fourth((1,2,3))
@report_call get_fourth([1,2,3]) # NB: False negative!
```

### How to fix
If this error appears, the offending code uses a bad index. Since the error most often occurs when the index is hardcoded, simply fix the index value.

## may throw
### Description
This error indicates that JET detected the possibility of an exception.
By default, JET will not report this error, unless a function is inferred to _always_ throw, AND the exception is not caught in a try statement. In "sound" mode, this error is reported if the function _may_ throw.

### Example
In this example, the function is known at compile time to throw an uncaught exception, and so is reported by default:
```@repl
using JET # hide
f(x) = x isa Integer ? throw("Integer") : nothing;
@report_call f(1)
```

In this example, it's not known at compile time whether it throws, and therefore, JET reports no errors by default. In sound mode, the error is reported.
```@repl
using JET # hide
f(x) = x == 9873984732 ? nothing : throw("Bad value")
@report_call f(1)
@report_call mode=:sound f(1)
```

In this example, the exception is handled, so JET reports no errors by default. In sound mode, the error is reported:
```@repl
using JET # hide
g() = throw();
f() = try
    g()
catch
    nothing
end;
f()
@report_call f()
@report_call mode=:sound f()
```
