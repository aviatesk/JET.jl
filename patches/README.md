### JET.jl patches

Some of JET.jl's overloads include entire body of original functions for the type inference for native JIT routine.
This is necessary when JET.jl has to change some logics that are hard-coded within the original function (in other word, when we can't just use `@invoke` against `AbstractInterpreter` to make the inference process suit for JET analysis).

The `.diff` files in this directory keep monkey patches that each overloading does.

In order to improve the maintainability of the patches, we do:
- use syntactic hacks (`#=== ... ===#`) in order to keep the diff from the original code consisting of only additions, so that the future changes in the native compiler can be easily applied to the overloaded version
- directly evaluate the overloads into `Core.Compiler` module so that we don't need to maintain miscellaneous imports
