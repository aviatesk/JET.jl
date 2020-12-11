### JET.jl patches

JET.jl overloads some of `Core.Compiler` functions for the native JIT type inference.
This is necessary when JET.jl has to change/discard some logics that are hard-coded within original native function (in other word, when we can't just use `@invoke` against `AbstractInterpreter` to make the inference process suit for JET analysis).

The `.diff` files in this directory keep monkey patches that each overload does.

In order to improve the maintainability of the patches, we do:
- use syntactic hacks (`#=== ... ===#`) to indicate the locations and purposes of each patch
- each overload is directly evaluated in the `Core.Compiler` module so that we don't need to maintain miscellaneous imports
- as such, the overloads are done within `__init__` hook; there are wrapper functions whose name starts with `overload_`  for each overloading and the wrappers are registered to `push_inithook!`
- the docstrings of the wrappers tell the purposes of each overload
