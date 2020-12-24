### JET.jl patches

JET.jl overloads functions from Juila's `Core.Compiler` module, which are intended for its native JIT type inference.

They're overloaded on `JETInterpreter` so that `typeinf(::JETInterpreter, ::InferenceState)` will do abstract interpretation tuned for JET.jl's type error analysis.
Most overloads are done by using `invoke`, which allows us to call down to and reuse the original `NativeInterpreter`'s abstract interpretation methods while passing `JETInterpreter` for subsequent (maybe overloaded) callees (see `@invoke` macro).

But sometimes we can't just use `@invoke` and have to change/discard some logics that are hard-coded within original native function.
In such cases, currently JET.jl copy-and-pasted the original body of the overloaded function and applies monkey patches.
The `.diff` files in this directory keep those (horrible) monkey patches (mostly for the reference).
In order to keep the least maintainability, we do:
- use syntactic hacks (`#=== ... ===#`) to indicate the locations and purposes of each patch
- each overload is directly evaluated in the `Core.Compiler` module so that we don't need to maintain miscellaneous imports
- as such, the overloads are done within `__init__` hook; there are wrapper functions whose name starts with `overload_`  for each overloading and the wrappers are registered to `push_inithook!`
- the docstrings of the wrappers tell the purposes of each overload
