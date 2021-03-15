# JET doesn't conretize this by default, but just analyze its type
const GLOBAL_CODE_STORE = Dict()

macro with_code_record(a)
    GLOBAL_CODE_STORE[__source__] = a # record the code location in the global store
    esc(a)
end

# here JET will try to actually expand `@with_code_record`,
# but since `GLOBAL_CODE_STORE` didn't get concretized (i.e. instantiated), JET analysis fails at this point
@with_code_record foo(a) = identity(a)

foo(10) # top-level callsite, abstracted away
