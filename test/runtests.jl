using TypeProfiler

# undef var
# ---------

undef1(a) = return foo(a)
@profile_call undef1(0)

undef2(a) = @isdefined(b) && return a # will be lowered to `Expr(:isdefined)`
@profile_call undef2(0)


# non-boolean condition
# ---------------------

nonbool(a) = a ? a : nothing
nonbool() = (c = rand(Any[1,2,3])) ? c #=c is Any typed=# : nothing

@profile_call nonbool(1) # report
@profile_call nonbool(true) # not report
@profile_call nonbool() # not report because it's untyped


# no matching method
# ------------------

# single match
@profile_call sum("julia")
@profile_call sum(Char[])
@profile_call sum([]) # the actual error (i.e. no method for `zero(Any)`) is buriled in the "Too many methods matched" heuristic

# union splitting
nomethod_partial(a) = sin(a)
TypeProfiler.profile_call(Tuple{typeof(nomethod_partial), Union{Int,Char}})

# old
# ---

fib(n) = n <= 2 ? n : fib(n - 1) + fib(n - 2)
@profile_call fib(1000) # never ends otherwise

fib′(n) = n <= 2 ? n : fib′′(n - 1) + fib′(n′ - 2)
@profile_call fib′(1000) # never ends otherwise
