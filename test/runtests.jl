using TypeProfiler

# global ref
# ----------

foo(a) = return bar(a)
@profile_call foo(nothing)


# old
# ---

@profile_call sum("julia")
@profile_call sum([])

fib(n) = n <= 2 ? n : fib(n - 1) + fib(n - 2)
@profile_call fib(100) # never ends otherwise

fib′(n) = n <= 2 ? n : fib′′(n - 1) + fib′(n′ - 2)
@profile_call fib′(100) # never ends otherwise
