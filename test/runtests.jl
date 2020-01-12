using TypeProfiler
# using Test

@profile_call sum("julia")

fib(n) = n <= 2 ? n : fib(n - 1) + fib(n - 2)
@profile_call fib(100) # never ends otherwise

fib′(n) = n <= 2 ? n : fib′′(n - 1) + fib′(n′ - 2)
@profile_call fib′(100) # never ends otherwise
