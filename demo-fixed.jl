########
# demo #
########

# fibonacci
# ---------

# cache, cache, cache
function fib(n::T) where {T<:Number}
    cache = Dict(zero(T)=>zero(T), one(T)=>one(T))
    return _fib(n, cache)
end
_fib(n, cache) = if haskey(cache, n)
    cache[n]
else
    cache[n] = _fib(n-1, cache) + _fib(n-2, cache)
end

fib(BigInt(1000))


# language features
# -----------------

# user-defined types
struct Ty{T}
    fld::T
end

function foo(a)
    v = Ty(a)
    return bar(v)
end

# macros will be expanded
@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fld) # typo fixed
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2)
foo('1') # `Char` will be converted to `UInt32`
