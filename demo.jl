# demo
# ====

# fibonacci
# ---------

fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)

fib(1000)   # never terminates in ordinal execution
fib(m)      # undef var
fib("1000") # obvious type error


# language features
# -----------------

# user-defined types, macros
struct Ty{T}
    fld::T
end

function foo(a)
    v = Ty(a)
    return bar(v)
end

# macros will be expanded
@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2)
foo("1") # `String` can't be converted to `Number`

# even staged programming
# adapted from https://github.com/JuliaLang/julia/blob/9f665c19e076ab37cbca2d0cc99283b82e99c26f/base/namedtuple.jl#L253-L264
@generated function badmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
    names = Base.merge_names(an, bn)
    types = Base.merge_types(names, a, b)
    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(names[n]))) for n in 1:length(names) ] # missing quote, just ends up with under vars
    :( NamedTuple{$names,$types}(($(vals...),)) )
end

badmerge((x=1,y=2), (y=3,z=1))
