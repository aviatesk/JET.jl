# JET.jl demonstration
# ====================

# JET can find simple errors:

fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)

fib(1000)   # => never terminates
fib(m)      # => ERROR: UndefVarError: `m` not defined
fib("1000") # => ERROR: MethodError: no method matching isless(::String, ::Int64)

# JET supports all Julia language features:

# it supports user-defined types and functions
struct Ty{T}
    fld::T
end
function foo(a)
    v = Ty(a)
    return bar(v)
end

# it can analyze code with macros
@inline bar(n::T)     where {T<:Number} = n < 0 ? zero(T) : one(T)
@inline bar(v::Ty{T}) where {T<:Number} = bar(v.fdl) # typo "fdl"
@inline bar(v::Ty)                      = bar(convert(Number, v.fld))

foo(1.2) # => ERROR: type Ty has no field fdl
foo("1") # => ERROR: MethodError: Cannot `convert` an object of type String to an object of type Number

# even staged code can be analyzed
# (adapted from https://github.com/JuliaLang/julia/blob/9f665c19e076ab37cbca2d0cc99283b82e99c26f/base/namedtuple.jl#L253-L264)
@generated function badmerge(a::NamedTuple{an}, b::NamedTuple{bn}) where {an, bn}
    names = Base.merge_names(an, bn)
    types = Base.merge_types(names, a, b)
    vals = Any[ :(getfield($(Base.sym_in(names[n], bn) ? :b : :a), $(names[n]))) for n in 1:length(names) ] # missing quote, just ends up with under vars
    :( NamedTuple{$names,$types}(($(vals...),)) )
end

badmerge((x=1,y=2), (y=3,z=1)) # => ERROR: UndefVarError: `x` not defined
