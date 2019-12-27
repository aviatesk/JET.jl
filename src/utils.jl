typeof′(@nospecialize(x)) = typeof(x)
typeof′(::Type{T}) where {T} = T # XXX: too ad-hoc
typeof′(x::Const) = typeof′(x.val)
typeof′(x::Some) = typeof′(x.value) # maybe no longer needed in the future
typeof′(x::SomeType) = x.type
