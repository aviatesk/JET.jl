typeof′(@nospecialize(x)) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}
typeof′(x::Const) = typeof′(x.val)
typeof′(x::Some) = typeof′(x.value) # maybe no longer needed in the future
typeof′(x::SomeType) = x.type

strip_const(@nospecialize(t)) = t
strip_const(c::Const) = typeof′(c.val)

unwrap_sometype(@nospecialize(x)) = x
unwrap_sometype(x::SomeType) = x.type

# extract call arg types from `FrameData.locals`
function signature_type(frame::Frame)
  callargs = filter(!isnothing, frame.framedata.locals)
  return Tuple{typeof′.(callargs)...}
end
