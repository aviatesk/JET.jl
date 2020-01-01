# lookups
# -------

"""
    typ = @lookup_type(frame, node)
    typ = @lookup_type(mod, frame, node)

Looks up a previously-inferred type referenced as `SSAValue`, `SlotNumber`, `Const`,
  `TypedSlot`, `QuoteNode`, `GlobalRef`, or `Symbol` (in a scope of `moduleof(frame)`
  or the optional first argument `mod` module).
If none of the above apply, the value of `node` will be returned.
"""
macro lookup_type(args...)
  length(args) === 2 || length(args) === 3 || error("invalid number of arguments ", length(args))
  havemod = length(args) === 3
  local mod
  if havemod
    mod, frame, node = args
  else
    frame, node = args
  end
  nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
  fallback = havemod ?
    :($nodetmp isa Symbol ? typeof′(getfield($(esc(mod)), $nodetmp)) : typeof′($nodetmp)) :
    :(typeof′($nodetmp))

  quote
    $nodetmp = $(esc(node))
    ret =
      isa($nodetmp, SSAValue) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, SlotNumber) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, TypedSlot) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, QuoteNode) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, GlobalRef) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, Symbol) ? lookup_type($(esc(frame)), $nodetmp) :
      isa($nodetmp, Expr) ? lookup_type($(esc(frame)), $nodetmp) :
      $fallback
    isa(ret, Const) ? typeof′(ret.val) : ret
  end
end

lookup_type(frame::Frame, ssav::SSAValue) = frame.framecode.src.ssavaluetypes[ssav.id]
lookup_type(frame::Frame, slot::SlotNumber) = frame.framecode.src.slottypes[slot.id]
lookup_type(frame::Frame, tslot::TypedSlot) = slot.typ
lookup_type(frame::Frame, node::QuoteNode) = typeof′(node.value)
function lookup_type(frame::Frame, ref::GlobalRef)
  isdefined(ref.mod, ref.name) || return Undefined
  return typeof′(getfield(ref.mod, ref.name))
end
function lookup_type(frame::Frame, sym::Symbol)
  mod = moduleof(frame)
  isdefined(mod, sym) || return Undefiend
  return typeof′(getfield(mod, sym))
end
function lookup_type(frame::Frame, e::Expr)
  head = e.head
  if head === :the_exception
    @error "exceptions are not supported"
    return Undefined
  elseif head === :static_parameter
    arg = e.args[1]::Int
    if isassigned(frame.framedata.sparams, arg)
      return Type{frame.framedata.sparams[arg]}
    else
      syms = sparam_syms(frame.framecode.scope)
      throw(UndefVarError(syms[arg]))
    end
  elseif head === :boundscheck
    if length(e.args) === 0
      return Bool
    else
      error("invalid boundscheck at ", e)
    end
  else
    error("invalid lookup expr ", e)
  end
end

"""
    val = @lookup_value(frame, node)
    val = @lookup_value(mod, frame, node)

Looks up a value referenced as `SSAValues`, `SlotNumbers`, `Const`, `QuoteNode`,
  `GlobalRef` or `Symbol` (in a scope of `moduleof(frame)` or the optional first
  argument `mod` module).
If none of the above apply, the value of `node` will be returned.
"""
macro lookup_value(args...)
  length(args) === 2 || length(args) === 3 || error("invalid number of arguments ", length(args))
  havemod = length(args) === 3
  local mod
  if havemod
    mod, frame, node = args
  else
    frame, node = args
  end
  nodetmp = gensym(:node)  # used to hoist, e.g., args[4]
  fallback = havemod ?
    :($nodetmp isa Symbol ? getfield($(esc(mod)), $nodetmp) : $nodetmp) :
    :($nodetmp)

  quote
    $nodetmp = $(esc(node))
    ret =
      isa($nodetmp, SSAValue) ? lookup_value($(esc(frame)), $nodetmp) :
      isa($nodetmp, SlotNumber) ? lookup_value($(esc(frame)), $nodetmp) :
      isa($nodetmp, QuoteNode) ? lookup_value($(esc(frame)), $nodetmp) :
      isa($nodetmp, GlobalRef) ? lookup_value($(esc(frame)), $nodetmp) :
      isa($nodetmp, Symbol) ? lookup_value($(esc(frame)), $nodetmp) :
      $fallback
    isa(ret, Const) ? ret.val : ret
  end
end

lookup_value(frame::Frame, ssav::SSAValue) = frame.framecode.src.ssavaluetypes[ssav.id]
lookup_value(frame::Frame, slot::SlotNumber) = frame.framecode.src.slottypes[slot.id]
lookup_value(frame::Frame, node::QuoteNode) = node.value
function lookup_value(frame::Frame, ref::GlobalRef)
  isdefined(ref.mod, ref.name) || return Undefined()
  getfield(ref.mod, ref.name)
end
function lookup_value(frame::Frame, sym::Symbol)
  mod = moduleof(frame)
  isdefined(mod, sym) || return Undefined()
  return getfield(mod, sym)
end

# TODO: :foreigncall should be special cased
"""
    collect_call_arg_types(frame::Frame, call_expr::Expr; isfc::Bool = false)

Looks up for the types of function call arguments in `call_expr`, while reusing
  the already allocated array in `frame` (i.e. `frame.framedata.callargs`).
"""
function collect_call_arg_types(frame::Frame, call_expr::Expr; isfc::Bool = false)
  mod = moduleof(frame)
  arg_types = frame.framedata.callargs
  resize!(arg_types, length(call_expr.args))

  for (i, arg) in enumerate(call_expr.args)
    arg_types[i] = @lookup_type(mod, frame, arg)
  end

  return arg_types
end

# types
# -----

typeof′(@nospecialize(x)) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}
typeof′(x::Const) = typeof′(x.val)
typeof′(x::Some) = typeof′(x.value) # maybe no longer needed in the future
typeof′(x::SomeType) = x.type

unwrap_sometype(@nospecialize(x)) = x
unwrap_sometype(x::SomeType) = x.type

# extract call arg types from `FrameData.locals`
function signature_type(frame::Frame)
  callargs = filter(!isnothing, frame.framedata.locals)
  return Tuple{typeof′.(callargs)...}
end
