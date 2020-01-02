# lookups
# -------

"""
    typ = lookup_type(frame::Frame, x)

Looks up a previously-inferred type referenced as `SSAValue`, `SlotNumber`,
  `Const`, `TypedSlot`, `QuoteNode`, `GlobalRef`, or `Symbol` for a binding in
  a scope of `moduleof(frame)`.
If none of the above apply, returns the direct type of `x`.
"""
function lookup_type(frame::Frame, @nospecialize(x))
  ret = _lookup_type(frame, x)
  # ensure all `Const` are stripped
  return ret isa Const ? lookup_type(frame, ret.val) : ret
end
_lookup_type(frame::Frame, @nospecialize(x)) = typeof′(x) # fallback case8f
_lookup_type(frame::Frame, ssav::SSAValue) = frame.framecode.src.ssavaluetypes[ssav.id]
_lookup_type(frame::Frame, slot::SlotNumber) = frame.framecode.src.slottypes[slot.id]
_lookup_type(frame::Frame, c::Const) = _lookup_type(frame, c.val) # cascade to its value
_lookup_type(frame::Frame, tslot::TypedSlot) = tslot.typ
_lookup_type(frame::Frame, node::QuoteNode) = typeof′(node.value)
function _lookup_type(frame::Frame, ref::GlobalRef)
  isdefined(ref.mod, ref.name) || return Undefined
  return typeof′(getfield(ref.mod, ref.name))
end
function _lookup_type(frame::Frame, sym::Symbol)
  mod = moduleof(frame)
  isdefined(mod, sym) || return Undefiend
  return typeof′(getfield(mod, sym))
end
function _lookup_type(frame::Frame, e::Expr)
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
    @error "invalid lookup expr: $e"
    return Undefined
  end
end

# TODO: :foreigncall should be special cased
"""
    collect_call_arg_types(frame::Frame, call_expr::Expr; isfc::Bool = false)

Looks up for the types of function call arguments in `call_expr`, while reusing
  the already allocated array in `frame` (i.e. `frame.framedata.callargs`).
"""
function collect_call_arg_types(frame::Frame, call_expr::Expr; isfc::Bool = false)
  arg_types = frame.framedata.callargs
  resize!(arg_types, length(call_expr.args))

  for (i, arg) in enumerate(call_expr.args)
    arg_types[i] = lookup_type(frame, arg)
  end

  return arg_types
end

# types
# -----

typeof′(@nospecialize(x)) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}
typeof′(x::SomeType) = x.type
typeof′(tpl::NTuple{N,SomeType}) where {N} = Tuple{typeof′.(tpl)...}
typeof′(x::Core.IntrinsicFunction) = IntrinsicFunctionType(x)

unwrap_sometype(@nospecialize(x)) = x
unwrap_sometype(x::SomeType) = x.type
