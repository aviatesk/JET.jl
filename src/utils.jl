# lookups
# -------

"""
    typ = lookup_type(frame::Frame, x)

Looks up a previously-inferred type referenced as `SSAValue`, `SlotNumber`,
  `Const`, `TypedSlot`, `QuoteNode`, `GlobalRef`, or `Symbol` for a binding in
  a scope of `moduleof(frame)`.
If none of the above apply, returns the direct type of `x`.
"""
lookup_type(frame::Frame, @nospecialize(x)) = typeof′(x) # fallback case
lookup_type(frame::Frame, ssav::SSAValue) = frame.framecode.src.ssavaluetypes[ssav.id]
lookup_type(frame::Frame, slot::SlotNumber) = frame.framecode.src.slottypes[slot.id]
lookup_type(frame::Frame, c::Const) = lookup_type(frame, c.val) # cascade to its value
lookup_type(frame::Frame, tslot::TypedSlot) = tslot.typ
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
    val = lookup_value(frame, x)

Looks up a _value_ referenced as Const`, `QuoteNode`, `GlobalRef` or `Symbol` for
  a binding in a scope of `moduleof(frame)`.
If none of the above apply, returns the direct value of `x`.

!!! note

    Obviously our `frame` only holds _types_ instead of their actual values,
    we could expect this function work in a very limited circumstance like looking
    for a constant binding, etc.
"""
function lookup_value end
lookup_value(frame::Frame, @nospecialize(x)) = x
lookup_value(frame::Frame, c::Const) = c.val
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

unwrap_sometype(@nospecialize(x)) = x
unwrap_sometype(x::SomeType) = x.type
