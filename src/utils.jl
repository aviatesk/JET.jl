# Frame
# -----

nstmts(frame::Frame) = frame.nstmts
scopeof(frame::Frame) = frame.scope
moduleof(frame::Frame) = (s = scopeof(frame)) isa Module ? s : s.module

pc_stmt(src::CodeInfo, pc::Int) = src.code[pc]
pc_stmt(frame::Frame, pc::Int) = pc_stmt(frame.src, pc)
pc_stmt(frame::Frame) = pc_stmt(frame, frame.pc)

lineinfonode(frame::Frame) = frame.src.linetable[codeloc(frame)]
codeloc(frame::Frame) = frame.src.codelocs[frame.pc]

caller(frame::Frame) = frame.caller
caller_frame(frame::Frame) = frame.caller === nothing ? nothing : frame.caller.frame
caller_lin(frame::Frame) = frame.caller === nothing ? nothing : frame.caller.lin
callee(frame::Frame) = frame.callee
callee_frame(frame::Frame) = frame.callee === nothing ? nothing : frame.callee.frame
callee_lin(frame::Frame) = frame.callee === nothing ? nothing : frame.callee.lin

is_root(frame::Frame) = caller(frame) === nothing
is_leaf(frame::Frame) = callee(frame) === nothing

function traverse(traverser, init, stop_predicate = !isnothing)
  cur = init
  while (next = traverser(cur)) |> stop_predicate
    cur = next
  end
  return cur
end

root(frame::Frame) = traverse(caller, frame)
root_frame(frame::Frame) = traverse(caller_frame, frame)
root_lin(frame::Frame) = traverse(caller_lin, frame)
leaf(frame::Frame) = traverse(callee, frame)
leaf_frame(frame::Frame) = traverse(callee_frame, frame)
leaf_lin(frame::Frame) = traverse(callee_lin, frame)

# lookups
# -------

function lookup_type(frame::Frame, @nospecialize(x))
  @warn "hit fallback lookup_type: $(typeof(x))"
  return typeof′(x)
end
lookup_type(frame::Frame, ssav::SSAValue) = frame.ssavaluetypes[ssav.id]
function lookup_type(frame::Frame, gr::GlobalRef)
  if isdefined(gr.mod, gr.name)
    typeof′(getfield(gr.mod, gr.name))
  else
    # TODO: error report
    return Unknown
  end
end
lookup_type(frame::Frame, qn::QuoteNode) = typeof′(qn.value)

# TODO?:
# maybe we want to make a temporary field `call_argtypes` in `Frame` and reuse
# the previously allocated array for keeping the current call argtypes
"""
    collect_call_argtypes(frame::Frame, call_expr::Expr)

Looks up for the types of function call arguments in `call_expr`.
"""
collect_call_argtypes(frame::Frame, call_expr::Expr) =
  Type[lookup_type(frame, arg) for (i, arg) in enumerate(call_expr.args)]

"""
    to_function(ftyp::Type)

Identifies a function object from its type.

!!! note
    Intrinsic functions can't be identified its type, so we need to directly
    reference its value instead of using this function.
"""
function to_function(@nospecialize(ftyp::Type))
  return if ftyp <: Function
    ftyp.instance
  elseif ftyp <: Type
    ftyp.parameters[1]
  else # bultin
    if ftyp == Core.IntrinsicFunction
      error("intrinsic function can't be identified from its type")
    end
    ftyp.instance
  end
end

# """
#     typ = lookup_type(frame::Frame, x)
#
# Looks up a previously-inferred type referenced as `SSAValue`, `SlotNumber`,
#   `Const`, `TypedSlot`, `QuoteNode`, `GlobalRef`, or `Symbol` for a binding in
#   a scope of `moduleof(frame)`.
# If none of the above apply, returns the direct type of `x`.
# """
# function lookup_type(frame::Frame, @nospecialize(x))
#   ret = _lookup_type(frame, x)
#   # ensure all `Const` are stripped
#   return ret isa Const ? lookup_type(frame, ret.val) : ret
# end
# _lookup_type(frame::Frame, @nospecialize(x)) = typeof′(x) # fallback case8f
# _lookup_type(frame::Frame, ssav::SSAValue) = frame.framecode.src.ssavaluetypes[ssav.id]
# _lookup_type(frame::Frame, slot::SlotNumber) = frame.framecode.src.slottypes[slot.id]
# _lookup_type(frame::Frame, c::Const) = _lookup_type(frame, c.val) # cascade to its value
# _lookup_type(frame::Frame, tslot::TypedSlot) = tslot.typ
# _lookup_type(frame::Frame, node::QuoteNode) = typeof′(node.value)
# function _lookup_type(frame::Frame, ref::GlobalRef)
#   isdefined(ref.mod, ref.name) || return Unknown
#   return typeof′(getfield(ref.mod, ref.name))
# end
# function _lookup_type(frame::Frame, sym::Symbol)
#   mod = moduleof(frame)
#   isdefined(mod, sym) || return Undefiend
#   return typeof′(getfield(mod, sym))
# end
# function _lookup_type(frame::Frame, e::Expr)
#   head = e.head
#   if head === :the_exception
#     @error "exceptions are not supported"
#     return Unknown
#   elseif head === :static_parameter
#     arg = e.args[1]::Int
#     if isassigned(frame.framedata.sparams, arg)
#       return Type{frame.framedata.sparams[arg]}
#     else
#       syms = sparam_syms(frame.framecode.scope)
#       throw(UndefVarError(syms[arg]))
#     end
#   elseif head === :boundscheck
#     if length(e.args) === 0
#       return Bool
#     else
#       error("invalid boundscheck at ", e)
#     end
#   else
#     @error "invalid lookup expr: $e"
#     return Unknown
#   end
# end

# types
# -----

typeof′(@nospecialize(x)) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}
typeof′(x::ProfiledType) = x.type
# typeof′(tpl::NTuple{N,ProfiledType}) where {N} = Tuple{typeof′.(tpl)...}

unwrap_pt(@nospecialize(x)) = x
unwrap_pt(pt::ProfiledType) = pt.type

include_unknwon(typ::Type) = typ == Unknown
include_unknwon(itr) = any(==(Unknown), itr)
