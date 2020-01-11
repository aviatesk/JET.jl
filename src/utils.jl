# Frame
# -----

nstmts(frame::Frame) = frame.nstmts
scopeof(frame::Frame) = frame.scope
moduleof(frame::Frame) = (s = scopeof(frame)) isa Module ? s : s.module
rettyp(frame::Frame) = frame.rettyp === Union{} ?
  Unknown : # if Union{}, there was no `return` statement (which usually means an error occurs within)
  frame.rettyp

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

lookup_type(frame::Frame, @nospecialize(x)) = typeof′(x)
lookup_type(frame::Frame, ssav::SSAValue) = frame.ssavaluetypes[ssav.id]
lookup_type(frame::Frame, slot::SlotNumber) = frame.slottypes[slot.id]
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
    collect_call_argtypes(frame::Frame, call_ex::Expr)

Looks up for the types of function call arguments in `call_ex`.

!!! note
    `call_ex.head` should be `:call` or `:invoke`
"""
function collect_call_argtypes(frame::Frame, call_ex::Expr)
  args = call_ex.head === :call ? call_ex.args :
    call_ex.head === :invoke ? call_ex.args[2:end] :
    return Type[]
  return lookup_type.(Ref(frame), args)
end

# types
# -----

typeof′(@nospecialize(x)) = typeof(x)
typeof′(@nospecialize(x::Type{T})) where {T} = Type{T}
typeof′(x::ProfiledType) = x.type
# typeof′(tpl::NTuple{N,ProfiledType}) where {N} = Tuple{typeof′.(tpl)...}

unwrap_pt(@nospecialize(x)) = x
unwrap_pt(pt::ProfiledType) = pt.type

include_unknwon(@nospecialize(typ::Type)) = typ == Unknown
# XXX: introduce this ?
# include_unknown(union::Union) = union.a == Unknown || include_unknown(union.b)
include_unknwon(itr) = any(==(Unknown), itr)

"""
    @return_if_unknown! typ_ex

Returns [`Unknown`](@ref) type immediatelly if `typ_ex` includes `Unknown`.

See also: [`include_unknwon`](@ref)
"""
macro return_if_unknown!(typ_ex)
  return quote
    typ = $(esc(typ_ex))
    include_unknwon(typ) && return Unknown
    typ
  end
end

# function and methods
# --------------------

# adapted from Base.methods_including_ambiguous
function matching_methods(@nospecialize(tt))
  world = typemax(UInt)
  min = UInt[typemin(UInt)]
  max = UInt[typemax(UInt)]
  return ccall(:jl_matching_methods, Any, (Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}), tt, -1, 1, world, min, max)::Vector{Any}
end
