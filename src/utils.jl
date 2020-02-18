# Frame
# -----

nstmts(frame::Frame) = frame.nstmts
scopeof(frame::Frame) = frame.scope
moduleof(frame::Frame) = (s = scopeof(frame)) isa Module ? s : s.def.module
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
caller_frame(frame::Frame, n::Int) = reduce(∘, ntuple(i -> caller_frame, n))(frame)
caller_lin(frame::Frame) = frame.caller === nothing ? nothing : frame.caller.lin
caller_lin(frame::Frame, n::Int) = reduce(∘, ntuple(i -> caller_lin, n))(frame)
callee(frame::Frame) = frame.callee
callee_frame(frame::Frame) = frame.callee === nothing ? nothing : frame.callee.frame
callee_frame(frame::Frame, n::Int) = reduce(∘, ntuple(i -> callee_frame, n))(frame)
callee_lin(frame::Frame) = frame.callee === nothing ? nothing : frame.callee.lin
callee_lin(frame::Frame, n::Int) = reduce(∘, ntuple(i -> callee_lin, n))(frame)

is_root(frame::Frame) = caller(frame) === nothing
is_leaf(frame::Frame) = callee(frame) === nothing

function traverse(traverser, init, stop_predicate = !isnothing)
  cur = init
  while (next = traverser(cur)) |> stop_predicate
    cur = next
  end
  return cur
end

root(frame::Frame) = root_frame(frame)
root_frame(frame::Frame) = traverse(caller_frame, frame)
root_lin(frame::Frame) = traverse(caller_lin, frame)
leaf(frame::Frame) = leaf_frame(frame)
leaf_frame(frame::Frame) = traverse(callee_frame, frame)
leaf_lin(frame::Frame) = traverse(callee_lin, frame)
function depth(frame::Frame; up::Bool = false)
  depth::Int = 0
  traverser = up ?
    frame -> (depth += 1; caller_frame(frame)) :
    frame -> (depth += 1; callee_frame(frame))
  traverse(traverser, frame)
  return depth
end

# types
# -----

typeof′(@nospecialize(x)) = typeof(x)
typeof′(@nospecialize(x::Type{T})) where {T} = Type{T}
typeof′(x::ProfiledType) = x.type
# typeof′(tpl::NTuple{N,ProfiledType}) where {N} = Tuple{typeof′.(tpl)...}

unwrap_pt(@nospecialize(x)) = x
unwrap_pt(pt::ProfiledType) = pt.type

# function and methods
# --------------------

# adapted from Base.methods_including_ambiguous
function matching_methods(@nospecialize(tt))
  # XXX: valid ?
  tt.parameters[1] == Any && return Any[]
  world = typemax(UInt)
  min = UInt[typemin(UInt)]
  max = UInt[typemax(UInt)]
  return ccall(:jl_matching_methods, Any, (Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}), tt, -1, 1, world, min, max)::Vector{Any}
end

# returns a call signature string from tt
function tt_to_signature_str(@nospecialize(tt))
  fn = ft_to_fname(tt.parameters[1])
  args = join("::" .* string.(tt.parameters[2:end]), ", ")
  return string(fn, '(', args, ')')
end

# returns function name from its type
function ft_to_fname(@nospecialize(ft))
  return if Core.Compiler.isconstType(ft)
    repr(ft.parameters[1])
  elseif ft isa DataType && isdefined(ft, :instance)
    repr(ft.instance)
  else
    repr(ft)
  end
end
