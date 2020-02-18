# entry point
# -----------

# function profile_file(filename::AbstractString; mod::Module = Main)
#   isfile(filename) || error("No such file exists: $filename")
#   filetext = read(filename, String)
#   profile_text(filetext; filename = filename, mod = Main)
# end
# function profile_text(text::String; filename::AbstractString = "none", mod::Module = Main)
#   exs = Base.parse_input_line(text; filename = filename)
#   for i = 2:2:length(exs.args)
#     frame = prepare_thunk(mod, exs.args[i])
#     evaluate_or_profile!(frame, true)
#   end
# end

function evaluate_or_profile!(frame::Frame)
  while (pc = step_code!(frame)) !== nothing end
  return rettyp(frame)
end

# abstract interpretation
# -----------------------

function step_code!(frame)
  frame.pc > nstmts(frame) && return nothing
  return pc = step_code!(frame, pc_stmt(frame))
end
function step_code!(frame, @nospecialize(stmt))
  @assert is_leaf(frame)
  rhs_type = profile_and_get_rhs_type!(frame, stmt)
  assign_rhs_type!(frame, stmt, rhs_type)
  return frame.pc += 1
end

function profile_and_get_rhs_type!(frame, @nospecialize(stmt))
  error("unimplemented type statement: $stmt")
end
profile_and_get_rhs_type!(frame, ::Nothing) = Nothing
# ignore goto statement and just proceed to profile the next statement
profile_and_get_rhs_type!(frame, gn::GotoNode) = Any
# TODO:
# Store profiled types for Pi, Upsilon nodes, and then use updated (i.e. profiled)
# types instead of inferred ones for Phi, PhiC nodes, respectively
const ControlNode = Union{PiNode, PhiNode, PhiCNode, UpsilonNode}
profile_and_get_rhs_type!(frame, node::ControlNode) =
  frame.src.ssavaluetypes[frame.pc]
profile_and_get_rhs_type!(frame, gr::GlobalRef) = lookup_type(frame, gr)
function profile_and_get_rhs_type!(frame, ex::Expr)
  head = ex.head
  # calls
  if head === :call
    return profile_call!(frame, ex)
  elseif head === :invoke
    return profile_invoke!(frame, ex)
  # :foreigncall and :new are supposed to be statically computed, let's just trust the inference
  # NOTE:
  # for toplevel frame, maybe we need referene ex.args[1], etc, since
  # src.ssavaluetypes may not have been computed
  elseif head === :foreigncall
    return frame.src.ssavaluetypes[frame.pc]::Type
  # constructor
  elseif head === :new || head === :splatnew
    return frame.src.ssavaluetypes[frame.pc]::Type
  # goto
  elseif head === :gotoifnot
    return profile_gotoifnot!(frame, ex)
  # meta
  elseif (
    head === :meta ||
    head === (@static VERSION >= v"1.2.0-DEV.462" ? :loopinfo : :simdloop) ||
    head === :gc_preserve_begin ||
    head === :gc_preserve_end
  )
    return Any
  # TODO: handle exceptions somehow
  elseif head === :enter || head === :leave || head === :pop_exception
    return Any
  elseif head === :throw_undef_if_not
    # # XXX:
    # # :throw_undef_if_not includes lots of false positives as is
    # name = ex.args[1]::Symbol
    # (mod = scopeof(frame)) isa Module || (mod = mod.def)
    # @report!(frame, UndefVarErrorReport(mod, name, true))
    return Any
  elseif head === :unreachable
    # basically this is a sign of an error, but hopefully we profiled all of them
    # up to here, so let's just ignore this
    return Unknown
  elseif head === :return
    rettyp = lookup_type(frame, ex.args[1])
    return update_rettyp!(frame, rettyp)
  elseif head === :static_parameter
    return lookup_type(frame, ex)
  else
    error("unimplmented expression type: $ex")
  end
end

# lookups
# -------

lookup_type(frame::Frame, @nospecialize(x)) = typeof′(x)
lookup_type(frame::Frame, ssav::SSAValue) = frame.ssavaluetypes[ssav.id]
lookup_type(frame::Frame, slot::SlotNumber) = frame.slottypes[slot.id]
function lookup_type(frame::Frame, gr::GlobalRef)
  isdefined(gr.mod, gr.name) && return typeof′(getfield(gr.mod, gr.name))
  @report!(frame, UndefVarErrorReport(gr.mod, gr.name, false))
end
lookup_type(frame::Frame, qn::QuoteNode) = typeof′(qn.value)
function lookup_type(frame::Frame, ex::Expr)
  head = ex.head
  if head === :static_parameter
    return frame.sparams[ex.args[1]]
  elseif head === :boundscheck
    return Bool
  # TODO: handle exceptions somehow
  elseif head === :enter || head === :leave || head === :pop_exception
    return Any
  end
  error("unimplmented expression lookup: $ex")
end

# NOTE: problematic
"""
    collect_call_argtypes(frame::Frame, call_ex::Expr)::Vector

Looks up for the types of function call arguments in `call_ex`.

!!! note
    `call_ex.head` should be `:call` or `:invoke`
"""
function collect_call_argtypes(frame::Frame, call_ex::Expr)::Vector
  local args
  if call_ex.head === :call
    args = call_ex.args
  elseif call_ex.head === :invoke
    args = call_ex.args[2:end]
  else
    return Type[]
  end
  # TODO?
  # maybe we want to make a temporary field `call_argtypes` in `Frame` and reuse
  # the previously allocated array for keeping the current call argtypes
  # return lookup_type.(Ref(frame), args)
  return map(x -> lookup_type(frame, x), args)
end

# assignment and return
# ---------------------

assign_rhs_type!(frame, stmt, rhs_type) = frame.ssavaluetypes[frame.pc] = rhs_type

function update_rettyp!(frame, rettyp)
  frame.rettyp == Unknown && return Unknown # already an error profiled
  rettyp == Unknown && return frame.rettyp = Unknown # ignore previously profiled types once we profile Unknown
  return frame.rettyp = tmerge(frame.rettyp, rettyp)
end
