using Base.Meta: isexpr

# entry point
# -----------

function profile_file(filename::AbstractString; mod::Module = Main)
  isfile(filename) || error("No such file exists: $filename")
  filetext = read(filename, String)
  profile_text(filetext; filename = filename, mod = Main)
end
function profile_text(text::String; filename::AbstractString = "none", mod::Module = Main)
  exs = Base.parse_input_line(text; filename = filename)
  for i = 2:2:length(exs.args)
    frame = prepare_thunk(mod, exs.args[i])
    evaluate_or_profile!(frame, true)
  end
end

function evaluate_or_profile!(frame::Frame, istoplevel::Bool = false)
  # type annotate `frame`
  (s = scopeof(frame)) isa Method && type_annotate_frame!(frame, s)

  # finishes this frame
  while (pc = step_code!(frame, istoplevel)) !== nothing end
  return get_return_type(frame)
end

# function get_return_type(frame)
#   node = pc_expr(frame)
#   if isexpr(node, :return)
#     lookup_type(frame, (node::Expr).args[1])
#   elseif node isa Const && isexpr(node.val, :return)
#     lookup_type(frame, (node.val::Expr).args[1])
#   else
#     error("expected return statement, got ", node)
#   end
# end

# recursive call
# --------------

function step_code!(reports, frame)
  frame.pc > nstmts(frame) && return nothing
  return pc = step_code!(reports, frame, pc_stmt(frame))
end
function step_code!(reports, frame, @nospecialize(stmt))
  @assert is_leaf(frame)
  rhs_type = profile_and_get_rhs_type!(reports, frame, stmt)::PT
  assign_rhs_type!(reports, frame, stmt, rhs_type)
  return frame.pc += 1
end

function profile_and_get_rhs_type!(reports, frame, @nospecialize(stmt))
  @error "unimplemented type statement: $stmt"
  return PT(Undefined)
end

# ignore goto statement and just proceed to profile the next statement
profile_and_get_rhs_type!(reports, frame, gn::GotoNode) = PT(Any)
# NOTE:
# let's just use the inference result for Pi and Phi nodes for now,
# but maybe we want to use updated (i.e. profiled) types instead
# - Pi node check: pi.typ == frame.ssavaluetypes[pi.val]
# - Phi node check: Core.tmerge(lookup_type.(phi.values)) == Core.tmerge(getindex.(frame.ssavaluetypes, phi.values))
profile_and_get_rhs_type!(reports, frame, pi::PiNode) = PT(frame.src.ssavaluetypes[frame.pc])
profile_and_get_rhs_type!(reports, frame, phi::PhiNode) = PT(frame.src.ssavaluetypes[frame.pc])

function profile_and_get_rhs_type!(reports, frame, ex::Expr)
  head = ex.head
  if head === :call
    @warn "you should implment :call head asap"
    return PT(Undefined)
  elseif head === :invoke
    mi = ex.args[1]::MethodInstance
    newframe = Frame(frame, mi)
    frame.callee = FrameChain(lineinfonode(frame), frame)
    # TODO: recursive profiling
    frame.callee = nothing
    return PT(newframe.src.rettype)
  elseif head === :gotoifnot
    # just check the node is really `Bool` type
    condex = ex.args[1]
    condtyp = lookup_type(frame, condex)
    profile_condition_type!(reports, frame, condtyp)
    return PT(condtyp)
  elseif head === :meta
    return PT(Any)
  elseif head === :unreachable
    # sign of an error, but hopefully we profiled all of them up to here, so let's just ignore it
    return PT(Union{})
  elseif head === :return
    retex = ex.args[1]
    rettyp = lookup_type(frame, retex)
    return update_rettyp!(frame, rettyp)
  else
    @error "unimplmented expression type: $ex"
    return PT(Undefined)
  end
end

function assign_rhs_type!(reports, frame, stmt, rhs_type)
  frame.ssavaluetypes[frame.pc] = rhs_type
end

function update_rettyp!(frame, rettyp)
  if frame.rettyp === nothing
    frame.rettyp = PT(rettyp)
  else
    oldtyp = unwrap_pt(frame.rettyp)
    newtyp = tmerge(oldtyp, rettyp)
    frame.rettyp = PT(newtyp)
  end
  return frame.rettyp
end
