function profile_call!(frame, call_ex)
  ret = maybe_profile_builtin_call!(frame, call_ex)
  ret isa Vector || return ret

  tt = to_tt(ret)
  tt.parameters[1] == typeof(print) && return Nothing # TODO: enable profiling on `print`
  mms = matching_methods(tt)

  isempty(mms) && @report!(frame, NoMethodErrorReport(tt))

  # # TODO: report method ambiguity error
  # if all(isconcretetype, @view tt.parameters[2:end])
  #   ms = [mm[3]::Method for mm in mms]
  #   for (m1, m2) in Iterators.product(ms, ms)
  #     m1 == m2 && continue
  #     Base.isambiguous(m1, m2) && @report!(frame, MethodAmbiguityErrorReport(tt, ms))
  #   end
  # end

  # TODO: handle abstract argument types gracefully
  rettyp = Union{}
  for (tt, sparams::SimpleVector, m::Method) in mms
    maybe_newframe = prepare_frame(m, tt, sparams, frame)

    if !isa(maybe_newframe, Frame)
      rettyp = tmerge(rettyp, maybe_newframe) # cache hit
      continue
    end

    newframe = maybe_newframe::Frame
    frame.callee = FrameChain(lineinfonode(frame), newframe)
    tmp_rettyp = evaluate_or_profile!(newframe)
    rettyp = tmerge(rettyp, tmp_rettyp)
    frame.callee = nothing
  end

  return rettyp
end

# TODO: support special cased invokes: e.g. getproperty
function profile_invoke!(frame, invoke_ex)
  mi = invoke_ex.args[1]::MethodInstance
  slottyps = collect_call_argtypes(frame, invoke_ex)
  maybe_newframe = prepare_frame(mi, slottyps, frame)

  !isa(maybe_newframe, Frame) && return maybe_newframe # cache hit

  newframe = maybe_newframe::Frame
  frame.callee = FrameChain(lineinfonode(frame), newframe)
  rettyp = evaluate_or_profile!(newframe)
  frame.callee = nothing

  return rettyp
end

function profile_gotoifnot!(frame, gotoifnot_ex)
  # just check the node is really `Bool` type
  condtyp = lookup_type(frame, gotoifnot_ex.args[1])
  condtyp == Bool && return condtyp
  condtyp == Unknown && return Unknown
  @report!(frame, ConditionErrorReport(condtyp))
end
