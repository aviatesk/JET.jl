function profile_call!(frame, call_ex)
  ret = maybe_profile_builtin_call!(frame, call_ex)
  ret isa Vector || return ret

  tt = to_tt(ret)
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
    newframe = Frame(m, tt, sparams, frame)
    frame.callee = FrameChain(lineinfonode(frame), newframe)
    tmp_rettyp = evaluate_or_profile!(newframe)
    rettyp = tmerge(rettyp, tmp_rettyp)
    frame.callee = nothing
  end

  return rettyp
end

function profile_gotoifnot!(frame, gotoifnot_ex)
  # just check the node is really `Bool` type
  @return_if_unknown! condtyp = lookup_type(frame, gotoifnot_ex.args[1])
  condtyp == Bool && return condtyp
  @report!(frame, ConditionErrorReport(condtyp))
end
