function profile_call!(frame, call_ex)
  ret = maybe_profile_builtin_call!(frame, call_ex)
  ret isa Vector{Type} || return ret

  # TODO: recursive method call
  return Any
end

function profile_gotoifnot!(frame, gotoifnot_ex)
  # just check the node is really `Bool` type
  @return_if_unknown! condtyp = lookup_type(frame, gotoifnot_ex.args[1])
  condtyp == Bool && return condtyp
  @report!(frame, ConditionErrorReport(condtyp))
end
