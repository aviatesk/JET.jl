function profile_condition_type!(reports, frame, cond_typ)
  cond_typ == Bool && return
  lin = lineinfonode(frame)
  report = ConditionErrorReport(frame, lin, cond_typ)
  push!(reports, report)
end
