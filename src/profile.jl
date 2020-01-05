"""
    @return_if_unknown! typ

Returns [`Unknown`](@ref) type immediatelly if `typ` includes `Unknown`.

See also: [`include_unknwon`](@ref)
"""
macro return_if_unknown!(typ)
  quote
    if include_unknwon($typ)
      return Unknown
    end
  end |> esc
end

"""
    @report!(report)

Adds `report` to `frame.reports` and then returns `Unknown` type away
  from the local scope.

!!! note
    `frame::Frame` is supposed to exist in a local scope.
"""
macro report!(report)
  quote
    report!(frame, $report)
    return Unknown
  end |> esc
end
report!(frame::Frame, report::ErrorReport) = push!(frame.reports, report)

macro maybe_report_argnumerr!(f, expected, actual)
  quote
    if $expected !== (profiled = $actual isa Int ? $actual : length($actual))
      @report!(ArgumentNumberErrorReport(frame, $f, $expected, profiled))
    end
  end |> esc
end

function profile_call!(frame, call_ex)
  ret = maybe_profile_builtin_call!(frame, call_ex)
  ret isa Vector{Type} || return ret

  # TODO: recursive method call
  return Any
end

function profile_subtype_call!(frame, argtyps)
  @maybe_report_argnumerr!(<:, 2, argtyps)

  expected = Tuple{Type,Type}
  actual = Tuple[argtyps...]
  if actual <: expected
    return Bool
  else
    @report!(ArgumentTypeErrorReport(frame, <:, expected, actual))
  end
end

function profile_equiv_call!(frame, argtyps)
  @maybe_report_argnumerr!(===, 2, argtyps)
  return Bool
end

function profile_ifelse_call!(frame, argtyps)
  @maybe_report_argnumerr!(ifelse, 3, argtyps)
  condtyp, l, r  = @inbounds argtyps
  if condtyp == Bool
    return tmerge(l, r)
  else
    @report!(ArgumentTypeErrorReport(frame, ifelse, Bool, condtyp))
  end
end

function profile_isa_call!(frame, argtyps)
  @maybe_report_argnumerr!(isa, 2, argtyps)

  expected = Tuple{Any,Type}
  actual = Tuple{argtyps...}
  if actual <: expected
    return Bool
  else
    @report!(ArgumentTypeErrorReport(frame, isa, expected, actual))
  end
end

function profile_isdefined_call!(frame, argtyps)
  @maybe_report_argnumerr!(isdefined, 2, argtyps)

  expected = Tuple{Any, Union{Symbol, Int}}
  actual = Tuple{argtyps...}
  if actual <: expected
    return Bool
  else
    @report!(ArgumentTypeErrorReport(frame, isdefined, expected, actual))
  end
end

function profile_typeof_call!(frame, argtyps)
  @maybe_report_argnumerr!(typeof, 1, argtyps)
  return argtyps[1]
end

function profile_gotoifnot!(frame, gotoifnot_ex)
  # just check the node is really `Bool` type
  condtyp = lookup_type(frame, gotoifnot_ex.args[1])
  @return_if_unknown! condtyp
  condtyp == Bool && return condtyp

  @report!(ConditionErrorReport(frame, condtyp))
end
