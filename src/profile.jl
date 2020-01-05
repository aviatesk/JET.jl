"""
    @return_if_unknown! type

Returns [`Unknown`](@ref) type immediatelly if `type` includes `Unknown`.

See also: [`include_unknwon`](@ref)
"""
macro return_if_unknown!(typex)
  quote
    if include_unknwon($typex)
      return Unknown
    end
  end |> esc
end

report!(frame::Frame, report::ErrorReport) = push!(frame.reports, report)

"""
    @report!(report)

Adds `report` to `frame.reports` (NOTE: `frame::Frame` is supposed to exist in a local scope) and then returns `Unknown` type away from the local scope.
"""
macro report!(report)
  quote
    report!($report)
    return Unknown
  end |> esc
end

function profile_call!(frame, call_ex)
  ret = maybe_profile_builtin_call!(frame, call_ex)
  ret isa Vector{Type} || return ret

  # TODO: recursive method call
  return Any
end

function profile_isdefined_call!(frame, argtyps)
  nargs = length(argtyps)
  if nargs !== 2
    @report!(ArgumentNumberErrorReport(frame, isdefined, 2, nargs))
  end

  @return_if_unknown! argtyps

  second_argtyp = @inbounds argtyps[2]
  if second_argtyp in (Symbol, Int, Union{Symbol, Int})
    return Bool
  else
    @report!(ArgumentTypeErrorReport(frame, isdefined, Union{Symbol, Int}, second_argtyp))
  end
end

function profile_gotoifnot!(frame, gotoifnot_ex)
  # just check the node is really `Bool` type
  condtyp = lookup_type(frame, gotoifnot_ex.args[1])
  @return_if_unknown! condtyp
  condtyp == Bool && return condtyp

  @report!(ConditionErrorReport(frame, condtyp))
end
