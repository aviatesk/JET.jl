macro maybe_report_argnumerr!(frame, f, profiled, actual)
  return quote
    profiled = $(esc(profiled))
    (actual = $(esc(actual))) isa Int || (actual = length(actual))
    if profiled !== actual
      @report!($(esc(frame)), ArgumentNumberErrorReport($(esc(f)), profiled, actual))
    end
  end
end

macro maybe_report_argtyperr!(frame, f, profiled, actual)
  return quote
    profiled = $(esc(profiled))
    actual = $(esc(actual))
    if !<:(actual, profiled)
      @report!($(esc(frame)), ArgumentTypeErrorReport($(esc(f)), profiled, actual))
    end
  end
end

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

# builtins
# --------

function profile_subtype_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, <:, 2, argtyps)
  @maybe_report_argtyperr!(frame, <:, Tuple{Type,Type}, to_tuple_type(argtyps))
  return Bool
end

function profile_equiv_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, ===, 2, argtyps)
  return Bool
end

function profile_ifelse_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, ifelse, 3, argtyps)
  condtyp, l, r  = argtyps
  @maybe_report_argtyperr!(frame, ifelse, Bool, condtyp)
  return tmerge(l, r)
end

function profile_isa_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, isa, 2, argtyps)
  @maybe_report_argtyperr!(frame, isa, Tuple{Any,Type}, to_tuple_type(argtyps))
  return Bool
end

function profile_isdefined_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, isdefined, 2, argtyps)
  @maybe_report_argtyperr!(frame, isdefined, Tuple{Any, Union{Symbol,Int}}, to_tuple_type(argtyps))
  return Bool
end

function profile_typeof_call!(frame, argtyps)
  @maybe_report_argnumerr!(frame, typeof, 1, argtyps)
  return argtyps[1]
end
