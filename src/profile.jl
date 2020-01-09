macro maybe_report_argnumerr!(frame, ftyp, profiled, actual)
  return quote
    (profiled = $(esc(profiled))) isa Int || (profiled = length(profiled))
    (actual = $(esc(actual))) isa Int || (actual = length(actual))
    if profiled !== actual
      @report!($(esc(frame)), ArgumentNumberErrorReport($(esc(ftyp)), profiled, actual))
    end
  end
end

macro maybe_report_argtyperr!(frame, ftyp, profiled, actual)
  return quote
    (profiled = $(esc(profiled))) isa Type || (profiled = to_tuple_type(profiled))
    (actual = $(esc(actual))) isa Type || (actual = to_tuple_type(actual))
    if !<:(actual, profiled)
      @report!($(esc(frame)), ArgumentTypeErrorReport($(esc(ftyp)), profiled, actual))
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

function profile_subtype_call!(frame, call_argtypes)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 2, argtyps)
  @maybe_report_argtyperr!(frame, ftyp, Tuple{Type,Type}, argtyps)
  return Bool
end

function profile_equiv_call!(frame, call_argtypes)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 2, argtyps)
  return Bool
end

function profile_ifelse_call!(frame, call_argtypes)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 3, argtyps)
  condtyp, l, r  = argtyps
  @maybe_report_argtyperr!(frame, ftyp, Bool, condtyp)
  return tmerge(l, r)
end

function profile_isa_call!(frame, call_argtypes)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 2, argtyps)
  @maybe_report_argtyperr!(frame, ftyp, Tuple{Any,Type}, argtyps)
  return Bool
end

function profile_isdefined_call!(frame, argtyps)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 2, argtyps)
  @maybe_report_argtyperr!(frame, ftyp, Tuple{Any, Union{Symbol,Int}}, argtyps)
  return Bool
end

function profile_typeof_call!(frame, argtyps)
  @views ftyp, argtyps = call_argtypes[1], call_argtypes[2:end]
  @maybe_report_argnumerr!(frame, ftyp, 1, argtyps)
  return argtyps[1]
end
