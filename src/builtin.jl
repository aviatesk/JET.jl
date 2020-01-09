"""
    ret = maybe_profile_builtin_call!(frame, call_ex, expand::Bool)

If `ftyp` is a builtin function typ, profile it and return the profiled
  type in a [`ProfiledType`](@ref) wrapper.
Otherwise, return `call_argtypes` that represents types of the (non-builtin) call.

$(""# If `expand` is true, `Core._apply` calls will be resolved as a call to the applied function.
)

!!! warning

    Most of the builtin functions are not implemented yet. For unimplemented functions,
    TypeProfiler just _trust_s the inference and reuses the inferred types for now.
"""
function maybe_profile_builtin_call!(frame, call_ex, expand::Bool = false)
  @return_if_unknown! call_argtypes = collect_call_argtypes(frame, call_ex)

  ftyp = @inbounds call_argtypes[1]
  ftyp <: Core.Builtin || return call_argtypes
  if ftyp == Core.IntrinsicFunction
    # TODO: identify intrinsic functions
    return frame.src.ssavaluetypes[frame.pc]
  else
    # builtin functions
    if ftyp == typeof(<:)
      return profile_subtype_call!(frame, call_argtypes)
    elseif ftyp == typeof(===)
      return profile_equiv_call!(frame, call_argtypes)
    elseif ftyp == typeof(ifelse)
      return profile_ifelse_call!(frame, call_argtypes)
    elseif ftyp == typeof(isa)
      return profile_isa_call!(frame, call_argtypes)
    elseif ftyp == typeof(isdefined)
      return profile_isdefined_call!(frame, call_argtypes)
    elseif ftyp == typeof(typeof)
      return profile_typeof_call!(frame, call_argtypes)
    else
      @warn "unimplmented builtin call: $ftyp"
      return frame.src.ssavaluetypes[frame.pc]
    end
  end

  @error "you shouldn't reach here: $call_ex"
  return Unknown
end
