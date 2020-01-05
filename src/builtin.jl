"""
    ret = maybe_profile_builtin_call!(reports, frame, call_ex, expand::Bool)

If `ftyp` is a builtin function typ, profile it and return the profiled
  type in a [`ProfiledType`](@ref) wrapper.
Otherwise, return `call_argtypes` that represents types of the (non-builtin) call.

$(""# If `expand` is true, `Core._apply` calls will be resolved as a call to the applied function.
)

!!! warning

    Most of the builtin functions are not implemented yet. For unimplemented functions,
    TypeProfiler just _trust_s the inference and reuses the inferred types for now.
"""
function maybe_profile_builtin_call!(reports, frame, call_ex, expand::Bool = false)
  call_argtypes = collect_call_argtypes(frame, call_ex)
  ftyp = @inbounds call_argtypes[1]
  argtyps = @inbounds call_argtypes[2:end]

  @return_if_unknown! argtyps

  ftyp <: Core.Builtin || return @show call_argtypes

  if ftyp == Core.IntrinsicFunction
    # TODO: identify intrinsic functions
    return frame.src.ssavaluetypes[frame.pc]
  else
    # builtin functions
    f = to_function(ftyp)
    if f == isdefined
      return profile_isdefined_call!(reports, frame, argtyps)
    else
      return frame.src.ssavaluetypes[frame.pc]
    end
  end

  @error "you shouldn't reach here: $(call_ex)"
  return Unknown
end
