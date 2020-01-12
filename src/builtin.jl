"""
    ret = maybe_profile_builtin_call!(frame, call_ex)

If `call_ex` is a builtin call, profile it and return its return type,
  otherwise, return `call_argtypes` that represents call argument types of the
  (non-builtin) call.

For this function to work, `frame.src` _**should**_ be a typed IR.
Then a builtin call has already been through the abstract interpretation by
  [`Core.Compiler.abstract_call_known`](@ref), and then we can trust its
  result and report an error if the return type is [`Union{}`](@ref).

!!! warning
    For `Core.IntrinsicFunction`s, [`Core.Compiler.builtin_tfunction`](@ref) only
      performs really rough estimation of its return type.
    Accordingly this function also can mis-profile errors in intrinsic function calls.
"""
function maybe_profile_builtin_call!(frame, call_ex)
  @return_if_unknown! call_argtypes = collect_call_argtypes(frame, call_ex)

  ftyp = @inbounds call_argtypes[1]
  !<:(ftyp, Core.Builtin) && return call_argtypes

  rettyp = frame.src.ssavaluetypes[frame.pc]

  # Union{} means the inference catches an error in this call
  if rettyp == Union{}
    # TODO: handle exceptions somehow
    # throw accepts any type of object and TP currently just ignores them
    ftyp == typeof(throw) && return rettyp

    tt = to_tuple_type(call_argtypes)
    @report!(frame, InvalidBuiltinCallErrorReport(tt))
  end

  return rettyp
end
