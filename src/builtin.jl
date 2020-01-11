"""
    ret = maybe_profile_builtin_call!(frame, call_ex, expand::Bool)

If `ftyp` is a builtin function typ, profile it and return its return type.
Otherwise, return `call_argtypes` that represents types of the (non-builtin) call.

For this function to work, `frame.src` _**should**_ hold typed IR.
Then a builtin call should already get through the abstract interpretation by
  [`Core.Compiler.abstract_call_known`](@ref), and so let's just trust its result
  and report an error if the return type is [`Union{}`](@ref)

!!! warning
    For [`Core.IntrinsicFunction`]s, [`Core.Compiler.builtin_tfunction`](@ref) only
      performs really rough estimation of its return type.
    Accordingly this function also can mis-profile errors in intrinsic function calls.
"""
function maybe_profile_builtin_call!(frame, call_ex, expand::Bool = false)
  @return_if_unknown! call_argtypes = collect_call_argtypes(frame, call_ex)

  ftyp = @inbounds call_argtypes[1]
  ftyp <: Core.Builtin || return call_argtypes

  rettyp = frame.src.ssavaluetypes[frame.pc]
  if rettyp == Union{}
    tt = to_tuple_type(call_argtypes)
    @report!(frame, InvalidBuiltinCallErrorReport(tt))
  end

  return rettyp
end
