function type_annotate_framecode!(frame::Frame, m::Method)
  newsrc = typed_src(frame, m)
  replace_coretypes!(newsrc)
  frame.framecode = copy_framecode(frame.framecode, newsrc)
end

# XXX: maybe too fragile, make this robust
function typed_src(frame::Frame, m::Method)
  # extract argument types from `FrameData.locals`
  fargs = filter(!isnothing, frame.framedata.locals) # wrapped in `Some`
  f = fargs[1].value
  types = tuple(typeof′.(fargs[2:end])...)

  rets = try
    code_typed(f, types; optimize = false)
  catch err
    @error err
    []
  end

  length(rets) !== 1 && error("failed to infer (maybe already profiled): $(frame)")

  src, rettyp = rets[1]::Pair{Core.CodeInfo, DataType}
  return src
end

function copy_framecode(framecode::FrameCode, src::Core.CodeInfo)
  newframecode = ccall(:jl_new_struct_uninit, Any, (Any,), FrameCode)::FrameCode
  for (i, name) in enumerate(fieldnames(FrameCode))
    if isdefined(framecode, name)
      val = name === :src ? src : getfield(framecode, name)
      ccall(:jl_set_nth_field, Cvoid, (Any, Csize_t, Any), newframecode, i-1, val===nothing || isa(val, Union{Type, Method}) ? val : copy(val))
    end
  end
  return newframecode
end

function replace_coretypes!(src::Core.CodeInfo; rev::Bool= false)
  replace_coretypes_list!(src.code; rev = rev)
  replace_coretypes_list!(src.slottypes; rev = rev)
  replace_coretypes_list!(src.ssavaluetypes; rev = rev)
  src.rettype = rev ?
    replaced_coretype_rev(src.rettype) :
    replaced_coretype(src.rettype)
  return nothing
end

function replace_coretypes_list!(xs; rev::Bool)
  for (i, x) in enumerate(xs)
    if x isa Expr
      replace_coretypes_list!(x.args; rev = rev)
      continue
    end
    xs[i] = rev ? replaced_coretype_rev(x) : replaced_coretype(x)
  end
  return nothing
end

replaced_coretype(@nospecialize(x)) = x
replaced_coretype(ssav::Core.SSAValue) = SSAValue(ssav.id)
replaced_coretype(slot::Core.SlotNumber) = SlotNumber(slot.id)
replaced_coretype(c::Core.Compiler.Const) = Const(c.val, c.actual)
replaced_coretype(tslot::Core.TypedSlot) = TypedSlot(tslot.id, tslot.typ)

replaced_coretype_rev(@nospecialize(x)) = x
replaced_coretype_rev(ssav::SSAValue) = Core.SSAValue(ssav.id)
replaced_coretype_rev(slot::SlotNumber) = Core.SlotNumber(slot.id)
replaced_coretype_rev(c::Const) = Core.Compiler.Const(c.val, c.actual)
replaced_coretype(tslot::TypedSlot) = Core.TypedSlot(tslot.id, tslot.typ)
