# additional replacements for `Core` types (basically what appears in typed `Core.CodeInfo`)
struct Const
  val::Any
  actual::Bool
end
struct TypedSlot
  id::Int
  typ::Any
end

replaced_coretype(@nospecialize(x)) = x
replaced_coretype(ssav::Core.SSAValue) = SSAValue(ssav.id)
replaced_coretype(slot::Core.SlotNumber) = SlotNumber(slot.id)
replaced_coretype(c::Core.Compiler.Const) = Const(c.val, c.actual)
replaced_coretype(tslot::Core.TypedSlot) = TypedSlot(tslot.id, replaced_coretype(tslot.typ))
replaced_coretype_rev(@nospecialize(x)) = x
replaced_coretype_rev(ssav::SSAValue) = Core.SSAValue(ssav.id)
replaced_coretype_rev(slot::SlotNumber) = Core.SlotNumber(slot.id)
replaced_coretype_rev(c::Const) = Core.Compiler.Const(c.val, c.actual)
replaced_coretype_rev(tslot::TypedSlot) = Core.TypedSlot(tslot.id, replaced_coretype_rev(tslot.typ))

"""
    SomeType

Wrapper type for "type-profiled" variables -- the actual type is kept in
  `type` field.
"""
struct SomeType
  type::Type
end

"""
    struct Undefined end

A Singleton type that represents when the type profiler failed to track a type.
It should be reported when and where this is introduced, and then any further
  profiling for things including this type won't be executed.
"""
struct Undefined end
