# introduce JuliaInterpreter's types here
using JuliaInterpreter: Frame, FrameCode, FrameData

# Our own replacements for Core types. We need to do this to ensure we can tell the difference
# between "data" (Core types) and "code" (our types) if we step into Core.Compiler
struct SSAValue
    id::Int
end
struct SlotNumber
    id::Int
end
struct Const
  val::Any
  actual::Bool
end
struct TypedSlot
  id::Int
  typ::Any
end
TypedSlot(id::Int, _const::Core.Compiler.Const) = TypedSlot(id, Const(_const.val, _const.actual))

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
