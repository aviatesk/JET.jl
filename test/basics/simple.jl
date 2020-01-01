using TypeProfiler

module TypeProfiler

_add(x, y) = Core.Intrinsics.add_int(x, y)
add(x, y) = x + y
add(x, y, z) = x + y + z

frame = enter_call(_add, 1, 1)
evaluate_or_profile!(frame)

frame = enter_call(add, 1, 1)
evaluate_or_profile!(frame)

frame = enter_call(add, 1, 1, 1)
evaluate_or_profile!(frame)

# end
