function prepare_argtypes(@nospecialize(f_type), )
end

framecode, lenv = get_call_framecode(fargs, frame.framecode, 10)

function summer(A::AbstractVector{T}) where {T}
    s = zero(T)
    for a in A
        s += a
    end
    return s
end
function foo(n::T) where {T<:Number}
  s = one(T)
  summer(s:n)
end
function hoo(a; k = 1)
  ret = sin(a)
  ret *= cos(k)
  return ret
end
outer() = hoo(1, k = 10)
frame = enter_call(outer,)
JuliaInterpreter.finish_and_return!(frame)

frame = enter_call(foo, 10)
framecode, lenv = get_call_framecode(Any[SomeType(typeof(summer)), SomeType(typeof(1:10))], frame.framecode, 3)
newframe = prepare_frame_caller(frame, framecode, Any[SomeType(typeof(summer)), SomeType(typeof(1:10))], lenv)
@show newframe
newframe.framedata.locals
