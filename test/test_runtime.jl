module test_runtime

using JET, Test

call_xs(f, xs) = f(xs[])

@test_throws "Type{$Int}" @analysispass Int

pass1 = @analysispass JET.OptAnalyzer
@test pass1() do
    call_xs(sin, Ref(42))
end == sin(42)
@test_throws JET.JETRuntimeError pass1() do
    call_xs(sin, Ref{Any}(42))
end

function_filter(@nospecialize f) = f !== sin
pass2 = @analysispass JET.OptAnalyzer function_filter
@test pass2() do
    call_xs(sin, Ref(42))
end == sin(42)
@test pass2() do
    call_xs(sin, Ref{Any}(42))
end

pass3 = @analysispass JET.JETAnalyzer
@test pass3() do
    collect(1:10)
end == collect(1:10)

end # module test_runtime
