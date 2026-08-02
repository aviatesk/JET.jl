module test_misc

using JET, Test

@testset "report_call entry" begin
    @test_throws ErrorException("Could not find single target method for `sin(::String)`") report_call(sin, (String,))
    @test_throws ErrorException("Could not find single target method for `sin(::String)`") @report_call sin("julia")

    # https://github.com/aviatesk/JET.jl/issues/427
    test_call(getproperty, (Any,Symbol))
end

f_method_instance1(s::AbstractString) = sum(s)
f_method_instance2(s::Some) = sum(s.value)
try; f_method_instance1("throws"); catch end
try; f_method_instance2(Some{AbstractString}("throws")); catch end

@testset "from MethodInstance" begin
    let mi = only(Base.specializations(only(methods(f_method_instance1))))
        @test !isempty(JET.get_reports(report_call(mi)))
    end
    let mi = only(Base.specializations(only(methods(f_method_instance2))))
        @test !isempty(JET.get_reports(report_opt(mi)))
    end
end

@testset "configuration validation" begin
    # https://github.com/aviatesk/JET.jl/issues/414
    @test_throws "lkdsjkdlkas" report_call(+, (Int, Int), lkdsjkdlkas=true)
    @test_throws "target_module" @report_call target_module=(Core,) sum(Char[])
    @test_throws "foo = :bar" @report_call foo = :bar sum(Char[])
end


end # module test_misc
