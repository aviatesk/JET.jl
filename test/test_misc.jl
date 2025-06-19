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

using Pkg, JET
using JET: find_pkg

@testset "`find_pkg`" begin
    pkgid = Base.PkgId(JET)
    filename = pathof(JET)
    target = (; pkgid, filename)

    @test find_pkg("JET") == target
    @test_throws ErrorException find_pkg("unknown")

    @test find_pkg(JET) == target
    @test_throws ErrorException find_pkg(Module())

    # suppress logs from Pkg.jl if possible
    old = Pkg.project().path
    try
        Pkg.activate(pkgdir(JET); io=devnull)
        @test find_pkg(nothing) == target

        Pkg.activate(; temp=true, io=devnull)
        @test_throws ErrorException find_pkg(nothing)
    finally
        Pkg.activate(old; io=devnull)
    end
end

using JET: process_config_dict
using TOML: TOML
macro toml_str(s); TOML.parse(TOML.Parser(s)); end

@testset "`process_config_dict`" begin
    let config_dict = toml"""
        # usual
        analyze_from_definitions = true

        # will be `parse`d or `eval`ed
        context = "Base"
        concretization_patterns = ["const x_ = y_"]
        toplevel_logger = "stdout"
        """

        config = process_config_dict(config_dict)
        @test (:analyze_from_definitions => true) in config
        @test (:context => Base) in config
        @test (:concretization_patterns => [:(const x_ = y_)]) in config
        @test (:toplevel_logger => stdout) in config
    end

    # error when invalid expression given
    let config_dict = toml"""
        concretization_patterns = ["const x_ = end"]
        """
        @test_throws JET.JETConfigError process_config_dict(config_dict)
    end

    # error when incomplete expression given
    let config_dict = toml"""
        concretization_patterns = ["const x_ = "]
        """
        @test_throws JET.JETConfigError process_config_dict(config_dict)
    end

    # should be whitespece/newline insensitive
    let config_dict = toml"""
        concretization_patterns = [
            \"\"\"
            const x_ = y_
            \"\"\"
        ]
        """
        config = process_config_dict(config_dict)
        @test (:concretization_patterns => [:(const x_ = y_)]) in config
    end
end

@testset "configuration validation" begin
    # https://github.com/aviatesk/JET.jl/issues/414
    @test_throws "lkdsjkdlkas" report_call(+, (Int, Int), lkdsjkdlkas=true)
    @test_throws "target_module" @report_call target_module=(Core,) sum(Char[])
    @test_throws "foo = :bar" @report_call foo = :bar sum(Char[])
end

# NOTE this test is not stable as `schedule(t, InterruptException(); error=true)` may not work as expected
# @testset "watch_file" begin
#     @test_throws "Revise.jl is not loaded; load Revise and try again." watch_file("../demo.jl")
#
#     using Revise
#
#     let t = @async begin
#             mktemp() do path, io
#                 redirect_stdout(io) do
#                     watch_file("../demo.jl")
#                 end
#                 flush(io)
#                 read(path, String)
#             end
#         end
#         sleep(5)
#         if istaskstarted(t) && !(istaskdone(t) || istaskfailed(t))
#             ok = true
#             schedule(t, InterruptException(); error=true)
#         else
#             ok = false
#         end
#         @test ok
#         @test occursin("5 possible errors found", fetch(t))
#     end
# end

end # module test_misc
