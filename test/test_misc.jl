@testset "report_call entry" begin
    @test_throws ErrorException("unable to find single target method for `sin(::String)`") report_call(sin, (String,))
    @test_throws ErrorException("unable to find single target method for `sin(::String)`") @report_call sin("julia")
end

@testset "`get_package_file`" begin
    using Pkg, JET
    using JET: get_package_file

    target = pathof(JET)

    @test get_package_file("JET") == target
    @test_throws ErrorException get_package_file("unknown")

    @test get_package_file(JET) == target
    @test_throws ErrorException get_package_file(Module())

    # suppress logs from Pkg.jl if possible
    pkg_activate(args...; io = IOBuffer(), kwargs...) =
        Pkg.activate(args...; io, kwargs...)
    old = Pkg.project().path
    try
        pkg_activate(pkgdir(JET))
        @test get_package_file(nothing) == target

        pkg_activate(; temp = true)
        @test_throws ErrorException get_package_file(nothing)
    catch err
        rethrow(err)
    finally
        pkg_activate(old)
    end
end

using Base.TOML
using JET: process_config_dict!
macro toml_str(s); TOML.parse(TOML.Parser(s)); end

@testset "`process_config_dict`" begin
    let
        config_dict = toml"""
        # usual
        analyze_from_definitions = true

        # will be `parse`d or `eval`ed
        context = "Base"
        concretization_patterns = ["const x_ = y_"]
        toplevel_logger = "stdout"
        """

        config = process_config_dict!(config_dict)
        @test (:context => Base) in config
        @test (:concretization_patterns => [:(const x_ = y_)]) in config
        @test (:toplevel_logger => stdout) in config
    end

    # error when invalid expression given
    let
        config_dict = toml"""
        concretization_patterns = ["const x_ = end"]
        """
        @test_throws Meta.ParseError process_config_dict!(config_dict)
    end

    # error when incomplete expression given
    let
        config_dict = toml"""
        concretization_patterns = ["const x_ = "]
        """
        @test_throws ErrorException process_config_dict!(config_dict)
    end

    # should be whitespece/newline insensitive
    let
        config_dict = toml"""
        concretization_patterns = [
            \"\"\"
            const x_ = y_
            \"\"\"
        ]
        """

        config = process_config_dict!(config_dict)
        @test (:concretization_patterns => [:(const x_ = y_)]) in config
    end
end
