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

using JET: process_config_dict
using TOML: TOML
macro toml_str(s); TOML.parse(TOML.Parser(s)); end

@testset "`process_config_dict`" begin
    let config_dict = toml"""
        # usual
        analyze_from_definitions = "main"
        mode = "typo"
        sourceinfo = "compact"
        target_modules = ["Foo"]
        ignored_modules = ["Bar"]

        # will be `parse`d or `eval`ed
        context = "Base"
        concretization_patterns = ["const x_ = y_"]
        toplevel_logger = "stdout"
        """

        config = process_config_dict(config_dict)
        @test (:analyze_from_definitions => :main) in config
        @test (:mode => :typo) in config
        @test (:sourceinfo => :compact) in config
        @test (:target_modules => [:Foo]) in config
        @test (:ignored_modules => [:Bar]) in config
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

@testset "configuration file selection" begin
    mktempdir() do dir
        config_file = normpath(dir, JET.CONFIG_FILE_NAME)
        write(config_file, "mode = \"typo\"\nignore_throws = true\n")
        child = mkpath(normpath(dir, "child"))

        cd(dir) do
            result = report_call(()->throw("foo"))
            @test result.analyzer isa JET.TypoJETAnalyzer
            @test isempty(JET.get_reports(result))
        end

        cd(child) do
            result = report_call(()->throw("foo"))
            @test result.analyzer isa JET.BasicJETAnalyzer
            @test !isempty(JET.get_reports(result))

            result = report_call(()->throw("foo"); config_file)
            @test result.analyzer isa JET.TypoJETAnalyzer
            @test isempty(JET.get_reports(result))

            result = report_call(()->throw("foo");
                config_file, mode=:basic, ignore_throws=false)
            @test result.analyzer isa JET.BasicJETAnalyzer
            @test !isempty(JET.get_reports(result))

            result = report_call(()->throw("foo"); config_file=nothing)
            @test result.analyzer isa JET.BasicJETAnalyzer
            @test !isempty(JET.get_reports(result))
        end

        missing = normpath(dir, "missing.toml")
        @test_throws ArgumentError report_call(identity, (Any,); config_file=missing)
    end
end

@testset "configuration validation" begin
    # https://github.com/aviatesk/JET.jl/issues/414
    @test_throws "lkdsjkdlkas" report_call(+, (Int, Int), lkdsjkdlkas=true)
    @test_throws "target_module" @report_call target_module=(Core,) sum(Char[])
    @test_throws "foo = :bar" @report_call foo = :bar sum(Char[])
end


end # module test_misc
