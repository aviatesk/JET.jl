module test_misc

using JET, Test

function lineinfo_codelocs(locs::NTuple{3,Int32}...; firstline::Int32=Int32(0))
    data = Vector{Int32}(undef, 3length(locs))
    for (i, loc) in pairs(locs)
        data[3i - 2] = loc[1]
        data[3i - 1] = loc[2]
        data[3i] = loc[3]
    end
    return ccall(:jl_compress_codelocs, Any,
        (Int32, Any, Int), firstline, data, length(locs))::String
end
lineinfo_loc(line::Int, to::Int=0, pc::Int=0) =
    (Int32(line), Int32(to), Int32(pc))

function reference_get_lin(mi::Core.MethodInstance, src::Core.CodeInfo, pc::Int)
    lins = JET.CC.IRShow.buildLineInfoNode(src.debuginfo, mi, pc)
    if isempty(lins)
        return JET.CC.IRShow.LineInfoNode(
            mi, src.debuginfo.def::Symbol, mi.def.line)
    end
    return first(lins)
end

lineinfo_sample(x::Int) = x + 1
function lineinfo_source(debuginfo::Core.DebugInfo)
    src = copy(only(code_typed(lineinfo_sample, (Int,); optimize=false))[1])
    mi = src.parent::Core.MethodInstance
    src.debuginfo = debuginfo
    return mi, src
end

@testset "line info" begin
    direct = Core.DebugInfo(:direct_file, nothing, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(10), lineinfo_loc(20)))
    let (mi, src) = lineinfo_source(direct)
        @test JET._get_lin(mi, src, 1) == reference_get_lin(mi, src, 1)
        @test JET._get_lin(mi, src, 2) == reference_get_lin(mi, src, 2)
    end

    empty = Core.DebugInfo(:empty_file, nothing, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(0)))
    let (mi, src) = lineinfo_source(empty)
        @test JET._get_lin(mi, src, 1) == reference_get_lin(mi, src, 1)
    end

    linetable = Core.DebugInfo(:linetable_file, nothing, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(42)))
    indirect = Core.DebugInfo(:indirect_file, linetable, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(1)))
    let (mi, src) = lineinfo_source(indirect)
        @test JET._get_lin(mi, src, 1) == reference_get_lin(mi, src, 1)
    end

    edge = Core.DebugInfo(:edge_file, nothing, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(30)))
    inlined = Core.DebugInfo(:inlined_file, nothing, Core.svec(edge),
        lineinfo_codelocs(lineinfo_loc(10, 1, 1)))
    let (mi, src) = lineinfo_source(inlined)
        @test JET._get_lin(mi, src, 1) == reference_get_lin(mi, src, 1)
    end

    invalid_edge = Core.DebugInfo(:invalid_edge_file, nothing, Core.svec(),
        lineinfo_codelocs(lineinfo_loc(30)))
    invalid_inlined = Core.DebugInfo(:invalid_inlined_file, nothing,
        Core.svec(invalid_edge), lineinfo_codelocs(lineinfo_loc(10, 1, 2)))
    let (mi, src) = lineinfo_source(invalid_inlined)
        @test JET._get_lin(mi, src, 1) == reference_get_lin(mi, src, 1)
    end
end

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
