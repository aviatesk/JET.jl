# this file defines stuff used across JET's test suite
using Test

# test utilities are extracted into a separate file for easier interactive testing from REPL
# i.e. julia -i test/interactive_utils.jl
include("interactive_utils.jl")

function get_reports_with_test(args...)
    reports = get_reports(args...)
    buf = IOBuffer()
    print_reports(IOContext(buf, :color=>true), reports)
    @test !isempty(String(take!(buf)))
    return reports
end

const FIXTURE_DIR = normpath(@__DIR__, "fixtures")

const ERROR_REPORTS_FROM_SUM_OVER_STRING = let
    result = report_call(sum, (String,))
    @test !isempty(get_reports_with_test(result))
    get_reports_with_test(result)
end

get_msg(report::JET.InferenceErrorReport) = sprint(JET.print_report, report)

function test_sum_over_string(ers)
    @test !isempty(ers)
    for target in ERROR_REPORTS_FROM_SUM_OVER_STRING
        @test any(ers) do er
            return get_msg(er) == get_msg(target) && er.sig == target.sig
        end
    end
    return true
end
test_sum_over_string(res::JET.JETCallResult) = test_sum_over_string(get_reports_with_test(res))
test_sum_over_string(res::JET.JETToplevelResult) = test_sum_over_string(get_reports_with_test(res))
