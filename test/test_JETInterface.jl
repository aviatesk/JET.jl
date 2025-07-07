module test_JETInterface

using JET.JETInterface, JET, Test, InteractiveUtils
using JET: get_reports, UndefVarErrorReport

# customized analyzer example
# ===========================

# Example: create an analyzer that ignores all reports
struct IgnoreAllAnalyzer <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
end

IgnoreAllAnalyzer(world::UInt=Base.get_world_counter(); jetconfigs...) =
    IgnoreAllAnalyzer(AnalyzerState(world; jetconfigs...), AnalysisToken())

# API requirements
JETInterface.AnalyzerState(analyzer::IgnoreAllAnalyzer) = analyzer.state
JETInterface.AbstractAnalyzer(analyzer::IgnoreAllAnalyzer, state::AnalyzerState) = 
    IgnoreAllAnalyzer(state, analyzer.analysis_token)
JETInterface.AnalysisToken(analyzer::IgnoreAllAnalyzer) = analyzer.analysis_token

# This analyzer ignores all reports by not implementing any report methods

function report_call_ignore_all(args...; jetconfigs...)
    @nospecialize args jetconfigs
    analyzer = IgnoreAllAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end

let result = report_call_ignore_all(sum, ("julia",))
    @test isempty(get_reports(result))
end

# AbstractAnalyzer API
# ====================

# NOTE actual functionalities are tested by documentation

struct APIValidator <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
end

APIValidator(world::UInt=Base.get_world_counter(); jetconfigs...) =
    APIValidator(AnalyzerState(world; jetconfigs...), AnalysisToken())
function report_apivalidator(args...; jetconfigs...)
    @nospecialize args jetconfigs
    analyzer = APIValidator(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end
macro report_apivalidator(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_apivalidator, ex0)
end

function compute_sins(i)
    n = 1000000
    out = Vector{Float64}(undef, n)
    Threads.@threads for itr in 1:n
        i = itr
        out[i] = sin(i)
    end
    return out
end

const ERROR_MSG = "Missing `$AbstractAnalyzer` API"

@test_throws ERROR_MSG @report_apivalidator compute_sins(10)

# interface 1: `AnalyzerState(analyzer::APIValidator) -> AnalyzerState`
JETInterface.AnalyzerState(analyzer::APIValidator) = analyzer.state

@test_throws ERROR_MSG @report_apivalidator compute_sins(10)

# interface 2: `AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) -> APIValidator`
JETInterface.AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) = APIValidator(state, analyzer.analysis_token)

@test_throws ERROR_MSG @report_apivalidator compute_sins(10)

# interface 3: `AnalysisToken(analyzer::APIValidator) -> AnalysisToken`
JETInterface.AnalysisToken(analyzer::APIValidator) = analyzer.analysis_token

# `APIValidator` doesn't implement any report methods, so we won't get any reports
let result = @report_apivalidator compute_sins(10)
    @test isempty(get_reports(result))
end

end # module test_JETInterface
