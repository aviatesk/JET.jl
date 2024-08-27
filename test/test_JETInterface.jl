module test_JETInterface

using JET.JETInterface, JET, Test, InteractiveUtils
using JET: get_reports, BasicPass, UndefVarErrorReport

# customized report pass
# ======================

struct IgnoreAllPass <: ReportPass end
(::IgnoreAllPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = false
let result = @report_call report_pass=IgnoreAllPass() sum("julia")
    @test isempty(get_reports(result))
end

struct IgnoreAllExceptGlobalUndefVarPass <: ReportPass end
(::IgnoreAllExceptGlobalUndefVarPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = false
(::IgnoreAllExceptGlobalUndefVarPass)(::Type{UndefVarErrorReport}, @nospecialize(args...)) = BasicPass()(UndefVarErrorReport, args...)
let result = report_call(; report_pass=IgnoreAllExceptGlobalUndefVarPass()) do
        sum("julia") # should be ignored
        undefvar
    end
    let r = only(get_reports(result))
        @test isa(r, UndefVarErrorReport) && isa(r.var, GlobalRef) && r.var.name === :undefvar
    end
end

# AbstractAnalyzer API
# ====================

# NOTE actual functionalities are tested by documentation

struct APIValidator <: AbstractAnalyzer
    state::AnalyzerState
    analysis_cache::AnalysisCache
end

APIValidator(world::UInt=Base.get_world_counter(); jetconfigs...) =
    APIValidator(AnalyzerState(world; jetconfigs...), AnalysisCache())
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
JETInterface.AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) = APIValidator(state, analyzer.analysis_cache)

@test_throws ERROR_MSG @report_apivalidator compute_sins(10)

# interface 3: `ReportPass(analyzer::APIValidator) -> ReportPass`
JETInterface.ReportPass(analyzer::APIValidator) = IgnoreAllPass()

@test_throws ERROR_MSG @report_apivalidator compute_sins(10)

# interface 4: `AnalysisCache(analyzer::APIValidator) -> AnalysisCache`
JETInterface.AnalysisCache(analyzer::APIValidator) = analyzer.analysis_cache

# because `APIValidator` uses `IgnoreAllPass`, we won't get any reports
let result = @report_apivalidator compute_sins(10)
    @test isempty(get_reports(result))
end

end # module test_JETInterface
