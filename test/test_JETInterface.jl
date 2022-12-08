module InterfaceTest

using JET.JETInterface, JET, Test
import JET: get_reports, BasicPass, UndefVarErrorReport

# customized report pass
# ======================

struct IgnoreAllPass <: ReportPass end
(::IgnoreAllPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
let result = @report_call report_pass=IgnoreAllPass() sum("julia")
    @test isempty(get_reports(result))
end

struct IgnoreAllExceptGlobalUndefVarPass <: ReportPass end
(::IgnoreAllExceptGlobalUndefVarPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
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

function compute_sins(i)
    n = 1000000
    out = Vector{Float64}(undef, n)
    Threads.@threads for itr in 1:n
        i = itr
        out[i] = sin(i)
    end
    return out
end

const ERROR_MSG = "missing `$AbstractAnalyzer` API"

@test_throws ERROR_MSG @report_call analyzer=APIValidator compute_sins(10)

# interface 1: `APIValidator(; jetconfigs...) -> APIValidator`
APIValidator(; jetconfigs...) = APIValidator(AnalyzerState(; jetconfigs...), AnalysisCache())

@test_throws ERROR_MSG @report_call analyzer=APIValidator compute_sins(10)

# interface 2: `AnalyzerState(analyzer::APIValidator) -> AnalyzerState`
JETInterface.AnalyzerState(analyzer::APIValidator) = analyzer.state

@test_throws ERROR_MSG @report_call analyzer=APIValidator compute_sins(10)

# interface 3: `AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) -> APIValidator`
JETInterface.AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) = APIValidator(state, analyzer.analysis_cache)

@test_throws ERROR_MSG @report_call analyzer=APIValidator compute_sins(10)

# interface 4: `ReportPass(analyzer::APIValidator) -> ReportPass`
JETInterface.ReportPass(analyzer::APIValidator) = IgnoreAllPass()

@test_throws ERROR_MSG @report_call analyzer=APIValidator compute_sins(10)

# interface 5: `AnalysisCache(analyzer::APIValidator) -> AnalysisCache`
JETInterface.AnalysisCache(analyzer::APIValidator) = analyzer.analysis_cache

# because `APIValidator` uses `IgnoreAllPass`, we won't get any reports
let result = @report_call analyzer=APIValidator compute_sins(10)
    @test isempty(get_reports(result))
end

end # module InterfaceTest
