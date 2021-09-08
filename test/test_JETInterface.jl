module InterfaceTest

using JET.JETInterface, JET, Test
import JET: get_reports

# customized report pass
# ======================

struct IgnoreAllPass <: ReportPass end
(::IgnoreAllPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
let
    result = @report_call report_pass=IgnoreAllPass() sum("julia")
    @test isempty(get_reports(result))
end

struct IgnoreAllExceptGlobalUndefVar <: ReportPass end
(::IgnoreAllExceptGlobalUndefVar)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
function (::IgnoreAllExceptGlobalUndefVar)(::Type{GlobalUndefVarErrorReport}, @nospecialize(args...))
    BasicPass()(GlobalUndefVarErrorReport, args...)
end
let
    result = report_call(; report_pass=IgnoreAllExceptGlobalUndefVar()) do
        sum("julia") # should be ignored
        undefvar
    end
    @test length(get_reports(result)) == 1
    let r = first(get_reports(result))
        @test isa(r, GlobalUndefVarErrorReport)
        @test r.name === :undefvar
    end
end

# AbstractAnalyzer API
# ====================

# NOTE actual functionalities are tested by documentation

struct APIValidator <: AbstractAnalyzer
    state::AnalyzerState
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

@test_throws ErrorException @report_call analyzer=APIValidator compute_sins(10)

# interface 1: `APIValidator(; jetconfigs...) -> APIValidator`
APIValidator(; jetconfigs...) = APIValidator(AnalyzerState(; jetconfigs...))

@test_throws ErrorException @report_call analyzer=APIValidator compute_sins(10)

# interface 2: `AnalyzerState(analyzer::APIValidator) -> AnalyzerState`
JETInterface.AnalyzerState(analyzer::APIValidator) = analyzer.state

@test_throws ErrorException @report_call analyzer=APIValidator compute_sins(10)

# interface 3: `AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) -> APIValidator`
JETInterface.AbstractAnalyzer(analyzer::APIValidator, state::AnalyzerState) = APIValidator(state)

@test_throws ErrorException @report_call analyzer=APIValidator compute_sins(10)

# interface 4: `ReportPass(analyzer::APIValidator) -> ReportPass`
JETInterface.ReportPass(analyzer::APIValidator) = IgnoreAllPass()

@test_throws ErrorException @report_call analyzer=APIValidator compute_sins(10)

# interface 5: `get_cache_key(analyzer::APIValidator) -> UInt`
JETInterface.get_cache_key(analyzer::APIValidator) = AnalyzerState(analyzer).param_key

# because `APIValidator` uses `IgnoreAllPass`, we won't get any reports
let
    result = @report_call analyzer=APIValidator compute_sins(10)
    @test isempty(get_reports(result))
end

end # module InterfaceTest
