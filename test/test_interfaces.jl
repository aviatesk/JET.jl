using JET.JETInterfaces

# customized report pass
# ======================

struct IgnoreAllPass <: ReportPass end
(::IgnoreAllPass)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
let
    analyzer, frame = @analyze_call report_pass=IgnoreAllPass() sum("julia")
    @test isempty(get_reports(analyzer))
end

struct IgnoreAllExceptGlobalUndefVar <: ReportPass end
(::IgnoreAllExceptGlobalUndefVar)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return
function (::IgnoreAllExceptGlobalUndefVar)(::Type{GlobalUndefVarErrorReport}, @nospecialize(args...))
    BasicPass()(GlobalUndefVarErrorReport, args...)
end
let
    analyzer, frame = analyze_call(; report_pass=IgnoreAllExceptGlobalUndefVar()) do
        sum("julia") # should be ignored
        undefvar
    end
    @test length(get_reports(analyzer)) == 1
    let r = first(get_reports(analyzer))
        @test isa(r, GlobalUndefVarErrorReport)
        @test r.name === :undefvar
    end
end

# pluggable-analysis framework
# ============================

# NOTE this will be tested by documentation
