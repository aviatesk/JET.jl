module test_generatedeffectsanalyzer

include("../setup.jl")

# Mismatched effects: generated branch is pure (returns a constant),
# fallback branch has side effects (println)
function bad_generated(x::T) where T
    if @generated
        val = sizeof(T)
        return :($val)
    else
        println("fallback")
        return sizeof(T)
    end
end

# No @generated: should produce no reports
function plain_function(x)
    return x + 1
end

# Matching side effects in both branches
function matching_sideeffects(x::T) where T
    if @generated
        return :(println("gen: ", x))
    else
        println("fallback: ", x)
    end
end

@testset "mismatch detection" begin
    # A generated function whose generated branch is pure but fallback has
    # side effects should be detected as a mismatch
    let result = report_generated_effects(bad_generated, (Int,))
        reports = get_reports(result)
        @test length(reports) ≥ 1
        @test reports[1] isa GeneratedEffectsMismatchReport
        @test reports[1].field === :consistent
    end
end

@testset "no false positives" begin
    # Non-generated functions produce no reports
    let result = report_generated_effects(plain_function, (Int,))
        @test isempty(get_reports(result))
    end

    # Generated function with matching effects in both branches produces no reports
    let result = report_generated_effects(matching_sideeffects, (Int,))
        @test isempty(get_reports(result))
    end
end

@testset "@test_generated_effects macro" begin
    @test_generated_effects matching_sideeffects(42)
    @test_generated_effects plain_function(42)
end

@testset "test_generated_effects function" begin
    test_generated_effects(matching_sideeffects, (Int,))
    test_generated_effects(plain_function, (Int,))
end

@testset "report message format" begin
    let result = report_generated_effects(bad_generated, (Int,))
        reports = get_reports(result)
        @test length(reports) ≥ 1
        msg = sprint(JET.print_report_message, reports[1])
        @test occursin("if @generated", msg)
        @test occursin("consistent", msg)
    end
end

end # module
