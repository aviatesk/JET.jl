# GeneratedEffectsAnalyzer: checks that `if @generated` functions have consistent
# effects between the generated and fallback branches, so the compiler cannot
# unsoundly remove code from either branch based on the other's effects.

"""
Every [entry point of generated effects analysis](@ref generatedeffectsanalysis-entry)
can accept any of the [general configurations](@ref).

The generated effects analyzer detects `if @generated` functions in the call graph and
verifies that the inferred effects of the generated branch (for concrete types) match
the effects of the fallback branch (for abstract types). A mismatch could lead to
unsound optimizations where the compiler removes code from one branch based on stronger
effects inferred from the other branch.

The `nonoverlayed` effect is excluded from comparison since it legitimately differs:
concrete types can prove no method overlays exist, while abstract types conservatively
cannot. This is benign because `nonoverlayed` only gates concrete evaluation and never
causes code removal.
"""
struct GeneratedEffectsAnalyzer <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    __checked_methods::Set{Method} # avoid duplicate reports for the same generator method
end

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::GeneratedEffectsAnalyzer) = analyzer.state
function JETInterface.AbstractAnalyzer(analyzer::GeneratedEffectsAnalyzer, state::AnalyzerState)
    return GeneratedEffectsAnalyzer(state, analyzer.analysis_token, analyzer.__checked_methods)
end
JETInterface.AnalysisToken(analyzer::GeneratedEffectsAnalyzer) = analyzer.analysis_token
JETInterface.typeinf_world(::GeneratedEffectsAnalyzer) = JET_TYPEINF_WORLD[]
JETInterface.vscode_diagnostics_order(::GeneratedEffectsAnalyzer) = false

const GEN_EFFECTS_ANALYZER_CACHE = Dict{UInt,AnalysisToken}()
const GEN_EFFECTS_ANALYZER_CACHE_LOCK = ReentrantLock()

function GeneratedEffectsAnalyzer(world::UInt = Base.get_world_counter(); jetconfigs...)
    state = AnalyzerState(world; jetconfigs...)
    cache_key = compute_hash(state.inf_params, state.opt_params, GeneratedEffectsAnalyzer)
    analysis_token = @lock GEN_EFFECTS_ANALYZER_CACHE_LOCK get!(AnalysisToken, GEN_EFFECTS_ANALYZER_CACHE, cache_key)
    return GeneratedEffectsAnalyzer(state, analysis_token, Set{Method}())
end

let valid_keys = GENERAL_CONFIGURATIONS
    @eval JETInterface.valid_configurations(::GeneratedEffectsAnalyzer) = $valid_keys
end

# Report type
# ===========

@jetreport struct GeneratedEffectsMismatchReport <: InferenceErrorReport
    field::Symbol
    @nospecialize generated_value
    @nospecialize fallback_value
end
function JETInterface.print_report_message(io::IO, r::GeneratedEffectsMismatchReport)
    print(io, "`if @generated` effect mismatch: `", r.field, "` differs (generated=", r.generated_value, ", fallback=", r.fallback_value, ")")
end
JETInterface.report_color(::GeneratedEffectsMismatchReport) = :yellow

# Analysis hooks
# ==============

# Hook into finishinfer! to check effects after inference completes for each frame
@static if VERSION ≥ v"1.13.0-DEV.565"
function CC.finishinfer!(frame::InferenceState, analyzer::GeneratedEffectsAnalyzer, cycleid::Int,
                         opt_cache::IdDict{MethodInstance, CodeInstance})
    ret = @invoke CC.finishinfer!(frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int,
                                  opt_cache::IdDict{MethodInstance, CodeInstance})
    check_generated_effects!(analyzer, frame)
    return ret
end
else
function CC.finishinfer!(frame::InferenceState, analyzer::GeneratedEffectsAnalyzer, cycleid::Int)
    ret = @invoke CC.finishinfer!(frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int)
    check_generated_effects!(analyzer, frame)
    return ret
end
end

function check_generated_effects!(analyzer::GeneratedEffectsAnalyzer, frame::InferenceState)
    mi = frame.linfo
    m = mi.def
    m isa Method || return

    # Only check methods with generators (i.e., @generated / if @generated)
    isdefined(m, :generator) || return

    # Avoid duplicate checks for the same method (different specializations)
    m in analyzer.__checked_methods && return
    push!(analyzer.__checked_methods, m)

    # Build concrete and abstract type signatures for comparison.
    # Concrete types trigger the generated branch, abstract types trigger the fallback.
    concrete_types = mi.specTypes
    abstract_types = method_sig_bounds(m)

    if abstract_types === nothing || abstract_types === concrete_types
        return # already at the bounds, skip
    end

    # Split specTypes Tuple{F, Args...} into function type and arg types
    concrete_params = concrete_types.parameters
    abstract_params = abstract_types.parameters
    f_type = concrete_params[1]
    concrete_argtypes = Tuple{concrete_params[2:end]...}
    abstract_argtypes = Tuple{abstract_params[2:end]...}

    # Get a function instance for infer_effects (singleton types have a unique instance)
    f = try
        f_type.instance
    catch
        return # non-singleton function type, skip
    end

    # Infer effects for both branches
    world = CC.get_inference_world(analyzer)
    gen_effects = try
        Base.infer_effects(f, concrete_argtypes; world)
    catch
        return
    end
    fallback_effects = try
        Base.infer_effects(f, abstract_argtypes; world)
    catch
        return # if inference fails, skip
    end

    # Compare all effect fields except nonoverlayed
    for field in fieldnames(CC.Effects)
        field === :nonoverlayed && continue
        g = getfield(gen_effects, field)
        f = getfield(fallback_effects, field)
        if g != f
            add_new_report!(analyzer, frame.result,
                GeneratedEffectsMismatchReport(mi, field, g, f))
        end
    end
end

"""
    method_sig_bounds(m::Method) -> Type{<:Tuple} or nothing

Extract the declared parameter bounds from a method's signature, resolving any
TypeVars to their upper bounds. This gives the abstract type signature that triggers
the fallback branch of `if @generated` functions.
"""
function method_sig_bounds(m::Method)
    sig = m.sig
    # Collect TypeVars and their upper bounds
    tvars = Dict{TypeVar, Any}()
    while sig isa UnionAll
        tvars[sig.var] = sig.var.ub
        sig = sig.body
    end
    # sig is now the body DataType with potentially free TypeVars
    params = sig.parameters
    length(params) < 2 && return nothing
    new_params = Any[]
    for p in params
        push!(new_params, _resolve_tvar(p, tvars))
    end
    return Tuple{new_params...}
end

function _resolve_tvar(@nospecialize(T), tvars::Dict{TypeVar, Any})
    if T isa TypeVar
        return get(tvars, T, T.ub)
    else
        return T
    end
end

# Entry points
# ============

"""
    report_generated_effects(f, [types]; jetconfigs...) -> JETCallResult
    report_generated_effects(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult

Analyzes a function call to detect `if @generated` functions with inconsistent effects
between the generated and fallback branches.

See also: [`@report_generated_effects`](@ref)
"""
function report_generated_effects(args...; jetconfigs...)
    analyzer = GeneratedEffectsAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end

"""
    @report_generated_effects [jetconfigs...] f(args...)

Evaluates the arguments to a function call, determines their types, and then calls
[`report_generated_effects`](@ref) on the resulting expression.
"""
macro report_generated_effects(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_generated_effects, ex0)
end

"""
    @test_generated_effects [jetconfigs...] [broken=false] [skip=false] f(args...)

Tests that the function call `f(args...)` is free from `if @generated` effects mismatches.
"""
macro test_generated_effects(ex0...)
    return call_test_ex(:report_generated_effects, Symbol("@test_generated_effects"), ex0, __module__, __source__)
end

"""
    test_generated_effects(f, [types]; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_generated_effects`](@ref) on a function call with the given type signature
and tests that it is free from `if @generated` effects mismatches.
"""
function test_generated_effects(@nospecialize(args...); jetconfigs...)
    return func_test(report_generated_effects, :test_generated_effects, args...; jetconfigs...)
end
