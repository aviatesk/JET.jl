"""
Every [entry point of optimization analysis](@ref optanalysis-entry) accepts
any of the [general configurations](@ref general-configurations), together with
the following configurations specific to optimization analysis.

---
- `skip_noncompileable_calls::Bool = true`:\\
  A call with an abstract inferred signature may not be compileable as-is, but
  this does not necessarily make its downstream code inefficient. At runtime,
  Julia dispatches using concrete argument types and can compile specialized
  code for a
  ["kernel" function](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions).
  The kernel can therefore run efficiently even when reached from a
  type-unstable call site. Idiomatic Julia code may intentionally use such
  boundaries, relying on information available only at runtime rather than
  requiring inference to continue through them.

  To model this programming style, `OptAnalyzer` does not, by default, report
  optimization failures or runtime dispatches found inside calls that Julia
  would not compile for their inferred signatures. Such calls are often
  non-concrete, although the more precise criterion is whether a call is
  compileable; see the note below. The entry call itself is always analyzed.
  Set `skip_noncompileable_calls=false` to include reports from inside those
  calls.

  The following example is adapted from Julia's
  [kernel-function documentation](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions):
  ```julia-repl
  julia> function fill_twos!(a)
             for i = eachindex(a)
                 a[i] = 2
             end
         end;

  julia> function strange_twos(a::Vector)
             fill_twos!(a)
             return a
         end;

  # Analyze `strange_twos` with the abstract `Vector` entry signature.
  julia> report_opt(strange_twos, (Vector,))
  ═════ 1 possible error found ═════
  ┌ strange_twos(a::Vector) @ Main ./REPL[2]:2
  │ runtime dispatch detected: fill_twos!(a::Vector)::Any
  └────────────────────

  # Also include reports from inside non-compileable calls.
  julia> report_opt(strange_twos, (Vector,);
                    skip_noncompileable_calls=false)
  ═════ 5 possible errors found ═════
  ┌ strange_twos(a::Vector) @ Main ./REPL[2]:2
  │┌ fill_twos!(a::Vector) @ Main ./REPL[1]:3
  ││┌ setindex!(A::Vector, x::Int64, i::Int64) @ Base ./array.jl:986
  │││┌ _setindex!(A::Vector{T}, x::Any, i::Int64) where T @ Base ./array.jl:990
  ││││ runtime dispatch detected: Base.throw_boundserror(A::Vector, %12::Tuple{Int64})
  │││└────────────────────
  ││┌ setindex!(A::Vector, x::Int64, i::Int64) @ Base ./array.jl:985
  │││ runtime dispatch detected: convert(%5::Any, x::Int64)::Any
  ││└────────────────────
  ││┌ setindex!(A::Vector, x::Int64, i::Int64) @ Base ./array.jl:986
  │││ runtime dispatch detected: Base._setindex!(A::Vector, %9::Any, i::Int64)::Vector
  ││└────────────────────
  │┌ fill_twos!(a::Vector) @ Main ./REPL[1]:3
  ││ runtime dispatch detected: ((a::Vector)[%13::Int64] = 2::Any)
  │└────────────────────
  ┌ strange_twos(a::Vector) @ Main ./REPL[2]:2
  │ runtime dispatch detected: fill_twos!(a::Vector)::Any
  └────────────────────
  ```

  With the default setting, JET reports the runtime dispatch from the entry
  call to `fill_twos!(::Vector)` but omits reports from inside `fill_twos!`.
  With `skip_noncompileable_calls=false`, JET also reports runtime dispatches
  encountered while analyzing the body of `fill_twos!(::Vector)`.

  !!! note "Non-compileable calls"
      Julia runtime system sometimes generate and execute native code of an abstract call.
      More technically, when some of call arguments are annotated as `@nospecialize`,
      Julia compiles the call even if those `@nospecialize`d arguments aren't fully concrete.
      `skip_noncompileable_calls = true` also respects this behavior, i.e. doesn't skip
      compileable abstract calls:
      ```julia-repl
      julia> function maybesin(x)
                 if isa(x, Number)
                     return sin(x)
                 else
                     return 0
                 end
             end;

      julia> report_opt((Vector{Any},)) do xs
                 for x in xs
                     # This `maybesin` call is dynamically dispatched since `maybesin(::Any)`
                     # is not compileable. Therefore, JET by default will only report the
                     # runtime dispatch of `maybesin` while it will not report the runtime
                     # dispatch within `maybesin(::Any)`.
                     s = maybesin(x)
                     s !== 0 && return s
                 end
             end
      ═════ 1 possible error found ═════
      ┌ (::var"#3#4")(xs::Vector{Any}) @ Main ./REPL[3]:7
      │ runtime dispatch detected: maybesin(%19::Any)::Any
      └────────────────────

      julia> function maybesin(@nospecialize x) # mark `x` with `@nospecialize`
                 if isa(x, Number)
                     return sin(x)
                 else
                     return 0
                 end
             end;

      julia> report_opt((Vector{Any},)) do xs
                 for x in xs
                     # Now `maybesin` is marked with `@nospecialize` allowing `maybesin(::Any)`
                     # to be resolved statically and compiled. Thus JET will not report the
                     # runtime dispatch of `maybesin(::Any)`, although it now reports the
                     # runtime dispatch _within_ `maybesin(::Any)`.
                     s = maybesin(x)
                     s !== 0 && return s
                 end
             end
      ═════ 1 possible error found ═════
      ┌ (::var"#5#6")(xs::Vector{Any}) @ Main ./REPL[5]:7
      │┌ maybesin(x::Any) @ Main ./REPL[4]:3
      ││ runtime dispatch detected: sin(%3::Number)::Any
      │└────────────────────
      ```

---
- `function_filter = @nospecialize(f)->true`:\\
  A predicate which takes a function object and returns `false` to skip runtime dispatch
  analysis on calls of the function. This configuration is particularly useful when your
  program uses a function that is intentionally designed to use runtime dispatch.

  ```julia-repl
  # Ignore `Compiler.widenconst`, which intentionally uses runtime dispatch.
  julia> function_filter(@nospecialize f) = f !== Compiler.widenconst;

  julia> @test_opt function_filter=function_filter f(args...)
  ...
  ```
---
"""
struct OptAnalyzer{FF} <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    function_filter::FF
    skip_noncompileable_calls::Bool
end

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::OptAnalyzer) = analyzer.state
function JETInterface.AbstractAnalyzer(analyzer::OptAnalyzer, state::AnalyzerState)
    return OptAnalyzer(
        state,
        analyzer.analysis_token,
        analyzer.function_filter,
        analyzer.skip_noncompileable_calls)
end
JETInterface.AnalysisToken(analyzer::OptAnalyzer) = analyzer.analysis_token
JETInterface.typeinf_world(::OptAnalyzer) = JET_TYPEINF_WORLD[]
JETInterface.vscode_diagnostics_order(::OptAnalyzer) = false

const OPT_ANALYZER_CACHE = Dict{UInt,AnalysisToken}()
const OPT_ANALYZER_CACHE_LOCK = ReentrantLock()

# overloads
# =========

function CC.const_prop_call(analyzer::OptAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
    concrete_eval_result::Union{Nothing,ConstCallResult})
    ret = @invoke CC.const_prop_call(analyzer::AbstractAnalyzer,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
        concrete_eval_result::Union{Nothing,ConstCallResult})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        filter_lineages!(analyzer, sv, result.edge.def)
    end
    return ret
end
function CC.const_prop_call(analyzer::OptAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::CC.IRInterpretationState,
    concrete_eval_result::Union{Nothing,ConstCallResult})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        return concrete_eval_result
    end
    return @invoke CC.const_prop_call(analyzer::AbstractInterpreter,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::CC.IRInterpretationState,
        nothing::Nothing)
end

# TODO better to work only with `CC.finish!`
@static if VERSION ≥ v"1.13.0-DEV.565"
function CC.finishinfer!(frame::InferenceState, analyzer::OptAnalyzer, cycleid::Int,
                         opt_cache::IdDict{MethodInstance, CodeInstance})
    ret = @invoke CC.finishinfer!(frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int,
                                  opt_cache::IdDict{MethodInstance, CodeInstance})
    finishinfer!_overload(frame, analyzer)
    return ret
end
else
function CC.finishinfer!(frame::InferenceState, analyzer::OptAnalyzer, cycleid::Int)
    ret = @invoke CC.finishinfer!(frame::InferenceState, analyzer::AbstractAnalyzer, cycleid::Int)
    finishinfer!_overload(frame, analyzer)
    return ret
end
end

function is_analysis_target(analyzer::OptAnalyzer, caller::InferenceResult)
    analyzer.skip_noncompileable_calls || return true
    mi = caller.linfo
    return is_compileable_mi(mi) || is_entry(analyzer, mi)
end

function should_analyze_initial(analyzer::OptAnalyzer, caller::InferenceResult)
    is_analysis_target(analyzer, caller) || return false
    return !CC.is_result_constabi_eligible(caller)
end

function finishinfer!_overload(frame::InferenceState, analyzer::OptAnalyzer)
    caller = frame.result
    is_analysis_target(analyzer, caller) || return nothing
    if CC.is_result_constabi_eligible(caller)
        # turn off optimization for this frame in order to achieve a minor perf gain,
        # similar to the effect of setting `may_discard_trees(::OptAnalyzer) = true`
        caller.src = nothing
    else
        report_captured_variable!(analyzer, frame)
    end
    return nothing
end

# analysis injections
# ===================

function optimized_ir(opt::OptimizationState)
    @static if isdefinedglobal(CC, :compute_inlining_cost)
        return (opt.optresult::CC.OptimizationResult).ir
    else
        return opt.ir::CC.IRCode
    end
end

function is_inlineable_optimized(
        analyzer::OptAnalyzer, caller::InferenceResult, opt::OptimizationState
    )
    @static if isdefinedglobal(CC, :compute_inlining_cost)
        optresult = opt.optresult::CC.OptimizationResult
        return CC.compute_inlining_cost(analyzer, caller, optresult) != CC.MAX_INLINE_COST
    else
        return CC.is_inlineable(opt.src)
    end
end

function should_analyze_optimized(
        analyzer::OptAnalyzer, caller::InferenceResult, opt::OptimizationState
    )
    CC.is_result_constabi_eligible(caller) && return false
    is_analysis_target(analyzer, caller) && return true
    return is_inlineable_optimized(analyzer, caller, opt)
end

function CC.optimize(
        analyzer::OptAnalyzer, opt::OptimizationState, caller::InferenceResult
    )
    ret = @invoke CC.optimize(
        analyzer::AbstractInterpreter, opt::OptimizationState, caller::InferenceResult)
    if should_analyze_optimized(analyzer, caller, opt)
        state = OptimizedIRState(opt, optimized_ir(opt))
        report_runtime_dispatch!(analyzer, caller, state)
    end
    return ret
end

function CC.finish!(
        analyzer::OptAnalyzer, frame::InferenceState, validation_world::UInt,
        time_before::UInt64
    )
    caller = frame.result
    if should_analyze_initial(analyzer, caller)
        report_optimization_failure!(analyzer, caller)
    end
    return @invoke CC.finish!(
        analyzer::AbstractAnalyzer, frame::InferenceState, validation_world::UInt,
        time_before::UInt64)
end

# analysis
# ========

@jetreport struct CapturedVariableReport <: InferenceErrorReport
    name::Union{Nothing,Symbol}
end
function JETInterface.print_report_message(io::IO, (; name)::CapturedVariableReport)
    if isnothing(name)
        print(io, "captured variable detected")
    else
        print(io, "captured variable `", name, "` detected")
    end
end
JETInterface.print_signature(::CapturedVariableReport) = false
function report_captured_variable!(analyzer::OptAnalyzer, frame::InferenceState)
    local reported = false
    code = frame.src.code
    for pc = 1:length(code)
        if CC.was_reached(frame, pc) && widenconst(ignorelimited(frame.src.ssavaluetypes[pc])) === Core.Box
            stmt = code[pc]
            if isexpr(stmt, :(=))
                var = stmt.args[1]
                if isa(var, SlotNumber)
                    name = frame.src.slotnames[slot_id(var)]
                else
                    name = nothing
                end
                add_new_report!(analyzer, frame.result, CapturedVariableReport((frame, pc), name))
                reported |= true
            end
        end
    end
    return reported
end

# report optimization failure due to recursive calls, etc.
@jetreport struct OptimizationFailureReport <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::OptimizationFailureReport)
    print(io, "failed to optimize due to recursion")
end
function report_optimization_failure!(analyzer::OptAnalyzer, caller::InferenceResult)
    if caller.src === nothing # the optimization didn't happen
        add_new_report!(analyzer, caller, OptimizationFailureReport(caller.linfo))
        return true
    end
    return false
end

@jetreport struct RuntimeDispatchReport <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::RuntimeDispatchReport)
    print(io, "runtime dispatch detected")
end
function report_runtime_dispatch!(
        analyzer::OptAnalyzer, caller::InferenceResult, state::OptimizedIRState
    )
    ir = state.ir
    local reported = false
    for (pc, inst) in enumerate(ir.stmts)
        stmt = inst[:stmt]
        if isexpr(stmt, :call)
            ft = argextype(first(stmt.args), ir)
            f = singleton_type(ft)
            if f !== nothing
                f isa Builtin && continue # ignore `:call`s of language intrinsics
                isnothing(analyzer.function_filter) ||
                    @invokelatest(analyzer.function_filter(f)) || continue
            end
            lins = get_lins((state, pc))
            isempty(lins) && continue # dead statement, just ignore it
            if length(lins) > 1
                # this statement has been inlined, so ignore it as any problems within
                # that callee should already have been reported
                continue
            end
            add_new_report!(analyzer, caller, RuntimeDispatchReport((state, pc)))
            reported |= true
        end
    end
    return reported
end

# entries
# =======

# the entry constructor
function OptAnalyzer(world::UInt = Base.get_world_counter();
    function_filter = Returns(true),
    skip_noncompileable_calls::Bool = true,
    __cache_hash__::Any = nothing,
    jetconfigs...)
    jetconfigs = kwargs_dict(jetconfigs)
    validate_configs(OPT_ANALYZER_VALID_CONFIGURATIONS, jetconfigs)
    state = AnalyzerState(world)
    cache_key = compute_hash(state.inf_params, state.opt_params, OptAnalyzer,
                             skip_noncompileable_calls, __cache_hash__)
    cache_key = @invoke hash(function_filter::Any, cache_key::UInt) # HACK avoid dynamic dispatch
    analysis_token = @lock OPT_ANALYZER_CACHE_LOCK get!(AnalysisToken, OPT_ANALYZER_CACHE, cache_key)
    return OptAnalyzer(
        state,
        analysis_token,
        function_filter,
        skip_noncompileable_calls)
end

const OPT_ANALYZER_CONFIGURATIONS = Set{Symbol}((
    :function_filter, :skip_noncompileable_calls, :__cache_hash__))
const OPT_ANALYZER_VALID_CONFIGURATIONS =
    GENERAL_CONFIGURATIONS ∪ OPT_ANALYZER_CONFIGURATIONS

JETInterface.valid_configurations(::OptAnalyzer) = OPT_ANALYZER_VALID_CONFIGURATIONS

"""
    report_opt(f, [types]; jetconfigs...) -> JETCallResult
    report_opt(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult
    report_opt(mi::Core.MethodInstance; jetconfigs...) -> JETCallResult

Analyzes a function call with the given type signature and returns a
[`JETCallResult`](@ref) containing detected optimization failures and
unresolved method dispatches.

The [general configurations](@ref general-configurations) and
[optimization-analysis-specific configurations](@ref optanalysis-config) can be
supplied as keyword arguments.

See [the documentation of the optimization analysis](@ref optanalysis) for details.
"""
function report_opt(args...; jetconfigs...)
    analyzer = OptAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end

"""
    @report_opt [jetconfigs...] f(args...)

Evaluates the function and its arguments, determines their types, and calls
[`report_opt`](@ref) with the resulting function and argument-type signature.

The [general configurations](@ref general-configurations) and
[optimization-analysis-specific configurations](@ref optanalysis-config) can be
supplied as optional leading configuration arguments.
"""
macro report_opt(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_opt, ex0)
end

"""
    @test_opt [jetconfigs...] [broken=false] [skip=false] f(args...)

Runs [`@report_opt jetconfigs... f(args...)`](@ref @report_opt) and records a
test that passes when the call is free from optimization failures and
unresolved method dispatches that `@report_opt` can detect.

As with [`@report_opt`](@ref), the [general configurations](@ref general-configurations) and
[optimization-analysis-specific configurations](@ref optanalysis-config) can be supplied as
optional leading configuration arguments:
```julia-repl
julia> function f(n)
           r = sincos(n)
           # Ignore runtime dispatch reports from the `println` implementation.
           println(r)
           return r
       end;

julia> @test_opt ignored_modules=(Base,) f(10)
Test Passed
  Expression: #= REPL[3]:1 =# JET.@test_opt ignored_modules = (Base,) f(10)
```

Like [`@test_call`](@ref), `@test_opt` integrates with the
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/).
See [`@test_call`](@ref) for details.
"""
macro test_opt(ex0...)
    return call_test_ex(:report_opt, Symbol("@test_opt"), ex0, __module__, __source__)
end

"""
    test_opt(f, [types]; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_opt(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_opt`](@ref) on a function call with the given type signature and
tests that it is free from optimization failures and unresolved method
dispatches that `report_opt` can detect. It behaves like [`@test_opt`](@ref),
but accepts a type signature rather than a call expression.
"""
function test_opt(@nospecialize(args...); jetconfigs...)
    return func_test(report_opt, :test_opt, args...; jetconfigs...)
end
