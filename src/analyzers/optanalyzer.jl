"""
Every [entry point of optimization analysis](@ref optanalysis-entry) can accept
any of the [general configurations](@ref) as well as the following additional configurations
that are specific to the optimization analysis.

---
- `skip_noncompileable_calls::Bool = true`:\\
  Julia's runtime dispatch is "powerful" because it can always compile code with concrete
  runtime arguments so that [a "kernel" function](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions)
  runs very effectively even if it's called from a type-instable call site.
  This means, we (really) often accept that some parts of our code are not inferred statically,
  and rather we want to just rely on information that is only available at runtime.
  To model this programming style, the optimization analyzer by default does NOT report any
  optimization failures or runtime dispatches detected within non-concrete calls
  (more correctly, "non-compileable" calls are ignored: see also the note below).
  We can turn off this `skip_noncompileable_calls` configuration to get type-instabilities
  within those calls.
  ```julia-repl
  # the following examples are adapted from https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions
  julia> function fill_twos!(a)
             for i = eachindex(a)
                 a[i] = 2
             end
         end;

  julia> function strange_twos(n)
             a = Vector{rand(Bool) ? Int64 : Float64}(undef, n)
             fill_twos!(a)
             return a
         end;

  # by default, only type-instabilities within concrete call (i.e. `strange_twos(3)`) are reported
  # and those within non-concrete calls (`fill_twos!(a)`) are not reported
  julia> @report_opt strange_twos(3)
  ═════ 2 possible errors found ═════
  ┌ strange_twos(n::Int64) @ Main ./REPL[23]:2
  │ runtime dispatch detected: %33::Type{Vector{_A}} where _A(undef, n::Int64)::Vector
  └────────────────────
  ┌ strange_twos(n::Int64) @ Main ./REPL[23]:3
  │ runtime dispatch detected: fill_twos!(%34::Vector)::Any
  └────────────────────

  # we can get reports from non-concrete calls with `skip_noncompileable_calls=false`
  julia> @report_opt skip_noncompileable_calls=false strange_twos(3)
  ┌ strange_twos(n::Int64) @ Main ./REPL[23]:3
  │┌ fill_twos!(a::Vector) @ Main ./REPL[22]:3
  ││┌ setindex!(A::Vector, x::Int64, i1::Int64) @ Base ./array.jl:1014
  │││ runtime dispatch detected: convert(%5::Any, x::Int64)::Any
  ││└────────────────────
  │┌ fill_twos!(a::Vector) @ Main ./REPL[22]:3
  ││ runtime dispatch detected: ((a::Vector)[%13::Int64] = 2::Any)
  │└────────────────────
  ┌ strange_twos(n::Int64) @ Main ./REPL[23]:2
  │ runtime dispatch detected: %33::Type{Vector{_A}} where _A(undef, n::Int64)::Vector
  └────────────────────
  ┌ strange_twos(n::Int64) @ Main ./REPL[23]:3
  │ runtime dispatch detected: fill_twos!(%34::Vector)::Any
  └────────────────────
  ```

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
  # ignore `Core.Compiler.widenconst` calls (since it's designed to be runtime-dispatched):
  julia> function_filter(@nospecialize f) = f !== Core.Compiler.widenconst;

  julia> @test_opt function_filter=function_filter f(args...)
  ...
  ```

---
- `skip_unoptimized_throw_blocks::Bool = true`:\\
  By default, Julia's native compilation pipeline intentionally disables inference (and so
  succeeding optimizations too) on "throw blocks", which are code blocks that will eventually
  lead to `throw` calls, in order to ease [the compilation latency problem, a.k.a. "first-time-to-plot"](https://julialang.org/blog/2020/08/invalidations/).
  Accordingly, the optimization analyzer also ignores any performance pitfalls detected
  within those blocks since we _usually_ don't mind if code involved with error handling
  isn't optimized.
  If `skip_unoptimized_throw_blocks` is set to `false`, it doesn't ignore them and will
  report type instabilities detected within "throw blocks".

  See also <https://github.com/JuliaLang/julia/pull/35982>.

  ```julia-repl
  # by default, unoptimized "throw blocks" are not analyzed
  julia> @test_opt sin(10)
  Test Passed
    Expression: #= none:1 =# JET.@test_opt sin(10)

  # we can turn on the analysis on unoptimized "throw blocks" with `skip_unoptimized_throw_blocks=false`
  julia> @test_opt skip_unoptimized_throw_blocks=false sin(10)
  JET-test failed at none:1
    Expression: #= REPL[6]:1 =# JET.@test_call analyzer = JET.OptAnalyzer skip_unoptimized_throw_blocks = false sin(10)
    ═════ 1 possible error found ═════
    ┌ @ math.jl:1221 Base.Math.sin(xf)
    │┌ @ special/trig.jl:39 Base.Math.sin_domain_error(x)
    ││┌ @ special/trig.jl:28 Base.Math.DomainError(x, "sin(x) is only defined for finite x.")
    │││ runtime dispatch detected: Base.Math.DomainError(x::Float64, "sin(x) is only defined for finite x.")::Any
    ││└──────────────────────

  ERROR: There was an error during testing

  # we can also turns off the heuristic itself
  julia> @test_opt unoptimize_throw_blocks=false skip_unoptimized_throw_blocks=false sin(10)
  Test Passed
    Expression: #= REPL[7]:1 =# JET.@test_call analyzer = JET.OptAnalyzer unoptimize_throw_blocks = false skip_unoptimized_throw_blocks = false sin(10)
  ```

---
"""
struct OptAnalyzer{RP<:ReportPass,FF} <: AbstractAnalyzer
    state::AnalyzerState
    analysis_cache::AnalysisCache
    report_pass::RP
    function_filter::FF
    skip_noncompileable_calls::Bool
    skip_unoptimized_throw_blocks::Bool
    __analyze_frame::BitVector # temporary stash to keep per-frame analysis-skip configuration

    function OptAnalyzer(state::AnalyzerState,
                         report_pass::RP,
                         function_filter::FF,
                         skip_noncompileable_calls::Bool,
                         skip_unoptimized_throw_blocks::Bool) where {RP<:ReportPass,FF}
        if ((@static VERSION < v"1.11.0-DEV.1255" && true) && generating_output())
            # XXX Avoid storing analysis results into a cache that persists across the
            #     precompilation, as pkgimage currently doesn't support serializing
            #     externally created `CodeInstance`. Otherwise, `CodeInstance`s created by
            #     JET, containing JET-specific data structures, will leak into the native
            #     code cache, likely causing segfaults or undefined behavior.
            #     (see https://github.com/JuliaLang/julia/issues/48453).
            analysis_cache = AnalysisCache()
        else
            cache_key = compute_hash(state.inf_params, state.opt_params, report_pass,
                                     skip_noncompileable_calls, skip_unoptimized_throw_blocks)
            cache_key = @invoke hash(function_filter::Any, cache_key::UInt) # HACK avoid dynamic dispatch
            analysis_cache = get!(AnalysisCache, OPT_ANALYZER_CACHE, cache_key)
        end
        return new{RP,FF}(state,
                          analysis_cache,
                          report_pass,
                          function_filter,
                          skip_noncompileable_calls,
                          skip_unoptimized_throw_blocks,
                          #=__analyze_frame=# BitVector())
    end
end

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::OptAnalyzer) = analyzer.state
function JETInterface.AbstractAnalyzer(analyzer::OptAnalyzer, state::AnalyzerState)
    return OptAnalyzer(
        state,
        analyzer.report_pass,
        analyzer.function_filter,
        analyzer.skip_noncompileable_calls,
        analyzer.skip_unoptimized_throw_blocks,)
end
JETInterface.ReportPass(analyzer::OptAnalyzer) = analyzer.report_pass
JETInterface.AnalysisCache(analyzer::OptAnalyzer) = analyzer.analysis_cache
JETInterface.vscode_diagnostics_order(analyzer::OptAnalyzer) = false

const OPT_ANALYZER_CACHE = IdDict{UInt, AnalysisCache}()

struct OptAnalysisPass <: ReportPass end

optanalyzer_function_filter(@nospecialize f) = true

function CC.const_prop_call(analyzer::OptAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
    concrete_eval_result::Union{Nothing,CC.ConstCallResults})
    ret = @invoke CC.const_prop_call(analyzer::AbstractAnalyzer,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
        concrete_eval_result::Union{Nothing,CC.ConstCallResults})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        filter_lineages!(analyzer, sv.result, result.edge::MethodInstance)
    end
    return ret
end
function CC.const_prop_call(analyzer::OptAnalyzer,
    mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::CC.IRInterpretationState,
    concrete_eval_result::Union{Nothing,CC.ConstCallResults})
    if concrete_eval_result !== nothing
        # HACK disable the whole `OptAnalyzer` analysis as far as the frame has been concretized
        # (otherwise we may end up with useless reports from recursive calls)
        return concrete_eval_result
    end
    return @invoke CC.const_prop_call(analyzer::AbstractInterpreter,
        mi::MethodInstance, result::MethodCallResult, arginfo::ArgInfo, sv::CC.IRInterpretationState,
        nothing::Nothing)
end

# TODO better to work only `CC.finish!`
function CC.finish(frame::InferenceState, analyzer::OptAnalyzer)
    ret = @invoke CC.finish(frame::InferenceState, analyzer::AbstractAnalyzer)

    analyze = true
    if analyzer.skip_noncompileable_calls
        mi = frame.linfo
        if !(is_compileable_mi(mi) || is_entry(analyzer, mi))
            analyze = false
        end
    end
    @static if VERSION ≥ v"1.11.0-DEV.404"
    if analyze && CC.is_result_constabi_eligible(frame.result)
        analyze = false
        # turn off optimization for this frame in order to achieve a minor perf gain,
        # similar to the effect of setting `may_discard_trees(::OptAnalyzer) = true`
        frame.result.src = nothing
    end
    end

    push!(analyzer.__analyze_frame, analyze)
    if analyze
        # report pass for captured variables
        ReportPass(analyzer)(CapturedVariableReport, analyzer, frame)
    end

    return ret
end

@jetreport struct CapturedVariableReport <: InferenceErrorReport
    name::Union{Nothing,Symbol}
end
function print_report_message(io::IO, (; name)::CapturedVariableReport)
    if isnothing(name)
        print(io, "captured variable detected")
    else
        print(io, "captured variable `", name, "` detected")
    end
end
print_signature(::CapturedVariableReport) = false
function (::OptAnalysisPass)(::Type{CapturedVariableReport}, analyzer::OptAnalyzer, frame::InferenceState)
    local reported = false
    code = frame.src.code
    for pc = 1:length(code)
        typ = (frame.src.ssavaluetypes::Vector{Any})[pc]
        if typ === Core.Box
            stmt = code[pc]
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, SlotNumber)
                    name = frame.src.slotnames[slot_id(lhs)]
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

function CC.finish!(analyzer::OptAnalyzer, frame::InferenceState)
    caller = frame.result

    # get the source before running `finish!` to keep the reference to `OptimizationState`
    analyze = popfirst!(analyzer.__analyze_frame)
    src = caller.src
    if src isa OptimizationState{typeof(analyzer)}
        # allow the following analysis passes to see the optimized `CodeInfo`
        caller.src = CC.ir_to_codeinf!(src)

        if !analyze
            # if this inferred source is not "compileable" but still is going to be inlined,
            # we should add report runtime dispatches within it
            analyze = CC.is_inlineable(src.src)
        end
    end

    if analyze
        ReportPass(analyzer)(OptimizationFailureReport, analyzer, caller)

        if src isa OptimizationState{typeof(analyzer)}
            ReportPass(analyzer)(RuntimeDispatchReport, analyzer, caller, src)
        elseif (@static JET_DEV_MODE ? true : false)
            if src === nothing # the optimization didn't happen
            else # and this pass should never happen
                # NOTE `src` never be `CodeInfo` since `CC.may_discard_trees(::OptAnalyzer) === false`
                Core.eval(@__MODULE__, :(src = $src))
                throw("unexpected state happened, inspect `$(@__MODULE__).src`")
            end
        end
    end

    return @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
end

# report optimization failure due to recursive calls, etc.
@jetreport struct OptimizationFailureReport <: InferenceErrorReport end
function print_report_message(io::IO, ::OptimizationFailureReport)
    print(io, "failed to optimize due to recursion")
end
function (::OptAnalysisPass)(::Type{OptimizationFailureReport}, analyzer::OptAnalyzer, caller::InferenceResult)
    if caller.src === nothing # the optimization didn't happen
        add_new_report!(analyzer, caller, OptimizationFailureReport(caller.linfo))
        return true
    end
    return false
end

@jetreport struct RuntimeDispatchReport <: InferenceErrorReport end
function print_report_message(io::IO, ::RuntimeDispatchReport)
    print(io, "runtime dispatch detected")
end
function (::OptAnalysisPass)(::Type{RuntimeDispatchReport}, analyzer::OptAnalyzer, caller::InferenceResult, opt::OptimizationState)
    (; src, sptypes, slottypes) = opt

    # TODO better to work on `opt.ir::IRCode` (with some updates on `handle_sig!`)
    local reported = false
    for (pc, x) in enumerate(src.code)
        lin = get_lin((opt, pc))
        lin === nothing && continue # dead statement, just ignore it
        if lin.inlined_at ≠ 0
            # this statement has been inlined, so ignore it as any problems within
            # that callee should already have been reported
            continue
        end
        if analyzer.skip_unoptimized_throw_blocks
            CC.is_stmt_throw_block(src.ssaflags[pc]) && continue
        end
        if isexpr(x, :call)
            ft = argextype(first(x.args), src, sptypes, slottypes)
            f = singleton_type(ft)
            if f !== nothing
                f isa Builtin && continue # ignore `:call`s of language intrinsics
                analyzer.function_filter(f) || continue # ignore user-specified functions
            end
            add_new_report!(analyzer, caller, RuntimeDispatchReport((opt, pc)))
            reported |= true
        end
    end
    return reported
end

# entries
# =======

# the entry constructor
function OptAnalyzer(world::UInt = Base.get_world_counter();
    report_pass::ReportPass = OptAnalysisPass(),
    function_filter = optanalyzer_function_filter,
    skip_noncompileable_calls::Bool = true,
    skip_unoptimized_throw_blocks::Bool = true,
    jetconfigs...)
    state = AnalyzerState(world; jetconfigs...)
    return OptAnalyzer(
        state,
        report_pass,
        function_filter,
        skip_noncompileable_calls,
        skip_unoptimized_throw_blocks)
end

const OPT_ANALYZER_CONFIGURATIONS = Set{Symbol}((
    :report_pass, :function_filter, :skip_noncompileable_calls, :skip_unoptimized_throw_blocks))

let valid_keys = GENERAL_CONFIGURATIONS ∪ OPT_ANALYZER_CONFIGURATIONS
    @eval JETInterface.valid_configurations(::OptAnalyzer) = $valid_keys
end

"""
    report_opt(f, [types]; jetconfigs...) -> JETCallResult
    report_opt(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult
    report_opt(mi::Core.MethodInstance; jetconfigs...) -> JETCallResult

Analyzes a function call with the given type signature to detect optimization failures and
unresolved method dispatches.

The [general configurations](@ref) and [the optimization analysis specific configurations](@ref optanalysis-config)
can be specified as a keyword argument.

See [the documentation of the optimization analysis](@ref optanalysis) for more details.
"""
function report_opt(args...; jetconfigs...)
    analyzer = OptAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end

"""
    @report_opt [jetconfigs...] f(args...)

Evaluates the arguments to a function call, determines their types, and then calls
[`report_opt`](@ref) on the resulting expression.

The [general configurations](@ref) and [the optimization analysis specific configurations](@ref optanalysis-config)
can be specified as an optional argument.
"""
macro report_opt(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_opt, ex0)
end

"""
    @test_opt [jetconfigs...] [broken=false] [skip=false] f(args...)

Runs [`@report_opt jetconfigs... f(args...)`](@ref @report_opt) and tests that the function
call `f(args...)` is free from optimization failures and unresolved method dispatches that
`@report_opt` can detect.

As with [`@report_opt`](@ref), the [general configurations](@ref) and
[optimization analysis specific configurations](@ref optanalysis-config)
can be specified as an optional argument:
```julia-repl
julia> function f(n)
            r = sincos(n)
            # `println` is full of runtime dispatches,
            # but we can ignore the corresponding reports from `Base`
            # with the `target_modules` configuration
            println(r)
            return r
       end;

julia> @test_opt target_modules=(@__MODULE__,) f(10)
Test Passed
  Expression: #= REPL[3]:1 =# JET.@test_call analyzer = JET.OptAnalyzer target_modules = (#= REPL[3]:1 =# @__MODULE__(),) f(10)
```

Like [`@test_call`](@ref), `@test_opt` is fully integrated with the
[`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/).
See [`@test_call`](@ref) for the details.
"""
macro test_opt(ex0...)
    return call_test_ex(:report_opt, Symbol("@test_opt"), ex0, __module__, __source__)
end

"""
    test_opt(f, [types]; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_opt(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_opt`](@ref) on a function call with the given type signature and tests that
it is free from optimization failures and unresolved method dispatches that `report_opt` can detect.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_opt`](@ref).
"""
function test_opt(@nospecialize(args...); jetconfigs...)
    return func_test(report_opt, :test_opt, args...; jetconfigs...)
end
