# PerformanceAnalyzer
# ===================

"""
Every [entry point of dispatch analysis](@ref dispatch-analysis-entry-points) can accept
any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/) as well as
the following additional configurations that are specific to dispatch analysis.

---
- `frame_filter = x::$State->true`:\\
  A predicate which takes `InfernceState` or `OptimizationState` and returns `false` to skip analysis on the frame.
  ```julia
  # only checks code within the current module:
  julia> mymodule_filter(x) = x.mod === @__MODULE__;
  julia> @test_nopitfall frame_filter=mymodule_filter f(args...)
  ...
  ```

---
- `function_filter = @nospecialize(ft)->true`:\\
  A predicate which takes a function type and returns `false` to skip analysis on the call.
  ```julia
  # ignores `Core.Compiler.widenconst` calls (since it's designed to be runtime-dispatched):
  julia> myfunction_filter(@nospecialize(ft)) = ft !== typeof(Core.Compiler.widenconst)
  julia> @test_nopitfall function_filter=myfunction_filter f(args...)
  ...
  ```

---
- `skip_nonconcrete_calls::Bool = true`:\\
  Julia's runtime dispatch is "powerful" because it can always compile code with concrete
  runtime arguments so that [a "kernel" function](https://docs.julialang.org/en/v1/manual/performance-tips/#kernel-functions)
  runs very effectively even if it's called from a type-instable call site.
  This means, we (really) often accept that some parts of our code are not inferred statically,
  and rather we want to just rely on information that is only available at runtime.
  To model this programming style, dispatch analyzer does NOT report any optimization failures
  or runtime dispatches detected within non-concrete calls under the default configuration.
  We can turn off this `skip_nonconcrete_calls` configuration to get type-instabilities
  within non-concrete calls.
  ```julia
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
  julia> @report_pitfall strange_twos(3)
  ═════ 2 possible errors found ═════
  ┌ @ REPL[2]:2 %45(Main.undef, n)
  │ runtime dispatch detected: %45::Type{Vector{_A}} where _A(Main.undef, n::Int64)
  └─────────────
  ┌ @ REPL[2]:3 Main.fill_twos!(%46)
  │ runtime dispatch detected: Main.fill_twos!(%46::Vector)
  └─────────────
  Vector (alias for Array{_A, 1} where _A)

  # we can get reports from non-concrete calls with `skip_nonconcrete_calls=false`
  julia> @report_pitfall skip_nonconcrete_calls=false strange_twos(3)
  ═════ 4 possible errors found ═════
  ┌ @ REPL[2]:3 Main.fill_twos!(a)
  │┌ @ REPL[1]:3 Base.setindex!(a, 2, %14)
  ││ runtime dispatch detected: Base.setindex!(a::Vector, 2, %14::Int64)
  │└─────────────
  │┌ @ REPL[1]:3 Base.setindex!(a, 2, i)
  ││┌ @ array.jl:877 Base.convert(_, x)
  │││ runtime dispatch detected: Base.convert(_::Any, x::Int64)
  ││└────────────────
  ┌ @ REPL[2]:2 %45(Main.undef, n)
  │ runtime dispatch detected: %45::Type{Vector{_A}} where _A(Main.undef, n::Int64)
  └─────────────
  ┌ @ REPL[2]:3 Main.fill_twos!(%46)
  │ runtime dispatch detected: Main.fill_twos!(%46::Vector)
  └─────────────
  Vector (alias for Array{_A, 1} where _A)
  ```

---
- `skip_unoptimized_throw_blocks::Bool = true`:\\
  By default, Julia's native compilation pipeline intentionally disables inference (and so
  succeeding optimizations too) on "throw blocks", which are code blocks that will eventually
  lead to `throw` calls, in order to ease [the compilation latency problem, a.k.a. "first-time-to-plot"](https://julialang.org/blog/2020/08/invalidations/).
  Accordingly, the dispatch analyzer also ignores runtime dispatches detected within those blocks
  since we _usually_ don't mind if code involved with error handling isn't optimized.
  If `skip_unoptimized_throw_blocks` is set to `false`, it doesn't ignore them and will
  report type instabilities detected within "throw blocks".

  See also <https://github.com/JuliaLang/julia/pull/35982>.

  ```julia
  # by default, unoptimized "throw blocks" are not analyzed
  julia> @test_nopitfall sin(10)
  Test Passed
    Expression: #= none:1 =# JET.@test_nopitfall sin(10)

  # we can turn on the analysis on unoptimized "throw blocks" with `skip_unoptimized_throw_blocks=false`
  julia> @test_nopitfall skip_unoptimized_throw_blocks=false sin(10)
  Dispatch Test Failed at none:1
    Expression: #= none:1 =# JET.@test_nopitfall skip_unoptimized_throw_blocks = false sin(10)
    ═════ 1 possible error found ═════
    ┌ @ math.jl:1221 Base.Math.sin(xf)
    │┌ @ special/trig.jl:39 Base.Math.sin_domain_error(x)
    ││┌ @ special/trig.jl:28 Base.Math.DomainError(x, "sin(x) is only defined for finite x.")
    │││ runtime dispatch detected: Base.Math.DomainError(x::Float64, "sin(x) is only defined for finite x.")
    ││└──────────────────────

  ERROR: There was an error during testing

  # we can also turns off the heuristic itself
  julia> @test_nopitfall unoptimize_throw_blocks=false skip_unoptimized_throw_blocks=false sin(10)
  Test Passed
    Expression: #= none:1 =# JET.@test_nopitfall unoptimize_throw_blocks = false skip_unoptimized_throw_blocks = false sin(10)
  ```

---
"""
struct PerformanceAnalyzer{S,T} <: AbstractAnalyzer
    state::AnalyzerState
    frame_filter::S
    function_filter::T
    skip_unoptimized_throw_blocks::Bool
    _frame_checks::BitVector
end
function PerformanceAnalyzer(;
    frame_filter = x::State->true,
    function_filter = @nospecialize(ft)->true,
    skip_nonconcrete_calls::Bool = true,
    skip_unoptimized_throw_blocks::Bool = true,
    jetconfigs...)
    state = AnalyzerState(; jetconfigs...)
    if skip_nonconcrete_calls
        new_frame_filter = function (x::State)
            if isdispatchtuple(x.linfo.specTypes)
                return frame_filter(x)
            else
                return false
            end
        end
    else
        new_frame_filter = frame_filter
    end
    return PerformanceAnalyzer(
        state,
        new_frame_filter,
        function_filter,
        skip_unoptimized_throw_blocks,
        #=_frame_checks=# BitVector(),
        )
end

# AbstractAnalyzer API requirements
AnalyzerState(analyzer::PerformanceAnalyzer) = analyzer.state
function AbstractAnalyzer(analyzer::PerformanceAnalyzer, state::AnalyzerState)
    return PerformanceAnalyzer(
        state,
        analyzer.frame_filter,
        analyzer.function_filter,
        analyzer.skip_unoptimized_throw_blocks,
        analyzer._frame_checks,
        )
end
ReportPass(analyzer::PerformanceAnalyzer) = PerformanceAnalysisPass() # TODO parameterize this

# we want to run different analysis with a different filter, so include its hash into the cache key
function get_cache_key(analyzer::PerformanceAnalyzer)
    h = @invoke get_cache_key(analyzer::AbstractAnalyzer)
    h = @invoke hash(analyzer.frame_filter::Any, h::UInt)    # HACK avoid dynamic dispatch
    h = @invoke hash(analyzer.function_filter::Any, h::UInt) # HACK avoid dynamic dispatch
    h = hash(analyzer.skip_unoptimized_throw_blocks, h)
    return h
end

struct PerformanceAnalysisPass <: ReportPass end

function CC.finish(frame::InferenceState, analyzer::PerformanceAnalyzer)
    ret = @invoke CC.finish(frame::InferenceState, analyzer::AbstractAnalyzer)

    check = analyzer.frame_filter(frame)
    push!(analyzer._frame_checks, check)
    if check
        if isa(get_source(frame.result), OptimizationState)
            ReportPass(analyzer)(CapturedVariableReport, analyzer, frame)
        else
            ReportPass(analyzer)(OptimizationFailureReport, analyzer, frame.result)
        end
    end

    return ret
end

@reportdef struct CapturedVariableReport <: InferenceErrorReport
    name::Union{Nothing,Symbol}
end
get_msg(::Type{CapturedVariableReport}, analyzer, s, name::Union{Nothing,Symbol}) =
    isnothing(name) ? "captured variable detected" : "captured variable `$name` detected"
print_error_report(io, report::CapturedVariableReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
function (::PerformanceAnalysisPass)(::Type{CapturedVariableReport}, analyzer::PerformanceAnalyzer, frame::InferenceState)
    for (pc, typ) in enumerate(frame.src.ssavaluetypes)
        if typ === Core.Box
            stmt = frame.src.code[pc]
            if @isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, SlotNumber)
                    name = frame.src.slotnames[slot_id(lhs)]
                else
                    name = nothing
                end
                add_new_report!(frame.result, CapturedVariableReport(analyzer, (frame, pc), name))
            end
        end
    end
end

@reportdef struct OptimizationFailureReport <: InferenceErrorReport end
get_msg(::Type{OptimizationFailureReport}, args...) = "failed to optimize"
function (::PerformanceAnalysisPass)(::Type{OptimizationFailureReport}, analyzer::PerformanceAnalyzer, caller::InferenceResult)
    add_new_report!(caller, OptimizationFailureReport(analyzer, caller.linfo))
end

function CC.finish!(analyzer::PerformanceAnalyzer, frame::InferenceState)
    caller = frame.result

    # get the source before running `finish!` to keep the reference to `OptimizationState`
    src = get_source(caller)

    ret = @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)

    if popfirst!(analyzer._frame_checks)
        if isa(src, Const) # the optimization was very successful, nothing to report
        elseif isa(src, OptimizationState) # compiler cached the optimized IR, just analyze it
            ReportPass(analyzer)(RuntimeDispatchReport, analyzer, caller, src)
        else
            # we should already report `OptimizationFailureReport` for this case,
            # and thus this pass should never happen
            Core.eval(@__MODULE__, :(ret = $ret))
            throw("unexpected state happened, inspect $(@__MODULE__).ret") # this pass should never happen
        end
    end

    return ret
end

@reportdef struct RuntimeDispatchReport <: InferenceErrorReport end
get_msg(::Type{RuntimeDispatchReport}, analyzer, s) = "runtime dispatch detected"
function (::PerformanceAnalysisPass)(::Type{RuntimeDispatchReport}, analyzer::PerformanceAnalyzer, caller::InferenceResult, opt::OptimizationState)
    (; src, sptypes, slottypes) = opt

    throw_blocks =
        analyzer.skip_unoptimized_throw_blocks && opt.inlining.params.unoptimize_throw_blocks ?
        CC.find_throw_blocks(src.code) : nothing

    for (pc, x) in enumerate(src.code)
        if !isnothing(throw_blocks)
            # optimization is intentionally turned off for this block, let's ignore anything here
            CC.in(pc, throw_blocks) && continue
        end
        if @isexpr(x, :call)
            ft = widenconst(argextype(first(x.args), src, sptypes, slottypes))
            ft <: Builtin && continue # ignore `:call`s of language intrinsics
            if analyzer.function_filter(ft)
                add_new_report!(caller, RuntimeDispatchReport(analyzer, (opt, pc)))
            end
        end
    end
end

# entries
# -------

"""
    report_pitfall(f, types = Tuple{}; jetconfigs...) -> JETCallResult
    report_pitfall(tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult

Analyzes the generic function call with the given type signature with `PerformanceAnalyzer`,
which collects optimization failures and runtime dispatches involved within the call stack.
"""
function report_pitfall(@nospecialize(args...);
                        analyzer = PerformanceAnalyzer,
                        jetconfigs...)
    if !(analyzer === PerformanceAnalyzer)
        throw(ArgumentError("`analyzer` is fixed to $PerformanceAnalyzer"))
    end
    return report_call(args...; analyzer, jetconfigs...)
end

"""
    @report_pitfall [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`report_pitfall`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or [dispatch analysis specific configurations](@ref dispatch-analysis-configurations) can be given as the optional arguments like this:
```julia
# reports `rand(::Type{Bool})` with `unoptimize_throw_blocks` configuration turned on
julia> @report_pitfall unoptimize_throw_blocks=true rand(Bool)
```
"""
macro report_pitfall(ex0...)
    return var"@report_call"(__source__, __module__, :(analyzer=$PerformanceAnalyzer), ex0...)
end

"""
    @test_nopitfall [jetconfigs...] [broken=false] [skip=false] f(args...)

Tests the generic function call `f(args...)` is free from runtime dispatch.
Returns a `Pass` result if it is, a `Fail` result if if contains any location where runtime
dispatch or optimization failure happens, or an `Error` result if this macro encounters an
unexpected error. When the test `Fail`s, abstract call stack to each problem location will
also be printed to `stdout`.

```julia
julia> @test_nopitfall sincos(10)
Test Passed
  Expression: #= none:1 =# JET.@test_nopitfall sincos(10)
```

As with [`@report_pitfall`](@ref), any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or [dispatch analysis specific configurations](@ref dispatch-analysis-configurations) can be given as the optional arguments like this:
```julia
julia> function f(n)
            r = sincos(n)
            println(r) # `println` is full of runtime dispatches, but we can ignore the corresponding reports from `Base` by explicit frame filter
            return r
       end;
julia> this_module_filter(x) = x.mod === @__MODULE__;

julia> @test_nopitfall frame_filter=this_module_filter f(10)
Test Passed
  Expression: #= none:1 =# JET.@test_nopitfall frame_filter = this_module_filter f(10)
```

`@test_nopitfall` is fully integrated with [`Test` standard library's unit-testing infrastructure](https://docs.julialang.org/en/v1/stdlib/Test/).
It means, the result of `@test_nopitfall` will be included in the final `@testset` summary,
it supports `skip` and `broken` annotations as `@test` macro does, etc.
```julia
julia> using Test

julia> f(params) = sin(params.value); # type-stable
julia> params = (; value = 10);       # non-constant global variable
julia> g() = sin(params.value);       # very type-instable

julia> @testset "check optimizations" begin
           @test_nopitfall f((; value = 10)) # pass
           @test_nopitfall g()               # fail
           @test_nopitfall broken=true g()   # annotated as broken, thus still "pass"
       end
check optimizations: Dispatch Test Failed at none:3
  Expression: #= none:3 =# JET.@test_nopitfall g()
  ═════ 2 possible errors found ═════
  ┌ @ none:1 Base.getproperty(%1, :value)
  │ runtime dispatch detected: Base.getproperty(%1::Any, :value::Symbol)
  └──────────
  ┌ @ none:1 Main.sin(%2)
  │ runtime dispatch detected: Main.sin(%2::Any)
  └──────────

Test Summary:       | Pass  Fail  Broken  Total
check optimizations |    1     1       1      3
ERROR: Some tests did not pass: 1 passed, 1 failed, 0 errored, 1 broken.
```
"""
macro test_nopitfall(ex0...)
    return var"@test_call"(__source__, __module__, :(analyzer=$PerformanceAnalyzer), ex0...)
end

"""
    test_nopitfall(f, types = Tuple{}; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_nopitfall(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Tests the generic function call with the given type signature is free from runtime dispatch.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_nopitfall`](@ref).
"""
function test_nopitfall(@nospecialize(args...);
                         analyzer = PerformanceAnalyzer,
                         kwargs...)
    if !(analyzer === PerformanceAnalyzer)
        throw(ArgumentError("`analyzer` is fixed to $PerformanceAnalyzer"))
    end
    return test_call(args...; analyzer, kwargs...)
end
