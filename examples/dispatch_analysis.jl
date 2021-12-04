# # Dispatch Analysis
#
# ```@meta
# CurrentModule = JET
# ```

# ## Motivation
#
# When Julia compiles your code and type inference on it was not so successful, the compiler
# is likely to be unable to determine which method should be called at each generic function
# callsite, and then it will be looked up at runtime.
# That is called "runtime dispatch", which is known as a common source of performance problem
# – the compiler can't do various kinds of optimizations including inlining when it can't
# determine a single matching method, and method lookup itself can also be a bottleneck if
# the call happens many times.
#
# In order to avoid this problem, we usually use `code_typed`, inspect its output, and check
# if there is anywhere type is not well inferred (i.e. where is "type-instable") and
# optimization was not successful.
# But the problem is that `code_typed` can only present the "final" output of inference or
# optimization, and we can't inspect an entire call graph and _may_ not be able to find where
# a problem happened and how the "type instability" has been propagated.
#
# There is a nice package called [Cthulhu.jl](https://github.com/JuliaDebug/Cthulhu.jl),
# which allows us to inspect the output of `code_typed` by _descending_ into a call tree,
# recursively and interactively.
# The workflow with Cthulhu is much more powerful, but still, it's tedious.
#
# So, why not automate it ?
# We can use JET's pluggable analysis framework and create such an analyzer that automatically
# analyzes your code and alarms you when it detects anywhere Julia can't determine matching
# method statically and thus runtime dispatch will happen at runtime.

# ## Implementation
#
# In this analysis, the analyzer will be designed to detect:
# 1. where Julia compiler gives up optimization
# 2. where a runtime dispatch will happen
#
# The case 1. will happen when there are (mutually) recursive calls and Julia compiler
# decided not to do inference in order to make sure the inference's termination.
# In such a case, optimization won't happen and method dispatches aren't resolved statically,
# so we will just report it (as `OptimizationFailureReport`). In order to detect the case 2.,
# we will inspect the optimized IR and look for `:call` expressions. `:call` expressions are
# such calls that were not resolved statically and will be dispatched at runtime (as opposed
# to [`:invoke`](https://docs.julialang.org/en/v1/devdocs/ast/#Expr-types) expressions, that
# represent staticall resolved generic function calls).
#
# We will define `DispatchAnalyzer <: AbstractAnalyzer`, and overload some of `Core.Compiler` methods with it:
# - `Core.Compiler.finish(frame::CC.InferenceState, analyzer::DispatchAnalyzer)` to check if optimization will happen or not (the case 1.)
# - `Core.Compiler.finish!(analyzer::DispatchAnalyzer, caller::CC.InferenceResult)` to inspect an optimized IR (the case 2.)

using JET.JETInterface
const CC = Core.Compiler
import JET:
    JET,
    @invoke,
    get_source,
    isexpr

struct DispatchAnalyzer{T} <: AbstractAnalyzer
    state::AnalyzerState
    opts::BitVector
    frame_filter::T
    __cache_key::UInt
end
function DispatchAnalyzer(;
    ## a predicate, which takes `CC.InfernceState` and returns whether we want to analyze the call or not
    frame_filter = x::CC.InferenceState->true,
    jetconfigs...)
    state = AnalyzerState(; jetconfigs...)
    ## we want to run different analysis with a different filter, so include its hash into the cache key
    cache_key = state.param_key
    cache_key = hash(frame_filter, cache_key)
    return DispatchAnalyzer(state, BitVector(), frame_filter, cache_key)
end

## maybe filtering by module would be most useful
function module_filter(m)
    return function (frame::CC.InferenceState)
        frame.mod === m
    end
end

## AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::DispatchAnalyzer)                          = analyzer.state
JETInterface.AbstractAnalyzer(analyzer::DispatchAnalyzer, state::AnalyzerState) = DispatchAnalyzer(state, analyzer.opts, analyzer.frame_filter, analyzer.__cache_key)
JETInterface.ReportPass(analyzer::DispatchAnalyzer)                             = DispatchAnalysisPass()
JETInterface.get_cache_key(analyzer::DispatchAnalyzer)                          = analyzer.__cache_key

struct DispatchAnalysisPass <: ReportPass end

## ignore all reports defined by JET, since we'll just define our own reports
(::DispatchAnalysisPass)(T::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return

function CC._typeinf(analyzer::DispatchAnalyzer, frame::CC.InferenceState)
    @assert isempty(analyzer.opts)
    ret = @invoke CC._typeinf(analyzer::AbstractAnalyzer, frame::CC.InferenceState)
    @assert isempty(analyzer.opts)
    return ret
end

function CC.finish(frame::CC.InferenceState, analyzer::DispatchAnalyzer)
    ret = @invoke CC.finish(frame::CC.InferenceState, analyzer::AbstractAnalyzer)

    if !analyzer.frame_filter(frame)
        push!(analyzer.opts, false)
    else
        if isa(get_source(frame.result), CC.OptimizationState)
            push!(analyzer.opts, true)
        else # means, compiler decides not to do optimization
            ReportPass(analyzer)(OptimizationFailureReport, analyzer, frame.result)
            push!(analyzer.opts, false)
        end
    end

    return ret
end

@reportdef struct OptimizationFailureReport <: InferenceErrorReport end
JETInterface.get_msg(::Type{OptimizationFailureReport}, args...) =
    return "failed to optimize" #: signature of this MethodInstance

function (::DispatchAnalysisPass)(::Type{OptimizationFailureReport}, analyzer::DispatchAnalyzer, result::CC.InferenceResult)
    add_new_report!(result, OptimizationFailureReport(result.linfo))
end

function CC.finish!(analyzer::DispatchAnalyzer, frame::CC.InferenceState)
    caller = frame.result

    ## get the source before running `finish!` to keep the reference to `OptimizationState`
    src = get_source(caller)

    ## run `finish!(::AbstractAnalyzer, ::CC.InferenceState)` first to convert the optimized `IRCode` into optimized `CodeInfo`
    ret = @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::CC.InferenceState)

    if popfirst!(analyzer.opts) # optimization happened
        if isa(src, Core.Const) # the optimization was very successful, nothing to report
        elseif isa(src, CC.OptimizationState) # the compiler cached the optimized IR, just analyze it
            ReportPass(analyzer)(RuntimeDispatchReport, analyzer, caller, src)
        else
            ## we should already report `OptimizationFailureReport` for this case,
            ## and thus this pass should never happen
            throw("got $src, unexpected source found")
        end
    end

    return ret
end

@reportdef struct RuntimeDispatchReport <: InferenceErrorReport end
JETInterface.get_msg(::Type{RuntimeDispatchReport}, _) =
    return "runtime dispatch detected" #: call signature

function (::DispatchAnalysisPass)(::Type{RuntimeDispatchReport}, analyzer::DispatchAnalyzer, caller::CC.InferenceResult, opt::CC.OptimizationState)
    (; sptypes, slottypes) = opt
    for (pc, x) in enumerate(opt.src.code)
        if isexpr(x, :call)
            ft = CC.widenconst(CC.argextype(first(x.args), opt.src, sptypes, slottypes))
            ft <: Core.Builtin && continue # ignore `:call`s of language intrinsics
            add_new_report!(caller, RuntimeDispatchReport((opt, pc)))
        end
    end
end

# ## Usages
#
# So we defined our analyzer.
# Let's setup utility analysis entries first:

using JET, InteractiveUtils # to use analysis entry points

function report_dispatch(@nospecialize(f), @nospecialize(types = Tuple{});
                         analyzer = DispatchAnalyzer,
                         jetconfigs...)
    @assert analyzer === DispatchAnalyzer "analyzer is fixed to $DispatchAnalyzer"
    report_call(f, types; analyzer, jetconfigs...)
end
macro report_dispatch(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_dispatch, ex0)
end

# Now we can just call `@report_dispatch f(args...)` and check if there are any problematic
# part within the entire call tree of `f(args...)`.

# ### Simple cases
#
# First, let's play with simple and factitious examples and check if `DispatchAnalyzer`
# works as expected.

f(a) = a
f(a::Number) = a

# `f(::Int)` a concrete call and just type stable and anything shouldn't be reported:
@report_dispatch f(10) # should be ok

# But if the argument type isn't well typed, compiler can't determine which method to call,
# and it will lead to runtime dispatch:
report_dispatch((Any,)) do a
    f(a) # runtime dispatch !
end

# Note that even if a call is not "well-typed", i.e. it's not a concrete call, runtime
# dispatch won't happen as far as a single method can be resovled statically:
report_dispatch((Integer,)) do a
    f(a) # this isn't so good, but ok
end

# Ok, working nicely so far. Let's move on to a bit more complicated examples.
# If we annotate `@noinline` to a function, then its call won't be inlined and will be
# dispatched runtime. We will confirm this:

@inline   g1(a) = return a # WARNING use `@inline` just for demonstrative purpose, usually we really don't need to use it for a very trivial function like this
@noinline g2(a) = return a
report_dispatch((Any,)) do a
    r1 = g1(a) # this call should be statically resolved and inlined
    r2 = g2(a) # this call should be statically resolved but not inlined, and will be dispatched
    return (r1, r2)
end

# We can assert this report by looking at the output of `code_typed`, where `g1(a)` has been
# just disappeared by inlining and optimizations, but `g2(a)` still remains as a `:call` expression:
code_typed((Any,)) do a
    r1 = g1(a) # this call should be statically resolved and inlined
    r2 = g2(a) # this call should be statically resolved but not inlined, and will be dispatched
    return (r1, r2)
end |> first

# ### Real-world targets
#
# Let's run `DispatchAnalyzer` on real-world code and check how it works.
# Here we will test with Julia's `Base` module.

# Numerical computations are usually written to be very type-stable, and so we expects e.g.
# `sin(10)` to be dispatch-free and run fast. Let's check it.
@report_dispatch sin(10)

# Oh no, so runtime dispatch happens there even in `Base`. Well, actually, this specific dispatch
# is expected. Especially, <https://github.com/JuliaLang/julia/pull/35982> implements an
# heuristic to intentionally disable inference (and so succeeding optimizations too) in
# order to ease [the latency problem, a.k.a. "first-time-to-plot"](https://julialang.org/blog/2020/08/invalidations/).
# The report trace certainly suggests a dispatch was detected where `DomainError` can be thrown.
# We can turn off the heuristic by turning off [the `unoptimize_throw_blocks::Bool` configuration](@ref abstractinterpret-config),
# and this time any runtime dispatch won't be reported:
@report_dispatch unoptimize_throw_blocks=false sin(10)

# We can also confirm that the same thing would happen for `rand(1:1000)`:
@report_dispatch rand(1:1000) # an runtiem dispatch will be detected within a `throw` block
#
@report_dispatch unoptimize_throw_blocks=false rand(1:1000) # nothing should be reported

# Finally, let's see an example of very "type-instable" code maintained within `Base`.
# Typically, anything involved with I/O is written in a very dynamic way for good reasons,
# and certainly, e.g. `println(QuoteNode(nothing))` will yield bunch of runtime dispatches:
# ```julia
# @report_dispatch println(QuoteNode(nothing))
# ═════ 585 possible errors found ═════
# ┌ @ coreio.jl:4 Base.println(Core.tuple(Core.typeassert(Base.stdout, Base.IO)), xs...)
# │┌ @ strings/io.jl:73 Base.print(Core.tuple(io), xs, Core.tuple("\n")...)
# ││┌ @ strings/io.jl:43 Base.lock(io)
# │││┌ @ show.jl:334 Base.lock(Base.getproperty(io, :io))
# # so many reports follow ...
# ```

# But what if your code contains a single `println` call, which you're absolutely okay with
# the type instabilities involved with it (e.g. it's only called once or only in debug mode,
# or such), but still you want to assert that any other part of code is type-stable and dispatch-free ?
# `DispatchAnalyzer`'s `frame_filter` option can be useful for this, by allowing us to
# specificy where it should and shouldn't run analysis.
# For example, we can check type-stabilities of anything in the current module like this:

## problem: when ∑1/n exceeds 30 ?
function compute(x)
    r = 1
    s = 0.0
    n = 1
    @time while r < x
        s += 1/n
        if s ≥ r
            println("round $r/$x has been finished") # we're not interested type-instabilities within this call
            r += 1
        end
        n += 1
    end
    return n, s
end

## NOTE:
## `compute(30)` will take more than hours in actual execution, according to https://twitter.com/genkuroki/status/1401332946707963909,
## but `@report_dispatch` will just do abstract interpretation of the call, so will finish instantly
@report_dispatch frame_filter=module_filter(@__MODULE__) compute(30)
