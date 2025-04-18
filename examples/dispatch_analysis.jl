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
# We will define `DispatchAnalyzer <: AbstractAnalyzer`, and overload some of `Base.Compiler` methods with it:
# - `CC.finish(frame::CC.InferenceState, analyzer::DispatchAnalyzer)` to check if optimization will happen or not (the case 1.)
# - `CC.finish!(analyzer::DispatchAnalyzer, caller::CC.InferenceResult)` to inspect an optimized IR (the case 2.)

using JET.JETInterface
using JET: JET, CC

struct DispatchAnalyzer{T} <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    opts::BitVector
    frame_filter::T # a predicate, which takes `CC.InfernceState` and returns whether we want to analyze the call or not
end

## AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::DispatchAnalyzer) = analyzer.state
JETInterface.AbstractAnalyzer(analyzer::DispatchAnalyzer, state::AnalyzerState) = DispatchAnalyzer(state, analyzer.analysis_token, analyzer.opts, analyzer.frame_filter)
JETInterface.ReportPass(analyzer::DispatchAnalyzer) = DispatchAnalysisPass()
JETInterface.AnalysisToken(analyzer::DispatchAnalyzer) = analyzer.analysis_token

struct DispatchAnalysisPass <: ReportPass end

## ignore all reports defined by JET, since we'll just define our own reports
(::DispatchAnalysisPass)(T::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return

function CC.finish!(analyzer::DispatchAnalyzer, frame::CC.InferenceState, validation_world::UInt, time_before::UInt64)
    caller = frame.result

    ## get the source before running `finish!` to keep the reference to `OptimizationState`
    src = caller.src
    if src isa CC.OptimizationState
        ## allow the following analysis passes to see the optimized `CodeInfo`
        caller.src = CC.ir_to_codeinf!(src)
        frame.edges = collect(Any, caller.src.edges)
    end

    if analyzer.frame_filter(frame.linfo)
        if isa(src, Core.Const) # the optimization was very successful, nothing to report
        elseif isnothing(src) # means, compiler decides not to do optimization
            ReportPass(analyzer)(OptimizationFailureReport, analyzer, caller, src)
        elseif isa(src, CC.OptimizationState) # the compiler optimized it, analyze it
            ReportPass(analyzer)(RuntimeDispatchReport, analyzer, caller, src)
        else # and thus this pass should never happen
            ## as we should already report `OptimizationFailureReport` for this case
            throw("got $src, unexpected source found")
        end
    end

    return @invoke CC.finish!(analyzer::AbstractAnalyzer, frame::CC.InferenceState, validation_world::UInt, time_before::UInt64)
end

@jetreport struct OptimizationFailureReport <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::OptimizationFailureReport)
    print(io, "failed to optimize due to recursion")
end
function (::DispatchAnalysisPass)(::Type{OptimizationFailureReport}, analyzer::DispatchAnalyzer, result::CC.InferenceResult)
    add_new_report!(analyzer, result, OptimizationFailureReport(result.linfo))
end

@jetreport struct RuntimeDispatchReport <: InferenceErrorReport end
function JETInterface.print_report_message(io::IO, ::RuntimeDispatchReport)
    print(io, "runtime dispatch detected")
end

function (::DispatchAnalysisPass)(::Type{RuntimeDispatchReport}, analyzer::DispatchAnalyzer, caller::CC.InferenceResult, opt::CC.OptimizationState)
    (; sptypes, slottypes) = opt
    for (pc, x) in enumerate(opt.src.code)
        if Base.Meta.isexpr(x, :call)
            ft = CC.widenconst(CC.argextype(first(x.args), opt.src, sptypes, slottypes))
            ft <: Core.Builtin && continue # ignore `:call`s of the builtin intrinsics
            add_new_report!(analyzer, caller, RuntimeDispatchReport((opt, pc)))
        end
    end
end

# ## Usages
#
# So we defined our analyzer.
# Let's set up utility analysis entries first:

using InteractiveUtils # to use `gen_call_with_extracted_types_and_kwargs`

const global_analysis_token = AnalysisToken()

## the constructor for creating a new configured `DispatchAnalyzer` instance
function DispatchAnalyzer(world::UInt = Base.get_world_counter();
    analysis_token::AnalysisToken = global_analysis_token,
    frame_filter = x::Core.MethodInstance->true,
    jetconfigs...)
    state = AnalyzerState(world; jetconfigs...)
    ## just for the sake of simplicity, create a fresh code cache for each `DispatchAnalyzer` instance (i.e. don't globalize the cache)
    return DispatchAnalyzer(state, analysis_token, BitVector(), frame_filter)
end
function report_dispatch(args...; jetconfigs...)
    @nospecialize args jetconfigs
    analyzer = DispatchAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
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

getsomething(x::Any) = x
getsomething(x::Array) = x[]
getsomething(::Nothing) = throw(ArgumentError("nothing is nothing"))
getsomething(::Missing) = throw(ArgumentError("too philosophical"))

# If callsite is type-stable (i.e. dispatched with concretely-typed arguments),
# any problem shouldn't be reported:
@report_dispatch getsomething(42) # should be ok

# But if the argument isn't well-typed, compiler can't determine which method to call,
# and it will lead to runtime dispatch:
report_dispatch((Any,)) do a
    getsomething(a) # runtime dispatch !
end

# Note that even if a call is not "well-typed" (i.e. it's not a concrete call), runtime
# dispatch won't happen as far as a single method can be resolved statically:
report_dispatch((AbstractString,)) do a
    getsomething(a) # this call isn't very concrete, but ok, Julia can optimize it
end

# Ok, working nicely so far. Let's move on to a bit more complicated examples.
# When working on inherently-untyped code base where we somehow need to deal with
# arbitrarily-typed objects at runtime (as like Julia's high-level compiler),
# the [`@nospecialize`](https://docs.julialang.org/en/v1/base/base/#Base.@nospecialize)
# annotation can be very useful -- it helps us avoids excessive code
# specialization by _suppressing_ runtime dispatches with runtime object types.
# For example, let's assume we have a vector of arbitrary untyped objects used within
# an user-program and need to check if its element is `Type`-like object with the
# following logic:
function isTypelike(x)
    if isa(x, DataType)
        return isa(x, DataType) && x.name === Type.body.name
    elseif isa(x, Union)
        return isTypelike(x.a) && isTypelike(x.b)
    elseif isa(x, UnionAll)
        return isTypelike(x.body)
    else
        return false
    end
end

# But without `@nospecialize`, we gonna see runtime dispatches at the recursive call sites as
# they will be specialized at runtime. In this setup, we can suppress the runtime dipsatches
# and achieve a best performance by applying `@nospecialize` annotation to the argument `x`:
function isTypelike′(@nospecialize x)
    if isa(x, DataType)
        return isa(x, DataType) && x.name === Type.body.name
    elseif isa(x, Union)
        return isTypelike′(x.a) && isTypelike′(x.b)
    elseif isa(x, UnionAll)
        return isTypelike′(x.body)
    else
        return false
    end
end

# We can confirm the effect of `@nospecialize` with `DispatchAnalyzer`:
report_dispatch((Vector{Any},)) do xs
    x  = xs[1]
    r  = isTypelike(x)  # this call will be runtime-dispatched
    r′ = isTypelike′(x) # this call will be statically resolved (not runtime-dispatched)
    return r, r′
end

# We can assert this report by looking at the output of `code_typed`, where `isTypelike(x)`
# remains as `:call` expression (meaning it will be dispatched at runtime) while `isTypelike′(x)`
# has been statically resolved and even inlined:
code_typed((Vector{Any},)) do xs
    x  = xs[1]
    r  = isTypelike(x)  # this call will be runtime-dispatched
    r′ = isTypelike′(x) # this call will be statically resolved (not runtime-dispatched)
    return r, r′
end

# ### Real-world targets
#
# Let's run `DispatchAnalyzer` on real-world code and check how it works.
# Here we will test with Julia's `Base` module.

# Random number generation might be one of the most important feature for numerical computations,
# and so Julia's `rand` function should run fast. Let's see if it is free from runtime-dispatch.
@report_dispatch rand(1:1000)

# Oh no, runtime dispatch happens there even in `Base`. Well, actually, this specific dispatch
# is expected. Especially, <https://github.com/JuliaLang/julia/pull/35982> implements an
# heuristic to intentionally _disable_ inference (and so succeeding optimizations too) in
# order to ease [the latency problem, a.k.a. "first-time-to-plot"](https://julialang.org/blog/2020/08/invalidations/).
# The report trace certainly suggests a dispatch was detected where `ArgumentError` can be thrown.
# We can turn off the heuristic by turning off [the `unoptimize_throw_blocks::Bool` configuration](@ref abstractinterpret-config),
# and this time any runtime dispatch won't be reported:
@report_dispatch unoptimize_throw_blocks=false rand(1:1000) # nothing should be reported

# Finally, let's see an example of very "type-instable" code maintained within `Base`.
# Typically, anything involving I/O is written in a very dynamic way for good reasons,
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

# `DispatchAnalyzer`'s `frame_filter` option can be useful for this, by allowing us to
# specify where it should and shouldn't run analysis.
# For example, we can limit the scope of analysis to the current module like this:
function module_filter(m) # filter by module
    return function (linfo::Core.MethodInstance)
        def = linfo.def
        isa(def, Method) ? def.module === m : def === m
    end
end

## NOTE:
## `compute(30)` will take more than hours in actual execution, according to https://twitter.com/genkuroki/status/1401332946707963909,
## but `@report_dispatch` will just do abstract interpretation of the call, so will finish instantly
@report_dispatch frame_filter=module_filter(@__MODULE__) compute(30)
