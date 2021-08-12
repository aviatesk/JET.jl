# # "Unstable API" Analysis
#
# ```@meta
# CurrentModule = JET
# ```

# ## Motivation
#
# Julia doesn't have any facilities to truly hide module internals.
# This means, we can always access to whatever defined within a module and use it freely,
# but some of them may be considered as the module's "internal"s and subject to changes.
# When possible, we want to avoid their usages for better maintainability in the future.
# But the problem is, how can we automatically find them already used in an existing code ?
#
# This analysis is motivated by [this discussion](https://github.com/JuliaLang/julia/pull/40745#issuecomment-850876150).

# ## Implementation

# Let's define "unstable API" s such that, they're
# - undefined binding, or
# - not `export`ed nor documented, if defined
# and now we can implement such analyzer that detects code that matches the definition
# above using JET.jl's pluggable-analysis framework.
#
# The implementation below is _almost sound_, under the assumption that the bindings are
# resolved statically.
# One thing to note is that, the analysis implements an heuristic to avoid false positives
# from "language intrinsics", for example, `Base.indexed_iterate` and `Base.Broadcast.broadcasted`.
# They're _usually_ introduced into your code implicitly by Julia's iteration protocols and such,
# and we're not responsible for their details (thus not interested in their usages).
# But the problem is that the analyzer below doesn't distinguish those introduced by the
# language and those written by ourselves, and in the latter case we're certainly uses
# "unstable API" under the definition above.

using JET.JETInterfaces  # to load APIs of the pluggable analysis framework
const CC = Core.Compiler # to inject a customized report pass

# First off, we define `UnstableAPIAnalyzer`, which is a new [`AbstractAnalyzer`](@ref) and will
# implement the customized report pass

struct UnstableAPIAnalyzer{T} <: AbstractAnalyzer
    state::AnalyzerState
    is_target_module::T
end
function UnstableAPIAnalyzer(;
    is_target_module = ==(@__MODULE__),
    jetconfigs...)
    return UnstableAPIAnalyzer(AnalyzerState(; jetconfigs...), is_target_module)
end
JETInterfaces.AnalyzerState(analyzer::UnstableAPIAnalyzer) = analyzer.state
JETInterfaces.AbstractAnalyzer(analyzer::UnstableAPIAnalyzer, state::AnalyzerState) =
    UnstableAPIAnalyzer(state, analyzer.is_target_module)
JETInterfaces.ReportPass(analyzer::UnstableAPIAnalyzer) = UnstableAPIAnalysisPass()

# Next, we overload some of `Core.Compiler`'s [abstract interpretation](@ref abstractinterpret-analysis) methods,
# and inject a customized analysis pass (here we gonna name it `UnstableAPIAnalysisPass`).
# In this analysis, we are interested in whether a binding that appears in a target code is
# an "unstable API" or not, and we can simply check if each abstract element appeared during
# abstract interpretation meets our criteria of "unstable API".
# For that purpose, it's suffice to overload `Core.Compiler.abstract_eval_special_value`
# and `Core.Compiler.builtin_tfunction`.
# To inject a report pass, we use [`ReportPass(::AbstractAnalyzer)`](@ref) interface.

struct UnstableAPIAnalysisPass <: ReportPass end

function CC.abstract_eval_special_value(analyzer::UnstableAPIAnalyzer, @nospecialize(e), vtypes::CC.VarTable, sv::CC.InferenceState)
    if analyzer.is_target_module(sv.mod) # we care only about what we wrote
        ReportPass(analyzer)(UnstableAPI, analyzer, sv, e)
    end

    ## recurse into JET's default abstract interpretation routine
    return Base.@invoke CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, e, vtypes::CC.VarTable, sv::CC.InferenceState)
end

function CC.builtin_tfunction(analyzer::UnstableAPIAnalyzer, @nospecialize(f), argtypes::Vector{Any}, sv::CC.InferenceState)
    if f === getfield
        if length(argtypes) ≥ 2
            a1, a2 = argtypes[1:2]
            if isa(a1, Core.Const) && (v1 = a1.val; isa(v1, Module))
                if isa(a2, Core.Const) && (v2 = a2.val; isa(v2, Symbol))
                    if analyzer.is_target_module(sv.mod) || # we care only about what we wrote, but with relaxed filter
                       (parent = sv.parent; isa(parent, CC.InferenceState) && analyzer.is_target_module(parent.mod))
                        ReportPass(analyzer)(UnstableAPI, analyzer, sv, GlobalRef(v1, v2))
                    end
                end
            end
        end
    end

    ## recurse into JET's default abstract interpretation routine
    return Base.@invoke CC.builtin_tfunction(analyzer::AbstractAnalyzer, f, argtypes::Vector{Any}, sv::CC.InferenceState)
end

# Additionally, we can cut off the performance cost involved with Julia's native compiler's optimizations passes:
CC.may_optimize(analyzer::UnstableAPIAnalyzer) = return false

# Now we implement the body of our analysis.
# We define "unstable API"s such that they're:
# 1. undefined binding, or
# 2. not `export`ed nor documented, if defined
# and we're not interested in any other program properties other than whether our code contains "unstable API"s or not.

# So in our report pass, we would like to ignore all the reports implemented by JET.jl by default
(::UnstableAPIAnalysisPass)(T::Type{<:InferenceErrorReport}, analyzer, state, @nospecialize(spec_args...)) = return

# but except the report of undefined global references (i.e. `GlobalUndefVarErrorReport`).
# This overload allow us to find code that falls into the category 1.
function (::UnstableAPIAnalysisPass)(T::Type{GlobalUndefVarErrorReport}, analyzer, state, @nospecialize(spec_args...))
    BasicPass()(T, analyzer, state, spec_args...) # bypass to JET's default report pass
end

# And now we will define new [`InferenceErrorReport`](@ref) report type `UnstableAPI`,
# which represents the category 2, and implement a report pass to detect it.

@reportdef struct UnstableAPI <: InferenceErrorReport
    g::GlobalRef
end
function JETInterfaces.get_msg(::Type{UnstableAPI}, analyzer::UnstableAPIAnalyzer, sv, g::GlobalRef)
    (; mod, name) = Base.resolve(g) # resolve to original name
    return "$mod.$name is unstable !"
end

function (::UnstableAPIAnalysisPass)(::Type{UnstableAPI}, analyzer::UnstableAPIAnalyzer, sv, @nospecialize(e))
    if isa(e, GlobalRef)
        isdefined(e.mod, e.name) || return false # this global reference falls into the category 1, should be caught by `GlobalUndefVarErrorReport` instead

        (; mod, name) = Base.resolve(e) # this reference will be safely resolved
        analyzer.is_target_module(mod) && return # we don't care about what we defined ourselves

        if isunstable(mod, name)
            add_new_report!(UnstableAPI(analyzer, sv, e), analyzer)
        end
    end
end

# In the report pass above, `isunstable` will take the heavy lifting to find "unstable API"s.
# Here we will implement `isunstable` according to the definition above but with some heuristics
# to exclude language intrinsics, which can automatically be included into our code and aren't
# usually of our interest.

function isunstable(mod, name)
    ## exclude language intrinsics
    mod === Core && return false
    x = getfield(mod, name)
    x isa Core.Builtin && return false
    (x === Base.indexed_iterate || x === Base.SizeUnknown) && return false # iteration protocol
    (x === Base.Iterators.Filter || x === Base.Iterators.Flatten) && return false # iterator protocol
    x === Base.Broadcast.broadcasted && return false # broadcase protocol
    x === Base.kwerr && return false # ignore keyword lowering

    return !isexported(mod, name) && !hasdoc(mod, name)
end

function isexported(mod, name)
    (; mod, name) = Base.resolve(GlobalRef(mod, name))
    return Base.isexported(mod, name)
end

## adapted from https://github.com/JunoLab/CodeTools.jl/blob/56e7f0b514a7476864c27523bcf9d4bc04699ce1/src/summaries.jl#L24-L34

using Base.Docs
function hasdoc(mod, name)
    binding = Docs.Binding(mod, name)
    for m in Docs.modules
        meta = Docs.meta(m)
        haskey(meta, binding) && return true
        (; mod, var) = binding
        isdefined(mod, var) && haskey(meta, getfield(mod, var)) && return true
    end
    return false
end

# ## Usages

# Now we find "unstable API"s in your code using [JET's analysis entry points](@ref usages)
# with passing `UnstableAPIAnalyzer` as the `analyzer` configuration.

using JET # to use analysis entry points

# ### Simple cases

# Let's first use the [interactive analysis entries](@ref interactive-entries) and
# try simple test cases.

# `UnstableAPIAnalyzer` can find an "unstable" function:
function some_reflection_code(@nospecialize(f))
    return any(Base.hasgenerator, methods(f)) # Base.hasgenerator is unstable
end
@report_call analyzer=UnstableAPIAnalyzer some_reflection_code(sin);

# `UnstableAPIAnalyzer` can find an "unstable" global variable:
module foo; bar = 1 end
report_call((Any,); analyzer=UnstableAPIAnalyzer) do a
    foo.bar + a # foo.bar is unstable
end;

# `UnstableAPIAnalyzer` can detect "unstable API"s even if they're imported binding or
# nested reference (, which will be resolve to `getproperty`)
import Base: hasgenerator
report_call((Any,); analyzer=UnstableAPIAnalyzer) do mi
    ## NOTE every function call appearing here is unstable
    ci = hasgenerator(mi) ? Core.Compiler.get_staged(mi) : Base.uncompressed_ast(mi)
end;

# ### Analyze a real-world package

# Finally we can use [JET's top-level analysis entry points](@ref toplevel-entries) to analyze
# a whole script or package.
#
# Here we will run `UnstableAPIAnalyzer` on [IRTools.jl](https://github.com/FluxML/IRTools.jl),
# which uses `Base.isgenerated`, which is renamed to `Base.hasgenerator` in Julia v1.7 and
# invoked the discussion at <https://github.com/JuliaLang/julia/pull/40745#issuecomment-850876150>.
# Especially, it uses `Base.isgenerator` [here](https://github.com/FluxML/IRTools.jl/blob/1f3f43be654a41d0db154fd16b31fdf40f30748c/src/reflection/reflection.jl#L49),
# and you can see the analyzer correctly detects it if you run the following code with IRTools@v0.4.2 installed.

using Pkg #src
if "IRTools" in keys(Pkg.project().dependencies) #src
is_irtools(mod) = occursin("IRTools", string(Symbol(mod))) # module context will be virtualized by `report_package`, thus use string match
report_package("IRTools"; analyzer=UnstableAPIAnalyzer, is_target_module=is_irtools)
else #src
@warn "IRTools isn't installed in the current environment at $(Pkg.project().path)" #src
end #src

# ```
# ═════ 59 possible errors found ═════
# ┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:39 Core.kwfunc(IRTools.Inner.invoke_meta)(Core.apply_type(Core.NamedTuple, (:world,))(Core.tuple(world)), IRTools.Inner.invoke_meta, T)
# │┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:69 IRTools.Inner.#invoke_meta#6(world, _3, T)
# ││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:74 Core.kwfunc(IRTools.Inner.meta)(Core.apply_type(Core.NamedTuple, (:types, :world))(Core.tuple(S, world)), IRTools.Inner.meta, T)
# │││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:38 IRTools.Inner.#meta#1(types, world, _3, T)
# ││││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:43 Base._methods_by_ftype
# │││││ Base._methods_by_ftype is unstable !: Base._methods_by_ftype
# ││││└─────────────────────────────────────────────────────────────────────────────────
# ││││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:49 Base.isgenerated
# │││││ variable Base.isgenerated is not defined: Base.isgenerated
# ││││└─────────────────────────────────────────────────────────────────────────────────
# ││││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:49 Base.uncompressed_ast
# │││││ Base.uncompressed_ast is unstable !: Base.uncompressed_ast
# ││││└─────────────────────────────────────────────────────────────────────────────────
# ││││┌ @ /Users/aviatesk/.julia/packages/IRTools/aSVI5/src/reflection/reflection.jl:54
# ... # many other "unstable API"s detected
# ```
