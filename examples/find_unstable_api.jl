"""
"Unstable API"s Analysis
========================

Julia doesn't have any facilities to truly hide module internals.
This means, we can always access to whatever defined within a module and use it freely,
but some of them may be considered as the module's "internal"s and subject to changes.
When possible, we want to avoid using those "unstable API" s for better maintainability in the future.
But the problem is, how can we find them already used in an existing code ?

The following code implements such an analysis as a plug-in analysis under JET.jl framework.
Let's define "unstable API" s such that, they're
- not `export`ed
- nor documented

Then we can just check each binding discovered during abstract interpretation meets the criteria.
Technically, we will overload `Core.Compiler.abstract_eval_special_value` and check `GlobalRef`s appeared there.

The implementation below is _almost sound_, under the assumption that the bindings are resolved statically.
One thing to note is that, the analysis implements an heuristic to avoid false positives from "language intrinsics",
for example, `Base.indexed_iterate` and `Base.Broadcast.broadcasted`.
They're _usually_ automatically introduced into your code by Julia's iteration protocols and such,
and in such cases we're not responsible for those details.
But the problem is the analysis doesn't distinguish those introduced by the language and those written by ourselves,
and in the latter case we're certainly uses "unstable API" under the definition above.

This analysis is motivated by [this discussion](https://github.com/JuliaLang/julia/pull/40745#issuecomment-850876150).
"""
module find_unstable_api

using JET

const CC = Core.Compiler

struct UnstableAPIAnalyzer{T} <: JET.AbstractAnalyzer
    state::JET.AnalyzerState
    is_target_module::T
end
function UnstableAPIAnalyzer(;
    is_target_module = ==(@__MODULE__),
    jetconfigs...)
    return UnstableAPIAnalyzer(JET.AnalyzerState(; jetconfigs...), is_target_module)
end
JET.AnalyzerState(analyzer::UnstableAPIAnalyzer) = analyzer.state
JET.AbstractAnalyzer(analyzer::UnstableAPIAnalyzer, state::JET.AnalyzerState) =
    UnstableAPIAnalyzer(state, analyzer.is_target_module)
JET.ReportPass(analyzer::UnstableAPIAnalyzer) = UnstableAPIAnalysisPass()

struct UnstableAPIAnalysisPass <: JET.ReportPass end

function CC.abstract_eval_special_value(analyzer::UnstableAPIAnalyzer, @nospecialize(e), vtypes::CC.VarTable, sv::CC.InferenceState)
    if analyzer.is_target_module(sv.mod) # we care only about what we wrote
        JET.report_pass!(UnstableAPI, analyzer, sv, e)
    end

    return Base.@invoke CC.abstract_eval_special_value(analyzer::JET.AbstractAnalyzer, e, vtypes::CC.VarTable, sv::CC.InferenceState)
end

function CC.builtin_tfunction(analyzer::UnstableAPIAnalyzer, @nospecialize(f), argtypes::Vector{Any}, sv::CC.InferenceState)
    if f === getfield
        if length(argtypes) ≥ 2
            a1, a2 = argtypes[1:2]
            if isa(a1, Core.Const) && (v1 = a1.val; isa(v1, Module))
                if isa(a2, Core.Const) && (v2 = a2.val; isa(v2, Symbol))
                    if analyzer.is_target_module(sv.mod) || # we care only about what we wrote, but with relaxed filter
                       (parent = sv.parent; isa(parent, CC.InferenceState) && analyzer.is_target_module(parent.mod))
                        JET.report_pass!(UnstableAPI, analyzer, sv, GlobalRef(v1, v2))
                    end
                end
            end
        end
    end

    return Base.@invoke CC.builtin_tfunction(analyzer::JET.AbstractAnalyzer, f, argtypes::Vector{Any}, sv::CC.InferenceState)
end

# ignore report passes implemented within JET.jl
(::UnstableAPIAnalysisPass)(T::Type{<:JET.InferenceErrorReport}, analyzer, linfo, @nospecialize(spec_args...)) = return

# except undefined global references
function (::UnstableAPIAnalysisPass)(T::Type{JET.GlobalUndefVarErrorReport}, analyzer, linfo, @nospecialize(spec_args...))
    JET.BasicPass()(T, analyzer, linfo, spec_args...)
end

JET.@reportdef struct UnstableAPI <: JET.InferenceErrorReport
    g::GlobalRef
end
function JET.get_msg(::Type{UnstableAPI}, analyzer::UnstableAPIAnalyzer, sv::CC.InferenceState, g::GlobalRef)
    (; mod, name) = Base.resolve(g) # resolve to original name
    return "$mod.$name is unstable !"
end

function (::UnstableAPIAnalysisPass)(::Type{UnstableAPI}, analyzer::UnstableAPIAnalyzer, sv::CC.InferenceState, @nospecialize(e))
    if isa(e, GlobalRef)
        isdefined(e.mod, e.name) || return false # will be caught by GlobalUndefVarErrorReport

        (; mod, name) = Base.resolve(e) # safely resolve this reference
        analyzer.is_target_module(mod) && return # we don't care about what we defined ourselves

        if isunstable(mod, name)
            JET.report!(UnstableAPI, analyzer, sv, e)
        end
    end
end

# we define "unstable API" such that, it's
# - not `export`ed
# - nor documented
function isunstable(mod, name)
    # exclude language intrinsics
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

# adapted from https://github.com/JunoLab/CodeTools.jl/blob/56e7f0b514a7476864c27523bcf9d4bc04699ce1/src/summaries.jl#L24-L34

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

# test simple case
# ================

# a function
function some_reflection_code(@nospecialize(f))
    return any(Base.hasgenerator, methods(f)) # Base.hasgenerator is unstable
end
@report_call analyzer=UnstableAPIAnalyzer some_reflection_code(sin)

# global variable
module foo
bar = 1
end
report_call((Any,); analyzer=UnstableAPIAnalyzer) do a
    foo.bar + a # foo.bar is unstable
end

# supports imported binding, also nested reference (, which will be resolve to `getproperty`)
import Base: hasgenerator
@test report_call((Any,); analyzer=UnstableAPIAnalyzer) do mi
    # every function call appearing here is unstable
    ci = hasgenerator(mi) ? Core.Compiler.get_staged(mi) : Base.uncompressed_ast(mi)
end

# test target
# ===========

# IRTools uses `Base.isgenerator` and leaded to the discussion at https://github.com/JuliaLang/julia/pull/40745#issuecomment-850876150

is_irtools(mod) = occursin("IRTools", string(Symbol(mod))) # module context will be virtualized, thus use string match
report_package("IRTools"; analyzer=UnstableAPIAnalyzer, is_target_module=is_irtools)

end # module find_unstable_api
