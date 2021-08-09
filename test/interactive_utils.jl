using JET, InteractiveUtils

const CC = JET.CC

import .CC:
    widenconst, ⊑, Bottom

import JET:
    AbstractAnalyzer,
    JETAnalyzer,
    AbstractGlobal,
    get_result,
    ToplevelConfig,
    virtual_process,
    virtualize_module_context,
    gen_virtual_module,
    ToplevelErrorReport,
    InferenceErrorReport,
    print_reports,
    get_reports,
    get_cache

function subtypes_recursive!(t, ts)
    push!(ts, t)
    if isabstracttype(t)
        for ct in subtypes(t)
            subtypes_recursive!(ct, ts)
        end
    end
    return ts
end

let
    ts = Type[]
    subtypes_recursive!(ToplevelErrorReport, ts)
    subtypes_recursive!(InferenceErrorReport, ts)
    for t in ts
        canonicalname = Symbol(parentmodule(t), '.', nameof(t))
        canonicalpath = Symbol.(split(string(canonicalname), '.'))

        modpath = Expr(:., canonicalpath[1:end-1]...)
        symname = Expr(:., last(canonicalpath))
        ex = Expr(:import, Expr(:(:), modpath, symname))
        Core.eval(@__MODULE__, ex)
    end
end

import Base.Meta:
    isexpr

"""
    @fixturedef

Creates a virtual module and defines fixtures from toplevel expression `ex`
"""
macro fixturedef(ex)
    @assert isexpr(ex, :block)
    return quote let
        vmod = $gen_virtual_module()
        for x in $(ex.args)
            Core.eval(vmod, x)
        end
        vmod # return virtual module
    end end
end

"""
    @analyze_toplevel [jetconfigs...] ex -> JET.VirtualProcessResult

Enters JET analysis from toplevel expression `ex`, and returns the analysis result.
"""
macro analyze_toplevel(xs...)
    jetconfigs = filter(iskwarg, xs)
    xs′ = filter(!iskwarg, xs)
    @assert length(xs′) == 1
    ex = first(xs′)
    return _analyze_toplevel(ex, __source__, jetconfigs)
end

"""
    @analyze_toplevel2 [jetconfigs...] ex -> (Module, JET.VirtualProcessResult)

Works similarly to `@analyze_toplevel`, but also creates a virtual module beforehand and
returns that as well after the whole analysis for the later inspection.
"""
macro analyze_toplevel2(xs...)
    jetconfigs = filter(iskwarg, xs)
    xs′ = filter(!iskwarg, xs)
    @assert length(xs′) == 1
    ex = first(xs′)

    vmod = gensym(:vmod)
    jetconfigs = (:(context = $vmod), jetconfigs...,)
    jetconfigs = (:(virtualize = false), jetconfigs...,)
    ex2 = _analyze_toplevel(ex, __source__, jetconfigs)
    return :(let
        $(esc(vmod)) = $gen_virtual_module()
        ret2 = $ex2
        $(esc(vmod)), ret2
    end)
end

iskwarg(@nospecialize(x)) = isexpr(x, :(=))

function _analyze_toplevel(ex, lnn, jetconfigs)
    toplevelex = (isexpr(ex, :block) ?
                  Expr(:toplevel, lnn, ex.args...) : # flatten here
                  Expr(:toplevel, lnn, ex)
                  ) |> QuoteNode
    return :(let
        analyzer = $(GlobalRef(JET, :JETAnalyzer))(; $(map(esc, jetconfigs)...))
        config = ToplevelConfig(; $(map(esc, jetconfigs)...))
        $virtual_process($toplevelex,
                         $(string(lnn.file)),
                         analyzer,
                         config,
                         )
    end)
end

# `report_file` with silent top-level logger
report_file2(args...; kwargs...) =
    report_file(args...; toplevel_logger = nothing, kwargs...)

is_concrete(mod, sym) = isdefined(mod, sym) && !isa(getfield(mod, sym), AbstractGlobal)
is_abstract(mod, sym) = isdefined(mod, sym) && isa(getfield(mod, sym), AbstractGlobal)
isa_abstract(x, @nospecialize(typ)) = isa(x, AbstractGlobal) && x.t ⊑ typ

# JET will try to concretize global variable when its type is a constant at analysis time,
# but the starategy is a bit complicated right now and may change in the future
# these utilities allow robust testing to check if a object is successfully analyzed by JET whichever it's concretized or abstracted
is_analyzed(mod, sym) = isdefined(mod, sym) # essentially, `is_concrete(mod, sym) || is_abstract(mod, sym)`
isa_analyzed(x, @nospecialize(typ)) = isa_abstract(x, typ) || isa(x, typ)

# fresh execution/benchmark tools
include(normpath(@__DIR__, "..", "benchmark", "JETBenchmarkUtils.jl"))
using .JETBenchmarkUtils
