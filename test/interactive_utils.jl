using JET, InteractiveUtils

const CC = JET.CC

import .CC:
    widenconst, ⊑

import JET:
    JETInterpreter,
    AbstractGlobal,
    analyze_file,
    analyze_text,
    get_result,
    ToplevelConfig,
    virtual_process,
    virtualize_module_context,
    gen_virtual_module,
    ToplevelErrorReport,
    InferenceErrorReport,
    ExceptionReport,
    print_reports

for sym in Symbol.(last.(Base.Fix2(split, '.').(string.(vcat(subtypes(JET, ToplevelErrorReport),
                                                             subtypes(JET, InferenceErrorReport),
                                                             subtypes(JET, ExceptionReport),
                                                             )))))
    Core.eval(@__MODULE__, :(import JET: $(sym)))
end

import Base:
    Fix1,
    Fix2

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
    @analyze_toplevel [jetconfigs...] ex

Enters JET analysis from toplevel expression `ex`, and returns the analysis result.
"""
macro analyze_toplevel(xs...)
    jetconfigs = filter(iskwarg, xs)
    xs′ = filter(!iskwarg, xs)
    @assert length(xs′) == 1
    ex = first(xs′)
    return _analyze_toplevel(ex, __source__, jetconfigs)
end

iskwarg(@nospecialize(x)) = isexpr(x, :(=))

function _analyze_toplevel(ex, lnn, jetconfigs)
    toplevelex = (isexpr(ex, :block) ?
                  Expr(:toplevel, lnn, ex.args...) : # flatten here
                  Expr(:toplevel, lnn, ex)
                  ) |> QuoteNode
    return :(let
        interp = JETInterpreter(; $(map(esc, jetconfigs)...))
        config = ToplevelConfig(; $(map(esc, jetconfigs)...))
        $virtual_process($toplevelex,
                         $(string(lnn.file)),
                         interp,
                         config,
                         )
    end)
end

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
