using JET, InteractiveUtils

const CC = JET.CC

using .CC: Bottom, widenconst, ⊑

using JET:
    AbstractAnalyzer, InferenceErrorReport, JETAnalyzer, ToplevelConfig,
    ToplevelErrorReport, gen_virtual_module, get_reports, get_result, print_reports,
    virtual_process, virtualize_module_context

using Base.Meta: isexpr

get_cache(analyzer::AbstractAnalyzer) = JET.get_inf_cache(analyzer)

let ts = Type[]
    function subtypes_recursive!(t)
        push!(ts, t)
        if isabstracttype(t)
            for ct in subtypes(t)
                subtypes_recursive!(ct)
            end
        end
    end
    subtypes_recursive!(ToplevelErrorReport)
    subtypes_recursive!(InferenceErrorReport)
    for t in ts
        canonicalname = Symbol(parentmodule(t), '.', nameof(t))
        canonicalpath = Symbol.(split(string(canonicalname), '.'))
        modpath = Expr(:., canonicalpath[1:end-1]...)
        symname = Expr(:., last(canonicalpath))
        ex = Expr(:import, Expr(:(:), modpath, symname))
        Core.eval(@__MODULE__, ex)
    end
end

iskwarg(@nospecialize(x)) = isexpr(x, :(=))

"""
    @fixturedef

Creates a virtual module and defines fixtures from toplevel expression `ex`
"""
macro fixturedef(ex)
    @assert isexpr(ex, :block)
    return :(let
        vmod = gen_virtual_module()
        for x in $(ex.args)
            Core.eval(vmod, x)
        end
        vmod # return virtual module
    end)
end

"""
    @analyze_toplevel [jetconfigs...] ex -> JET.JETToplevelResult

Enters JET analysis from toplevel expression `ex`, and returns the analysis result.
"""
macro analyze_toplevel(xs...)
    jetconfigs = filter(iskwarg, xs)
    xs′ = filter(!iskwarg, xs)
    @assert length(xs′) == 1
    ex = first(xs′)
    return _analyze_toplevel_ex(ex, __source__, jetconfigs)
end

"""
    @analyze_toplevel2 [jetconfigs...] ex -> (Module, JET.JETToplevelResult)

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
    ex = _analyze_toplevel_ex(ex, __source__, jetconfigs)
    vmodname = esc(vmod)
    return :(let
        $vmodname = gen_virtual_module()
        ret = $ex
        $vmodname, ret
    end)
end

_analyze_toplevel_ex(ex, lnn, jetconfigs) =
    :(analyze_toplevel($(QuoteNode(ex)), $(QuoteNode(lnn)); $(map(esc, jetconfigs)...)))

function analyze_toplevel(ex, lnn; jetconfigs...)
    toplevelex = (isexpr(ex, :block) ?
                  Expr(:toplevel, lnn, ex.args...) : # flatten here
                  Expr(:toplevel, lnn, ex))
    analyzer = JETAnalyzer(; jetconfigs...)
    config = ToplevelConfig(; jetconfigs...)
    filename = let file = lnn.file; isnothing(file) ? "top-level" : String(file) end
    res = virtual_process(toplevelex, filename, analyzer, config)
    return JET.JETToplevelResult(analyzer, res, "analyze_toplevel"; jetconfigs...)
end

# `report_file` with silent top-level logger
report_file2(args...; kwargs...) = report_file(args...; toplevel_logger = nothing, kwargs...)

isconcrete(res::JET.JETToplevelResult, args...) = isconcrete(res.analyzer, args...)
isconcrete(analyzer::JET.AbstractAnalyzer, mod::Module, sym::Symbol) =
    @invokelatest(isdefinedglobal(mod, sym)) && !isabstract(analyzer, mod, sym)

isabstract(res::JET.JETToplevelResult, args...) = isabstract(res.analyzer, args...)
function isabstract(analyzer::JET.AbstractAnalyzer, mod::Module, sym::Symbol)
    binding = convert(Core.Binding, GlobalRef(mod, sym))
    return haskey(JET.get_binding_states(analyzer), binding.partitions)
end

isabstractconcrete(res::JET.JETToplevelResult, args...) = isabstractconcrete(res.analyzer, args...)
function isabstractconcrete(analyzer::JET.AbstractAnalyzer, mod::Module, sym::Symbol, T=nothing)
    binding = convert(Core.Binding, GlobalRef(mod, sym))
    binding_state = get(JET.get_binding_states(analyzer), binding.partitions, nothing)
    binding_state === nothing && return false
    if isdefined(binding_state, :typ)
        typ = binding_state.typ
        if typ isa Const
            if T === nothing
                return true
            else
                return typ.value isa T
            end
        end
    end
    return false
end

# JET will try to concretize global variable when its type is a constant at analysis time,
# but the starategy is a bit complicated right now and may change in the future
# these utilities allow robust testing to check if a object is successfully analyzed by JET whichever it's concretized or abstracted
isanalyzed(args...) = isconcrete(args...) || isabstract(args...)

function is_global_undef_var(@nospecialize(r::InferenceErrorReport), mod::Module, name::Symbol)
    r isa UndefVarErrorReport || return false
    var = r.var
    return var isa GlobalRef && var.mod === mod && var.name === name
end
function is_global_undef_var(@nospecialize(r::InferenceErrorReport), name::Symbol)
    r isa UndefVarErrorReport || return false
    var = r.var
    return var isa GlobalRef && var.name === name
end

function is_local_undef_var(@nospecialize(r::InferenceErrorReport), name::Symbol)
    r isa UndefVarErrorReport || return false
    var = r.var
    return var === name
end

# for inspection
macro lwr(ex) QuoteNode(Meta.lower(__module__, ex)) end
macro src(ex) QuoteNode(only(Meta.lower(__module__, ex).args)) end
