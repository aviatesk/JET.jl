module JET

let
    README = normpath(dirname(@__DIR__), "README.md")
    include_dependency(README)
    @doc read(README, String) JET
end

# not sure why, but a benchmark showed this is faster
Base.Experimental.@optlevel 1

const CC = Core.Compiler

# imports
# =======

# `AbstractAnalyzer`
import .CC:
    #= cicache.jl =#
    # haskey,
    # get,
    # getindex,
    # setindex!,
    # push!,
    #= types.jl =#
    InferenceParams,
    OptimizationParams,
    get_world_counter,
    get_inference_cache,
    code_cache,
    lock_mi_inference,
    unlock_mi_inference,
    add_remark!,
    may_optimize,
    may_compress,
    may_discard_trees,
    verbose_stmt_info,
    bail_out_toplevel_call,
    bail_out_call,
    #= inferenceresult.jl =#
    cache_lookup,
    #= inferencestate.jl =#
    InferenceState,
    #= tfuncs.jl =#
    builtin_tfunction,
    return_type_tfunc,
    #= abstractinterpretation.jl =#
    abstract_call_gf_by_type,
    add_call_backedges!,
    abstract_call_method_with_const_args,
    const_prop_entry_heuristic,
    abstract_call_method,
    abstract_invoke,
    abstract_call,
    abstract_eval_special_value,
    abstract_eval_value,
    abstract_eval_statement,
    #= typeinfer.jl =#
    typeinf,
    _typeinf,
    finish,
    transform_result_for_cache,
    finish!,
    typeinf_edge,
    #= optimize.jl =#
    inlining_policy

# `ConcreteInterpreter`
import JuliaInterpreter:
    #= interpreter.jl =#
    step_expr!,
    evaluate_call_recurse!,
    handle_err

# Test.jl integration
import Test:
    record

# usings
# ======
# TODO: really use `using` instead

import Core:
    Builtin,
    Intrinsics,
    IntrinsicFunction,
    MethodMatch,
    LineInfoNode,
    CodeInfo,
    CodeInstance,
    MethodInstance,
    NewvarNode,
    GlobalRef,
    GotoNode,
    GotoIfNot,
    Const,
    SSAValue,
    SlotNumber,
    Argument,
    Slot,
    ReturnNode,
    SimpleVector,
    svec

import .CC:
    AbstractInterpreter,
    NativeInterpreter,
    InferenceResult,
    InternalCodeCache,
    OptimizationState,
    WorldRange,
    WorldView,
    Bottom,
    LimitedAccuracy,
    NOT_FOUND,
    MethodCallResult,
    MethodMatches,
    UnionSplitMethodMatches,
    MethodMatchInfo,
    UnionSplitInfo,
    ConstCallInfo,
    InvokeCallInfo,
    MethodLookupResult,
    VarState,
    VarTable,
    CallMeta,
    CFG,
    BasicBlock,
    slot_id,
    widenconst,
    ⊑,
    is_throw_call,
    tmerge,
    switchtupleunion,
    argtypes_to_type,
    singleton_type,
    argtype_by_index,
    argtype_tail,
    _methods_by_ftype,
    specialize_method,
    add_backedge!,
    add_mt_backedge!,
    compute_basic_blocks,
    may_invoke_generator,
    inlining_enabled,
    instanceof_tfunc,
    ignorelimited,
    argextype

import Base:
    # @constprop,
    parse_input_line,
    unwrap_unionall,
    rewrap_unionall,
    uniontypes,
    @invokelatest,
    destructure_callex

import Base.Meta:
    _parse_string,
    lower,
    isexpr

using LoweredCodeUtils, JuliaInterpreter

import LoweredCodeUtils:
    istypedef,
    ismethod,
    callee_matches,
    rng,
    print_with_code,
    pushall!,
    # NamedVar,
    # add_requests!,
    add_ssa_preds!,
    # add_named_dependencies!,
    find_typedefs,
    add_control_flow!,
    add_typedefs!

import JuliaInterpreter:
    _INACTIVE_EXCEPTION,
    bypass_builtins,
    maybe_evaluate_builtin,
    collect_args,
    # finish!,
    is_return,
    is_quotenode_egal,
    moduleof,
    @lookup

import MacroTools: MacroTools, @capture, normalise, striplines

using InteractiveUtils

using Pkg, Pkg.TOML

import Test:
    Test,
    Result, Pass, Fail, Broken, Error,
    get_testset,
    TESTSET_PRINT_ENABLE,
    FallbackTestSet, DefaultTestSet,
    FallbackTestSetException

# common
# ======

const Argtypes = Vector{Any}

const JET_DEV_MODE = parse(Bool, get(ENV, "JET_DEV_MODE", "false"))

const CONFIG_FILE_NAME = ".JET.toml"

# hooks
# -----

const INIT_HOOKS = Function[]
push_inithook!(f) = push!(INIT_HOOKS, f)
__init__() = foreach(@nospecialize(f)->f(), INIT_HOOKS)

# compat
# ------

# branch on https://github.com/JuliaLang/julia/pull/42082
const IS_AFTER_42082 = hasmethod(InferenceState, (InferenceResult, Symbol, AbstractInterpreter))

const IS_AFTER_42529 = isdefined(CC, :ArgInfo)
IS_AFTER_42529 && import .CC: ArgInfo

# branch on https://github.com/JuliaLang/julia/pull/42125
@static if isdefined(Base, Symbol("@constprop"))
    import Base: @constprop
else
    macro constprop(_, ex); esc(ex); end
end

# macros
# ------

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

iskwarg(@nospecialize(x)) = isexpr(x, :(=))

# for inspection
macro lwr(ex) QuoteNode(lower(__module__, ex)) end
macro src(ex) QuoteNode(first(lower(__module__, ex).args)) end

"""
    @invoke f(arg::T, ...; kwargs...)

Provides a convenient way to call [`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke);
`@invoke f(arg1::T1, arg2::T2; kwargs...)` will be expanded into `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)`.
When an argument's type annotation is omitted, it's specified as `Any` argument, e.g.
`@invoke f(arg1::T, arg2)` will be expanded into `invoke(f, Tuple{T,Any}, arg1, arg2)`.

This could be used to call down to `NativeInterpreter`'s abstract interpretation method of
`f` while passing `AbstractAnalyzer` so that subsequent calls of abstract interpretation
functions overloaded against `AbstractAnalyzer` can be called from the native method of `f`.
E.g. `@invoke` can be used to call down to `NativeInterpreter`'s `abstract_call_gf_by_type`:
```julia
@invoke abstract_call_gf_by_type(analyzer::AbstractInterpreter, f, argtypes::Argtypes, atype, sv::InferenceState,
                                 max_methods::Int)
```
"""
macro invoke(ex)
    f, args, kwargs = destructure_callex(ex)
    arg2typs = map(args) do x
        if isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize")
            x = last(x.args)
        end
        isexpr(x, :(::)) ? (x.args...,) : (x, GlobalRef(Core, :Any))
    end
    args, argtypes = first.(arg2typs), last.(arg2typs)
    return esc(:($(GlobalRef(Core, :invoke))($(f), Tuple{$(argtypes...)}, $(args...); $(kwargs...))))
end

"""
    @withmixedhash (mutable) struct T
        fields ...
    end

Defines struct `T` while automatically defining its `Base.hash(::T, ::UInt)` method which
mixes hashes of all of `T`'s fields (and also corresponding `Base.:(==)(::T, ::T)` method).

This macro is supposed to abstract the following kind of pattern:

> https://github.com/aviatesk/julia/blob/999973df2850d6b2e0bd4bcf03ef90a14217b63c/base/pkgid.jl#L3-L25
```julia
struct PkgId
    uuid::Union{UUID,Nothing}
    name::String
end

==(a::PkgId, b::PkgId) = a.uuid == b.uuid && a.name == b.name

function hash(pkg::PkgId, h::UInt)
    h += 0xc9f248583a0ca36c % UInt
    h = hash(pkg.uuid, h)
    h = hash(pkg.name, h)
    return h
end
```

> with `@withmixedhash`
```julia
@withmixedhash struct PkgId
    uuid::Union{UUID,Nothing}
    name::String
end
```
"""
macro withmixedhash(typedef)
    @assert isexpr(typedef, :struct) "struct definition should be given"
    name = esc(typedef.args[2])
    fld2typs = filter(!isnothing, map(filter(!islnn, typedef.args[3].args)) do x
        if isexpr(x, :(::))
            fld, typex = x.args
            typ = Core.eval(__module__, typex)
            fld, typ
        elseif isa(x, Symbol)
            (x, Any)
        else # constructor etc.
            nothing
        end
    end)
    @assert !isempty(fld2typs) "no fields given, nothing to hash"

    h_init = UInt === UInt64 ? rand(UInt64) : rand(UInt32)
    hash_body = quote h = $h_init end
    for (fld, typ) in fld2typs
        push!(hash_body.args, :(h = Base.hash(x.$fld, h)::UInt))
    end
    push!(hash_body.args, :(return h))
    hash_func = :(function Base.hash(x::$name, h::UInt); $hash_body; end)
    eq_body = foldr(fld2typs; init = true) do (fld, typ), x
        if typ in _EGAL_TYPES
            eq_ex = :(x1.$fld === x2.$fld)
        else
            eq_ex = :((x1.$fld == x2.$fld)::Bool)
        end
        Expr(:&&, eq_ex, x)
    end
    eq_func = :(function Base.:(==)(x1::$name, x2::$name); $eq_body; end)

    return quote
        Base.@__doc__ $typedef
        $hash_func
        $eq_func
    end
end

# types that should be compared by `===` rather than `==`
const _EGAL_TYPES = Any[Symbol, MethodInstance, Type]

"""
    @jetconfigurable function config_func(args...; configurations...)
        ...
    end

This macro asserts that there's no configuration naming conflict across the `@jetconfigurable`
functions so that a configuration for a `@jetconfigurable` function  doesn't affect the other
`@jetconfigurable` functions.
This macro also adds a dummy splat keyword arguments (`jetconfigs...`) to the function definition
so that any configuration of other `@jetconfigurable` functions can be passed on to it.
"""
macro jetconfigurable(funcdef)
    @assert isexpr(funcdef, :(=)) || isexpr(funcdef, :function) "function definition should be given"

    defsig = funcdef.args[1]
    if isexpr(defsig, :where)
        defsig = first(defsig.args)
    end
    args = defsig.args::Vector{Any}
    thisname = first(args)
    i = findfirst(a->isexpr(a, :parameters), args)
    if isnothing(i)
        @warn "no JET configurations are defined for `$thisname`"
        insert!(args, 2, Expr(:parameters, :(jetconfigs...)))
    else
        kwargs = args[i]
        found = false
        for kwarg in kwargs.args
            if isexpr(kwarg, :...)
                found = true
                continue
            end
            kwargex = first(kwarg.args)
            kwargname = (isexpr(kwargex, :(::)) ? first(kwargex.args) : kwargex)::Symbol
            othername = get!(_JET_CONFIGURATIONS, kwargname, thisname)
            # allows same configurations for same generic function or function refinement
            @assert thisname == othername "`$thisname` uses `$kwargname` JET configuration name which is already used by `$othername`"
        end
        found || push!(kwargs.args, :(jetconfigs...))
    end

    return esc(funcdef)
end
const _JET_CONFIGURATIONS = Dict{Symbol,Union{Symbol,Expr}}()

# utils
# -----

# state

const State     = Union{InferenceState,OptimizationState}
const StateAtPC = Tuple{State,Int}
const LineTable = Union{Vector{Any},Vector{LineInfoNode}}

get_stmt((sv, pc)::StateAtPC)         = @inbounds sv.src.code[pc]
get_lin((sv, pc)::StateAtPC)          = begin
    codeloc = sv.src.codelocs[pc]
    linetable = sv.src.linetable::LineTable
    if 1 <= codeloc <= length(linetable)
        return @inbounds linetable[codeloc]::LineInfoNode
    else
        # Packages might dynamically generate code, which does not reference
        # a source, see https://github.com/aviatesk/JET.jl/issues/273
        return LineInfoNode(sv.mod, :unknown, :unknown, 0, 0)
    end
end
get_ssavaluetype((sv, pc)::StateAtPC) = @inbounds sv.src.ssavaluetypes[pc]

get_slottype(s::Union{StateAtPC,State}, slot)      = get_slottype(s, slot_id(slot))
get_slottype((sv, pc)::StateAtPC,       slot::Int) = get_slottype(sv, slot)
get_slottype(sv::State,                 slot::Int) = @inbounds sv.slottypes[slot]

get_slotname(s::Union{StateAtPC,State}, slot)      = get_slotname(s, slot_id(slot))
get_slotname((sv, pc)::StateAtPC,       slot::Int) = @inbounds sv.src.slotnames[slot]
get_slotname(sv::State,                 slot::Int) = @inbounds sv.src.slotnames[slot]

# check if we're in a toplevel module
istoplevel(sv::State)             = istoplevel(sv.linfo)
istoplevel(linfo::MethodInstance) = isa(linfo.def, Module)

# we can retrieve program-counter-level slottype during inference
get_slottype(s::Tuple{InferenceState,Int}, slot::Int) = @inbounds (get_states(s)[slot]::VarState).typ
get_states((sv, pc)::Tuple{InferenceState,Int})       = @inbounds sv.stmt_types[pc]::VarTable
get_currpc(sv::InferenceState) = min(sv.currpc, length(sv.src.code))

isconcreteframe(x) = isdispatchtuple(get_linfo(x).specTypes)

get_linfo(sv::State)               = sv.linfo
get_linfo(result::InferenceResult) = result.linfo
get_linfo(linfo::MethodInstance)   = linfo

is_constant_propagated(frame::InferenceState) =
    return !frame.cached && # const-prop'ed frame is never cached globally
           is_constant_propagated(frame.result)
is_constant_propagated(result::InferenceResult) = CC.any(result.overridden_by_const)

prewalk_inf_frame(@nospecialize(f), ::Nothing) = return
function prewalk_inf_frame(@nospecialize(f), frame::InferenceState)
    ret = f(frame)
    prewalk_inf_frame(f, frame.parent)
    return ret
end

postwalk_inf_frame(@nospecialize(f), ::Nothing) = return
function postwalk_inf_frame(@nospecialize(f), frame::InferenceState)
    postwalk_inf_frame(f, frame.parent)
    return f(frame)
end

# XXX this should be upstreamed
function Base.show(io::IO, frame::InferenceState)
    print(io, "InfernceState for ")
    show(io, frame.linfo)
    print(io, " at pc ", frame.currpc, '/', length(frame.src.code))
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", frame::InferenceState) =
    return frame

# lattice

ignorenotfound(@nospecialize(t)) = t === NOT_FOUND ? Bottom : t

# analysis core
# =============

include("abstractinterpret/inferenceerrorreport.jl")
include("abstractinterpret/abstractanalyzer.jl")
include("abstractinterpret/typeinfer.jl")

include("toplevel/graph.jl")
include("toplevel/virtualprocess.jl")

# results
# =======

"""
    res::JETToplevelResult

Represents the result of JET's analysis on a top-level script.
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.res::VirtualProcessResult`: [`VirtualProcessResult`](@ref) collected from this analysis
- `res.source::String`: the identity key of this analysis
- `res.jetconfigs`: [JET configurations](@ref JET-configurations) used for this analysis

`JETToplevelResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically choosen and render the analysis result.
"""
struct JETToplevelResult{Analyzer<:AbstractAnalyzer,JETConfigs}
    analyzer::Analyzer
    res::VirtualProcessResult
    source::String
    jetconfigs::JETConfigs
end
JETToplevelResult(analyzer::AbstractAnalyzer, res::VirtualProcessResult, source::AbstractString;
                  jetconfigs...) = JETToplevelResult(analyzer, res, source, jetconfigs)
@eval Base.iterate(res::JETToplevelResult, state=1) =
    return state > $(fieldcount(JETToplevelResult)) ? nothing : (getfield(res, state), state+1)

function get_reports(result::JETToplevelResult)
    res = result.res
    if !isempty(res.toplevel_error_reports)
        # non-empty `ret.toplevel_error_reports` means critical errors happened during
        # the AST transformation, so they always have precedence over `ret.inference_error_reports`
        return res.toplevel_error_reports
    else
        reports = res.inference_error_reports
        return _get_configured_reports(reports; res.defined_modules, result.jetconfigs...)
    end
end

"""
    res::JETCallResult

Represents the result of JET's analysis on a function call.
- `res.result::InferenceResult`: the result of this analysis
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.source::String`: the identity key of this analysis
- `res.jetconfigs`: [JET configurations](@ref JET-configurations) used for this analysis

`JETCallResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically choosen and render the analysis result.
"""
struct JETCallResult{Analyzer<:AbstractAnalyzer,JETConfigs}
    result::InferenceResult
    analyzer::Analyzer
    source::String
    jetconfigs::JETConfigs
end
JETCallResult(result::InferenceResult, analyzer::AbstractAnalyzer, source::AbstractString;
              jetconfigs...) = JETCallResult(result, analyzer, source, jetconfigs)
@eval Base.iterate(res::JETCallResult, state=1) =
    return state > $(fieldcount(JETCallResult)) ? nothing : (getfield(res, state), state+1)

get_result(result::JETCallResult) = get_result(result.result)
function get_result(result::InferenceResult)
    if any(r->isa(r, GeneratorErrorReport), get_reports(result))
        return Bottom
    else
        return result.result
    end
end
function get_reports(result::JETCallResult)
    reports = get_reports(result.result)
    return _get_configured_reports(reports; result.jetconfigs...)
end

"""
Configurations for [JET's analysis results](@ref analysis-result).
These configurations should be active always.

---
- `target_modules = nothing` \\
  Filters out collected reports based on their module context.
  By default (`target_modules = nothing`), JET prints any collected reports.
  If specified `target_modules` should be an iterator of `Module`s, and then JET will ignore
  whose error point is not in the specified module context.

  > Examples:
  ```julia-repl
  julia> function foo(a)
             r1 = sum(a)       # => Base: MethodError(+(::Char, ::Char)), MethodError(zero(::Type{Any}))
             r2 = undefsum(a)  # => @__MODULE__: UndefVarError(:undefsum)
             return r1, r2
         end;

  # by default, JET will print all the collected reports:
  julia> @report_call foo("juila")
  ════ 3 possible errors found ═════
  ┌ @ none:2 r1 = Main.sum(a)
  │┌ @ reduce.jl:544 Base.#sum#258(Base.pairs(Core.NamedTuple()), #self#, a)
  ││┌ @ reduce.jl:544 Base.sum(Base.identity, a)
  │││┌ @ reduce.jl:515 Base.#sum#257(Base.pairs(Core.NamedTuple()), #self#, f, a)
  ││││┌ @ reduce.jl:515 Base.mapreduce(f, Base.add_sum, a)
  │││││┌ @ reduce.jl:289 Base.#mapreduce#254(Base.pairs(Core.NamedTuple()), #self#, f, op, itr)
  ││││││┌ @ reduce.jl:289 Base.mapfoldl(f, op, itr)
  │││││││┌ @ reduce.jl:162 Base.#mapfoldl#250(Base._InitialValue(), #self#, f, op, itr)
  ││││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f, op, init, itr)
  │││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
  ││││││││││┌ @ reduce.jl:48 v = Base._foldl_impl(op, nt, itr)
  │││││││││││┌ @ reduce.jl:62 v = op(v, Base.getindex(y, 1))
  ││││││││││││┌ @ reduce.jl:81 Base.getproperty(op, :rf)(acc, x)
  │││││││││││││┌ @ reduce.jl:24 Base.+(x, y)
  ││││││││││││││ no matching method found for call signature (Tuple{typeof(+), Char, Char}): Base.+(x::Char, y::Char)
  │││││││││││││└────────────────
  ││││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op, itr)
  │││││││││││┌ @ reduce.jl:365 Base.reduce_empty_iter(op, itr, Base.IteratorEltype(itr))
  ││││││││││││┌ @ reduce.jl:366 Base.reduce_empty(op, Base.eltype(itr))
  │││││││││││││┌ @ reduce.jl:342 Base.reduce_empty(Base.getproperty(op, :rf), _)
  ││││││││││││││┌ @ reduce.jl:334 Base.reduce_empty(Base.+, _)
  │││││││││││││││┌ @ reduce.jl:325 Base.zero(_)
  ││││││││││││││││ no matching method found for call signature (Tuple{typeof(zero), Type{Char}}): Base.zero(_::Type{Char})
  │││││││││││││││└─────────────────
  ┌ @ none:3 r2 = Main.undefsum(a)
  │ variable Main.undefsum is not defined: r2 = Main.undefsum(a::String)
  └──────────

  # with `target_modules=(@__MODULE__,)`, JET will ignore the errors detected within the `Base` module:
  julia> @report_call target_modules=(@__MODULE__,) foo("juila")
  ════ 1 possible error found ═════
  ┌ @ none:3 r2 = Main.undefsum(a)
  │ variable Main.undefsum is not defined: r2 = Main.undefsum(a::String)
  └──────────
  ```
---
"""
@jetconfigurable function _get_configured_reports(
    reports::Vector{InferenceErrorReport};
    target_modules = nothing,
    # only used for `JETToplevelResult`
    target_defined_modules::Bool = false, defined_modules = nothing,
    __jetconfigs...)
    if target_defined_modules
        target_modules = defined_modules
    end
    isnothing(target_modules) && return reports
    return filter(reports) do report
        def = last(report.vst).linfo.def
        mod = isa(def, Method) ? def.module : def
        mod in target_modules && return true
        if isa(report, NoFieldErrorReport)
            if length(report.vst) ≥ 2
                def = report.vst[end-1].linfo.def
                mod = isa(def, Method) ? def.module : def
                mod in target_modules && return true
            end
        end
        return false
    end
end

# UIs
# ===

# when virtualized, fix virtual module printing based on string manipulation;
# the "actual" modules may not be loaded into this process
gen_postprocess(::Nothing) = return identity
function gen_postprocess((actualmod, virtualmod)::Actual2Virtual)
    virtual = string(virtualmod)
    actual  = string(actualmod)
    return actualmod === Main ?
           replace2("Main." => "") ∘ replace2(virtual => actual) :
           replace2(virtual => actual)
end
replace2(pat) = x -> replace(x, pat)

# we may need something like this for stdlibs as well ?

function tofullpath(filename::AbstractString)
    path = abspath(filename)
    return isfile(path) ? path : fullbasepath(filename)
end
fullbasepath(filename) = normpath(JULIA_DIR, "base", filename)

# TODO make this configurable ?
const JULIA_DIR = @static occursin("DEV", string(VERSION)) ?
                  normpath(Sys.BINDIR, "..", "..", "..", "julia") :
                  normpath(Sys.BINDIR, Base.DATAROOTDIR)

# default UI (console)
include("ui/print.jl")
# UI for VSCode
include("ui/vscode.jl")

# entries
# =======

# abstractinterpret
# -----------------

# TODO `analyze_builtin!` ?
function analyze_gf_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}); kwargs...)
    mm = get_single_method_match(tt, InferenceParams(analyzer).MAX_METHODS, get_world_counter(analyzer))
    return analyze_method_signature!(analyzer, mm.method, mm.spec_types, mm.sparams; kwargs...)
end

function get_single_method_match(@nospecialize(tt), lim, world)
    mms = _methods_by_ftype(tt, lim, world)
    @assert !isa(mms, Bool) "unable to find matching method for $(tt)"

    filter!(mm::MethodMatch->mm.spec_types===tt, mms)
    @assert length(mms) == 1 "unable to find single target method for $(tt)"

    return first(mms)::MethodMatch
end

analyze_method!(analyzer::AbstractAnalyzer, m::Method; kwargs...) =
    analyze_method_signature!(analyzer, m, m.sig, method_sparams(m); kwargs...)

function method_sparams(m::Method)
    s = TypeVar[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(s, sig.var)
        sig = sig.body
    end
    return svec(s...)
end

function analyze_method_signature!(analyzer::AbstractAnalyzer, m::Method, @nospecialize(atype), sparams::SimpleVector; kwargs...)
    mi = specialize_method(m, atype, sparams)::MethodInstance
    return analyze_method_instance!(analyzer, mi; kwargs...)
end

function analyze_method_instance!(analyzer::AbstractAnalyzer, mi::MethodInstance;
                                  set_entry::Bool = true,
                                  )
    result = InferenceResult(mi)

    @static if IS_AFTER_42082
        frame = InferenceState(result, #=cache=# :global, analyzer)
    else # @static if IS_AFTER_42082
        frame = InferenceState(result, #=cached=# true, analyzer)
    end # @static if IS_AFTER_42082

    isnothing(frame) && return analyzer, result

    set_entry && set_entry!(analyzer, mi)
    return analyze_frame!(analyzer, frame)
end

const CACHE_ARG_TYPE = IS_AFTER_42082 ? Symbol : Bool

function InferenceState(result::InferenceResult, cache::CACHE_ARG_TYPE, analyzer::AbstractAnalyzer)
    set_result!(result) # modify `result` for succeeding JET analysis
    return @invoke InferenceState(result::InferenceResult, cache::CACHE_ARG_TYPE, analyzer::AbstractInterpreter)
end

function analyze_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    typeinf(analyzer, frame)
    return analyzer, frame.result
end

# toplevel
# --------

"""
    report_file(filename::AbstractString;
                jetconfigs...) -> JETToplevelResult

Analyzes `filename` and returns [`JETToplevelResult`](@ref).

This function will look for `$CONFIG_FILE_NAME` configuration file in the directory of `filename`,
and search _up_ the file tree until any `$CONFIG_FILE_NAME` is (or isn't) found.
When found, the configurations specified in the file will be applied.
See [JET's configuration file](@ref config-file) for more details.

!!! tip
    When you want to analyze your package, but any file actually using it isn't available, the
    `analyze_from_definitions` option can be useful (see [`ToplevelConfig`](@ref)'s `analyze_from_definitions` option). \\
    For example, JET can analyze JET itself like below:
    ```julia-repl
    # from the root directory of JET.jl
    julia> report_file("src/JET.jl";
                       analyze_from_definitions = true)
    ```

    See also: [`report_package`](@ref)

!!! note
    This function will enable the `toplevel_logger` configuration by default with the default logging level.
    You can still explicitly specify and configure it:
    ```julia
    report_file(args...;
                toplevel_logger = nothing, # suppress toplevel logger
                jetconfigs...) # other configurations
    ```
    See [JET's logging configurations](@ref logging-config) for more details.
"""
function report_file(filename::AbstractString;
                     __default_configs = (default_toplevel_logger_config(),),
                     source::Union{Nothing,AbstractString} = nothing,
                     jetconfigs...) where T
    isfile(filename) || throw(ArgumentError("$filename doesn't exist"))

    configfile = find_config_file(dirname(abspath(filename)))
    if isnothing(configfile)
        for default in __default_configs
            jetconfigs = set_if_missing(jetconfigs, default)
        end
    else
        config = parse_config_file(configfile)
        jetconfigs = overwrite_options(config, jetconfigs)
        for default in __default_configs
            jetconfigs = set_if_missing(jetconfigs, default)
        end
        toplevel_logger = get(jetconfigs, :toplevel_logger, nothing)
        if isa(toplevel_logger, IO)
            with_logger(toplevel_logger, :toplevel, ≥(INFO_LOGGER_LEVEL)) do @nospecialize(io)
                println(io, "applied JET configurations in $configfile")
            end
        end
    end

    if isnothing(source)
        source = string(nameof(var"#self#"), "(\"$filename\")")
    end

    return report_text(read(filename, String), filename; source, jetconfigs...)
end

default_toplevel_logger_config() =
    return :toplevel_logger => IOContext(stdout::IO, LOGGER_LEVEL_KEY => DEFAULT_LOGGER_LEVEL)

function set_if_missing(@nospecialize(jetconfigs), (key, value))
    haskey(jetconfigs, key) && return jetconfigs
    default = kwargs((; key => value))
    return overwrite_options(jetconfigs, default)
end

function find_config_file(dir)
    next_dir = dirname(dir)
    if (next_dir == dir || # ensure to escape infinite recursion
        isempty(dir))      # reached to the system root
        return nothing
    end
    path = normpath(dir, CONFIG_FILE_NAME)
    return isfile(path) ? path : find_config_file(next_dir)
end

"""
JET.jl offers [`.prettierrc` style](https://prettier.io/docs/en/configuration.html)
configuration file support.
This means you can use `$CONFIG_FILE_NAME` configuration file to specify any of configurations
explained above and share that with others.

When [`report_file`](@ref) or [`report_and_watch_file`](@ref) is called, it will look for
`$CONFIG_FILE_NAME` in the directory of the given file, and search _up_ the file tree until
a JET configuration file is (or isn't) found.
When found, the configurations specified in the file will be applied.

A configuration file can specify any of JET configurations like:
```toml
aggressive_constant_propagation = false # turn off aggressive constant propagation
... # other configurations
```

Note that the following configurations should be string(s) of valid Julia code:
- `context`: string of Julia code, which can be `parse`d and `eval`uated into `Module`
- `concretization_patterns`: vector of string of Julia code, which can be `parse`d into a
  Julia expression pattern expected by [`MacroTools.@capture` macro](https://fluxml.ai/MacroTools.jl/stable/pattern-matching/).
- `toplevel_logger`: string of Julia code, which can be `parse`d and `eval`uated into `Union{IO,Nothing}`
- `inference_logger`: string of Julia code, which can be `parse`d and `eval`uated into `Union{IO,Nothing}`

E.g. the configurations below are equivalent:
- configurations via keyword arguments
  ```julia
  report_file(somefile;
              concretization_patterns = [:(const GLOBAL_CODE_STORE = x_)],
              toplevel_logger = IOContext(open("toplevel.txt", "w"), :JET_LOGGER_LEVEL => 1))
  ```
- configurations via a configuration file
  $(let
      text = read(normpath(@__DIR__, "..", "test", "fixtures", "..JET.toml"), String)
      lines = split(text, '\n')
      pushfirst!(lines, "```toml"); push!(lines, "```")
      join(lines, "\n  ")
  end)

!!! note
    JET configurations specified as keyword arguments have precedence over those specified
    via a configuration file.
"""
parse_config_file(path) = process_config_dict!(TOML.parsefile(path))

function process_config_dict!(config_dict)
    context = get(config_dict, "context", nothing)
    if !isnothing(context)
        @assert isa(context, String) "`context` should be string of Julia code"
        config_dict["context"] = Core.eval(Main, trymetaparse(context))
    end
    concretization_patterns = get(config_dict, "concretization_patterns", nothing)
    if !isnothing(concretization_patterns)
        @assert isa(concretization_patterns, Vector{String}) "`concretization_patterns` should be array of string of Julia expression"
        config_dict["concretization_patterns"] = trymetaparse.(concretization_patterns)
    end
    toplevel_logger = get(config_dict, "toplevel_logger", nothing)
    if !isnothing(toplevel_logger)
        @assert isa(toplevel_logger, String) "`toplevel_logger` should be string of Julia code"
        config_dict["toplevel_logger"] = Core.eval(Main, trymetaparse(toplevel_logger))
    end
    inference_logger = get(config_dict, "inference_logger", nothing)
    if !isnothing(inference_logger)
        @assert isa(inference_logger, String) "`inference_logger` should be string of Julia code"
        config_dict["inference_logger"] = Core.eval(Main, trymetaparse(inference_logger))
    end
    return kwargs(config_dict)
end

function trymetaparse(s)
    ret = Meta.parse(strip(s); raise = true)
    isexpr(ret, :incomplete) && error(first(ret.args))
    return ret
end

function kwargs(dict)
    ns = (Symbol.(keys(dict))...,)
    vs = (collect(values(dict))...,)
    return pairs(NamedTuple{ns}(vs))
end

overwrite_options(old, new) = kwargs(merge(old, new))

"""
    report_package(package::Union{AbstractString,Module};
                   jetconfigs...) -> JETToplevelResult

Analyzes `package` in the same way as [`report_file`](@ref) with the special default
configurations, which are especially tuned for package analysis (see below for details).
`package` can be either a `Module` or a `String`.
In the latter case it must be the name of a package in your current environment.

This function configures analysis with the following configurations:
- `analyze_from_definitions = true`: allows JET to enter analysis without top-level call sites;
  this is useful for package analysis since a package itself usually has only definitions
  but not usages (i.e. call sites)
- `concretization_patterns = [:(x_)]`: concretizes every top-level code in a given `package`;
  concretizations are generally preferred for successful analysis as far as they're cheap,
  and a package definition doesn't contain heavy computations in general cases
See [`ToplevelConfig`](@ref) for more details.

---

    report_package([io::IO = stdout];
                   jetconfigs...) -> res::ReportResult

Like above but analyzes the package of the current project.

See also: [`report_file`](@ref)
"""
function report_package(package::Union{AbstractString,Module,Nothing} = nothing;
                        analyze_from_definitions::Bool = true,
                        concretization_patterns = [:(x_)], # concretize all top-level code
                        jetconfigs...)
    filename = get_package_file(package)
    __default_configs = ( # allow a configuration file to overwrite these configurations
        default_toplevel_logger_config(),
        :analyze_from_definitions => analyze_from_definitions,
        :concretization_patterns => concretization_patterns)
    return report_file(filename; __default_configs, jetconfigs...)
end

function get_package_file(package::AbstractString)
    filename = Base.find_package(package)
    isnothing(filename) && throw(ErrorException("unknown package $package."))
    return filename
end

function get_package_file(package::Module)
    filename = pathof(package)
    isnothing(filename) && throw(ErrorException("cannot analyze a module defined in the REPL."))
    return filename
end

function get_package_file(::Nothing)
    project = Pkg.project()
    project.ispackage || throw(ErrorException("active project at $(project.path) is not a package."))
    return normpath(dirname(project.path), "src", project.name::String * ".jl")
end

"""
    report_text(text::AbstractString,
                filename::AbstractString = "top-level";
                jetconfigs...) -> JETToplevelResult

Analyzes `text` and returns [`JETToplevelResult`](@ref).
"""
function report_text(text::AbstractString,
                     filename::AbstractString = "top-level";
                     analyzer::Type{Analyzer} = JETAnalyzer,
                     source::Union{Nothing,AbstractString} = nothing,
                     jetconfigs...) where {Analyzer<:AbstractAnalyzer}
    analyzer′ = Analyzer(; jetconfigs...)
    maybe_initialize_caches!(analyzer′)
    config = ToplevelConfig(; jetconfigs...)
    res = virtual_process(text,
                          filename,
                          analyzer′,
                          config,
                          )
    if isnothing(source)
        source = string(nameof(var"#self#"), "(..., \"$filename\")")
    end
    return JETToplevelResult(analyzer′, res, source; analyzer, jetconfigs...)
end

"""
Configurations for "watch" mode.
The configurations will only be active when used with [`report_and_watch_file`](@ref).

---
- `revise_all::Bool = true` \\
  Redirected to [`Revise.entr`](https://timholy.github.io/Revise.jl/stable/user_reference/#Revise.entr)'s `all` keyword argument.
  When set to `true`, JET will retrigger analysis as soon as code updates are detected in
  any module tracked by Revise.
  Currently when encountering `import/using` statements, JET won't perform analysis, but
  rather will just load the modules as usual execution (this also means Revise will track
  those modules).
  So if you're editing both files analyzed by JET and modules that are used within the files,
  this configuration should be enabled.
---
- `revise_modules = nothing` \\
  Redirected to [`Revise.entr`](https://timholy.github.io/Revise.jl/stable/user_reference/#Revise.entr)'s `modules` positional argument.
  If a iterator of `Module` is given, JET will retrigger analysis whenever code in `modules` updates.

  !!! tip
      This configuration is useful when your're also editing files that are not tracked by Revise,
      e.g. editing functions defined in `Base`:
      ```julia-repl
      # re-performe analysis when you make a change to `Base`
      julia> report_and_watch_file(yourfile; revise_modules = [Base])
      ```
---
"""
struct WatchConfig
    # Revise configurations
    revise_all::Bool
    revise_modules
    @jetconfigurable WatchConfig(; revise_all::Bool = true,
                                   revise_modules   = nothing,
                                   ) =
        return new(revise_all,
                   revise_modules,
                   )
end

"""
    report_and_watch_file(filename::AbstractString;
                          jetconfigs...)

Watches `filename` and keeps re-triggering analysis with [`report_file`](@ref) on code update.
JET will try to analyze all the `include`d files reachable from `filename`, and it will
re-trigger analysis if there is code update detected in any of the `include`d files.

This function internally uses [Revise.jl](https://timholy.github.io/Revise.jl/stable/) to
track code updates. Revise also offers possibilities to track changes in files that are
not directly analyzed by JET, or even changes in `Base` files.
See [watch configurations](@ref watch-config) for more details.

See also: [`report_file`](@ref)
"""
function report_and_watch_file(args...; kwargs...)
    if @isdefined(Revise)
        _report_and_watch_file(args...; kwargs...)
    else
        init_revise!()
        @invokelatest _report_and_watch_file(args...; kwargs...)
    end
end

# HACK to avoid Revise loading overhead when just using `@report_call`, etc.
init_revise!() = @eval (@__MODULE__) using Revise

function _report_and_watch_file(filename::AbstractString; jetconfigs...)
    local included_files::Set{String}

    config = WatchConfig(; jetconfigs...)

    included_files = let res = report_file(filename; jetconfigs...)
        display(res)
        res.res.included_files
    end

    interrupted = false
    while !interrupted
        try
            Revise.entr(collect(included_files), config.revise_modules;
                        postpone = true, all = config.revise_all) do
                next_included_files = let res = report_file(filename; jetconfigs...)
                    display(res)
                    res.res.included_files
                end
                if any(∉(included_files), next_included_files)
                    # refresh watch files
                    throw(InsufficientWatches(next_included_files))
                end
                return nothing
            end
            interrupted = true # `InterruptException` was gracefully handled within `entr`, shutdown watch mode
        catch err
            # handle "expected" errors, keep running

            if isa(err, InsufficientWatches)
                included_files = err.included_files
                continue
            elseif isa(err, LoadError) ||
                   (isa(err, ErrorException) && startswith(err.msg, "lowering returned an error")) ||
                   isa(err, Revise.ReviseEvalException)
                continue

            # async errors
            elseif isa(err, CompositeException)
                errs = err.exceptions
                i = findfirst(e->isa(e, TaskFailedException), errs)
                if !isnothing(i)
                    tfe = errs[i]::TaskFailedException
                    let res = tfe.task.result
                        if isa(res, InsufficientWatches)
                            included_files = res.included_files
                            continue
                        elseif isa(res, LoadError) ||
                               (isa(res, ErrorException) && startswith(res.msg, "lowering returned an error")) ||
                               isa(res, Revise.ReviseEvalException)
                            continue
                        end
                    end
                end
            end

            # fatal uncaught error happened in Revise.jl
            rethrow(err)
        end
    end
end

struct InsufficientWatches <: Exception
    included_files::Set{String}
end

# we have to go on hacks; see `transform_abstract_global_symbols!` and `resolve_toplevel_symbols!`
function analyze_toplevel!(analyzer::AbstractAnalyzer, src::CodeInfo;
                           set_entry::Bool = true,
                           )
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{}

    mi.def = mod = get_toplevelmod(analyzer)
    src = transform_abstract_global_symbols!(analyzer, src)
    src = resolve_toplevel_symbols!(mod, src)
    mi.uninferred = src

    result = InferenceResult(mi);
    set_result!(result) # modify `result::InferenceResult` for succeeding JET analysis
    # toplevel frames don't really need to be cached, but still better to be optimized
    # in order to get reasonable `LocalUndefVarErrorReport` and `UncaughtExceptionReport`
    # NOTE and also, otherwise `typeinf_edge` won't add "toplevel-to-callee" edges
    frame = (@static IS_AFTER_42082 ?
             InferenceState(result, src, #=cache=# :global, analyzer) :
             InferenceState(result, src, #=cached=# true, analyzer))::InferenceState

    set_entry && set_entry!(analyzer, mi)
    return analyze_frame!(analyzer, frame)
end

# HACK this is very naive hack to re-use `AbstractInterpreter`'s slot type approximation for
# assignments of abstract global variables, which are represented as toplevel symbols at this point;
# the idea is just to transform them into slot from symbol and use their approximated type
# on their assignment (see `finish(::InferenceState, ::AbstractAnalyzer)`).
# NOTE that `transform_abstract_global_symbols!` will produce really invalid code for
# actual interpretation or execution, but all the statements won't be interpreted anymore
# by `ConcreteInterpreter` nor executed by the native compilation pipeline anyway
function transform_abstract_global_symbols!(analyzer::AbstractAnalyzer, src::CodeInfo)
    nslots = length(src.slotnames)
    abstrct_global_variables = Dict{Symbol,Int}()
    concretized = get_concretized(analyzer)

    # linear scan, and find assignments of abstract global variables
    for (i, stmt) in enumerate(src.code::Vector{Any})
        if !(concretized[i])
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, Symbol)
                    if !haskey(abstrct_global_variables, lhs)
                        nslots += 1
                        push!(abstrct_global_variables, lhs => nslots)
                    end
                end
            end
        end
    end

    prewalk_and_transform!(src) do x, scope
        if isa(x, Symbol)
            slot = get(abstrct_global_variables, x, nothing)
            isnothing(slot) || return SlotNumber(slot)
        end
        return x
    end

    resize!(src.slotnames, nslots)
    resize!(src.slotflags, nslots)
    for (slotname, idx) in abstrct_global_variables
        src.slotnames[idx] = slotname
    end

    set_global_slots!(analyzer, Dict(idx => slotname for (slotname, idx) in abstrct_global_variables))

    return src
end

# resolve toplevel symbols (and other expressions like `:foreigncall`)
# so that the returned `CodeInfo` is eligible for abstractintepret and optimization
# TODO `jl_resolve_globals_in_ir` can throw, and we want to bypass it to `ToplevelErrorReport`
@static if VERSION ≥ v"1.8.0-DEV.421"
    function resolve_toplevel_symbols!(mod::Module, src::CodeInfo)
        newsrc = copy(src)
        @ccall jl_resolve_globals_in_ir(
            #=jl_array_t *stmts=# newsrc.code::Any,
            #=jl_module_t *m=# mod::Any,
            #=jl_svec_t *sparam_vals=# svec()::Any,
            #=int binding_effects=# 0::Int)::Cvoid
        return newsrc
    end
else
    # HACK before https://github.com/JuliaLang/julia/pull/42013, we need to go through
    # the method definition pipeline to get the effect of `jl_resolve_globals_in_ir`
    function resolve_toplevel_symbols!(mod::Module, src::CodeInfo)
        sig = svec(
            #=atypes=# svec(typeof(__toplevelf__)),
            #=tvars=# svec(),
            #=functionloc=# LineNumberNode(@__LINE__, @__FILE__))
        # branching on https://github.com/JuliaLang/julia/pull/41137
        method = (@static if isdefined(Core.Compiler, :OverlayMethodTable)
            ccall(:jl_method_def, Any, (Any, Ptr{Cvoid}, Any, Any), sig, C_NULL, src, mod)
        else
            ccall(:jl_method_def, Cvoid, (Any, Any, Any), sig, src, mod)
            only(methods(__toplevelf__))
        end)::Method
        return CC.uncompressed_ir(method)
    end
    function __toplevelf__ end
end

# interactive
# -----------

# TODO improve inferrability by making `analyzer` argument positional

"""
    @report_call [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`report_call`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](@ref) can be given as the optional
arguments like this:
```julia-repl
# reports `rand(::Type{Bool})` with `aggressive_constant_propagation` configuration turned off
julia> @report_call aggressive_constant_propagation=false rand(Bool)
```
"""
macro report_call(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_call, ex0)
end

"""
    report_call(f, types = Tuple{};
                analyzer::Type{<:AbstractAnalyzer} = JETAnalyzer,
                jetconfigs...) -> JETCallResult
    report_call(tt::Type{<:Tuple};
                analyzer::Type{<:AbstractAnalyzer} = JETAnalyzer,
                jetconfigs...) -> JETCallResult

Analyzes the generic function call with the given type signature with `analyzer`.
And finally returns the analysis result as [`JETCallResult`](@ref).
"""
function report_call(@nospecialize(f), @nospecialize(types = Tuple{}); jetconfigs...)
    ft = Core.Typeof(f)
    if isa(types, Type)
        u = unwrap_unionall(types)
        tt = rewrap_unionall(Tuple{ft, u.parameters...}, types)
    else
        tt = Tuple{ft, types...}
    end
    return report_call(tt; jetconfigs...)
end

function report_call(@nospecialize(tt::Type{<:Tuple});
                     analyzer::Type{Analyzer} = JETAnalyzer,
                     source::Union{Nothing,AbstractString} = nothing,
                     jetconfigs...) where {Analyzer<:AbstractAnalyzer}
    analyzer = Analyzer(; jetconfigs...)
    maybe_initialize_caches!(analyzer)
    analyzer, result = analyze_gf_by_type!(analyzer, tt)

    if isnothing(source)
        source = string(nameof(var"@report_call"), " ", sprint(show_tuple_as_call, Symbol(""), tt))
    end

    return JETCallResult(result, analyzer, source; jetconfigs...)
end

# Test.jl integration
# -------------------

"""
    @test_call [jetconfigs...] [broken=false] [skip=false] f(args...)

Runs [`@report_call jetconfigs... f(args...)`](@ref @report_call) and tests that the generic
function call `f(args...)` is free from problems that `@report_call` can detect.
If executed inside `@testset`, returns a `Pass` result if it is, a `Fail` result if it
contains any error points detected, or an `Error` result if this macro encounters an
unexpected error. When the test `Fail`s, abstract call stack to each problem location will
also be printed to `stdout`.

```julia-repl
julia> @test_call sincos(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call sincos(10)
```

As with [`@report_call`](@ref), any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or analyzer specific configurations can be given as the optional arguments `jetconfigs...` like this:
```julia-repl
julia> cond = false

julia> function f(n)
           # `cond` is untyped, and will be reported by the sound analysis pass,
           # while JET's default analysis pass will ignore it
           if cond
               return sin(n)
           else
               return cos(n)
           end
       end;

julia> @test_call f(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call f(10)

julia> @test_call mode=:sound f(10)
JET-test failed at none:1
  Expression: #= none:1 =# JET.@test_call mode = :sound f(10)
  ═════ 1 possible error found ═════
  ┌ @ none:2 goto %4 if not Main.cond
  │ non-boolean (Any) used in boolean context: goto %4 if not Main.cond
  └──────────

ERROR: There was an error during testing
```

`@test_call` is fully integrated with [`Test` standard library](https://docs.julialang.org/en/v1/stdlib/Test/)'s unit-testing infrastructure.
It means, the result of `@test_call` will be included in the final `@testset` summary,
it supports `skip` and `broken` annotations as like `@test` and its family:
```julia-repl
julia> using JET, Test

# Julia can't propagate the type constraint `ref[]::Number` to `sin(ref[])`, JET will report `NoMethodError`
julia> f(ref) = isa(ref[], Number) ? sin(ref[]) : nothing;

# we can make it type-stable if we extract `ref[]` into a local variable `x`
julia> g(ref) = (x = ref[]; isa(x, Number) ? sin(x) : nothing);

julia> @testset "check errors" begin
           ref = Ref{Union{Nothing,Int}}(0)
           @test_call f(ref)             # fail
           @test_call g(ref)             # fail
           @test_call broken=true f(ref) # annotated as broken, thus still "pass"
       end
check errors: JET-test failed at none:3
  Expression: #= none:3 =# JET.@test_call f(ref)
  ═════ 1 possible error found ═════
  ┌ @ none:1 Main.sin(Base.getindex(ref))
  │ for 1 of union split cases, no matching method found for call signatures (Tuple{typeof(sin), Nothing})): Main.sin(Base.getindex(ref::Base.RefValue{Union{Nothing, Int64}})::Union{Nothing, Int64})
  └──────────

Test Summary: | Pass  Fail  Broken  Total
check errors  |    1     1       1      3
ERROR: Some tests did not pass: 1 passed, 1 failed, 0 errored, 1 broken.
```
"""
macro test_call(ex0...)
    ex0 = collect(ex0)

    local broken = nothing
    local skip   = nothing
    idx = Int[]
    for (i,x) in enumerate(ex0)
        if iskwarg(x)
            key, val = x.args
            if key === :broken
                if !isnothing(broken)
                    error("invalid test macro call: cannot set `broken` keyword multiple times")
                end
                broken = esc(val)
                push!(idx, i)
            elseif key === :skip
                if !isnothing(skip)
                    error("invalid test macro call: cannot set `skip` keyword multiple times")
                end
                skip = esc(val)
                push!(idx, i)
            end
        end
    end
    if !isnothing(broken) && !isnothing(skip)
        error("invalid test macro call: cannot set both `skip` and `broken` keywords")
    end
    deleteat!(ex0, idx)

    testres, orig_expr = test_exs(ex0, __module__, __source__)

    return quote
        if $(!isnothing(skip) && skip)
            $(Test.record)($get_testset(), $Broken(:skipped, $orig_expr))
        else
            testres = $testres
            if $(!isnothing(broken) && broken)
                if isa(testres, $JETTestFailure)
                    testres = $Broken(:test_call, $orig_expr)
                elseif isa(testres, $Pass)
                    testres = $Error(:test_unbroken, $orig_expr, nothing, nothing, $(QuoteNode(__source__)))
                end
            else
                isa(testres, $Pass) || ccall(:jl_breakpoint, $Cvoid, ($Any,), testres)
            end
            $(Test.record)($get_testset(), testres)
        end
    end
end

get_exceptions() = @static if isdefined(Base, :current_exceptions)
    Base.current_exceptions()
else
    Base.catch_stack()
end
@static if !hasfield(Pass, :source)
    Pass(test_type::Symbol, orig_expr, data, thrown, source) = Pass(test_type, orig_expr, data, thrown)
end

function test_exs(ex0, m, source)
    analysis = InteractiveUtils.gen_call_with_extracted_types_and_kwargs(m, :report_call, ex0)
    orig_expr = QuoteNode(
        Expr(:macrocall, GlobalRef(@__MODULE__, Symbol("@test_call")), source, ex0...))
    source = QuoteNode(source)
    testres = :(try
        result = $analysis
        if $length($get_reports(result)) == 0
            $Pass(:test_call, $orig_expr, nothing, nothing, $source)
        else
            $JETTestFailure($orig_expr, $source, result)
        end
    catch err
        isa(err, $InterruptException) && rethrow()
        $Error(:test_error, $orig_expr, err, $get_exceptions(), $source)
    end) |> Base.remove_linenums!
    return testres, orig_expr
end

"""
    test_call(f, types = Tuple{}; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_call(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_call(f, types; jetconfigs...`](@ref report_call) and tests that the generic
function call `f(args...)` is free from problems that `report_call` can detect.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_call`](@ref).
"""
function test_call(@nospecialize(args...);
                   broken::Bool = false, skip::Bool = false,
                   jetconfigs...)
    source = LineNumberNode(@__LINE__, @__FILE__)
    kwargs = map(((k,v),)->Expr(:kw, k, v), collect(jetconfigs))
    orig_expr = :($test_call($(args...); $(kwargs...)))

    if skip
        Test.record(get_testset(), Broken(:skipped, orig_expr))
    else
        testres = try
            result = report_call(args...; jetconfigs...)
            if length(get_reports(result)) == 0
                Pass(:test_call, orig_expr, nothing, nothing, source)
            else
                JETTestFailure(orig_expr, source, result)
            end
        catch err
            isa(err, InterruptException) && rethrow()
            Error(:test_error, orig_expr, err, get_exceptions(), source)
        end

        if broken
            if isa(testres, JETTestFailure)
                testres = Broken(:test_call, orig_expr)
            elseif isa(testres, Pass)
                testres = Error(:test_unbroken, orig_expr, nothing, nothing, source)
            end
        else
            isa(testres, Pass) || ccall(:jl_breakpoint, Cvoid, (Any,), testres)
        end
        Test.record(get_testset(), testres)
    end
end

# NOTE we will just show abstract call strack, and won't show backtrace of actual test executions

struct JETTestFailure <: Result
    orig_expr::Expr
    source::LineNumberNode
    result::JETCallResult
end

const TEST_INDENTS = "  "

function Base.show(io::IO, t::JETTestFailure)
    printstyled(io, "JET-test failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    println(io, TEST_INDENTS, "Expression: ", t.orig_expr)
    # print abstract call stack, with appropriate indents
    _, ctx = Base.unwrapcontext(io)
    buf = IOBuffer()
    ioctx = IOContext(buf, ctx)
    show(ioctx, t.result)
    lines = replace(String(take!(buf)), '\n'=>string('\n',TEST_INDENTS))
    print(io, TEST_INDENTS, lines)
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", t::JETTestFailure) =
    return t

function Test.record(::FallbackTestSet, t::JETTestFailure)
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end

function Test.record(ts::DefaultTestSet, t::JETTestFailure)
    if TESTSET_PRINT_ENABLE[]
        printstyled(ts.description, ": ", color=:white)
        print(t)
        println()
    end
    # HACK convert to `Fail` so that test summarization works correctly
    push!(ts.results, Fail(:test_call, t.orig_expr, nothing, nothing, t.source))
    return t
end

# interface
# =========

"""
    JETInterface

This `baremodule` exports names that form the APIs of [`AbstractAnalyzer` Framework](@ref AbstractAnalyzer-Framework).
`using JET.JETInterface` loads all names that are necessary to define a plugin analysis.
"""
baremodule JETInterface
const DOCUMENTED_NAMES = Symbol[] # will be used in docs/make.jl
end

function reexport_as_api!(xs...)
    for x in xs
        ex = Expr(:block)

        val = Core.eval(@__MODULE__, x)
        canonicalname = Symbol(parentmodule(val), '.', nameof(val))
        canonicalpath = Symbol.(split(string(canonicalname), '.'))

        modpath = Expr(:., canonicalpath[1:end-1]...)
        symname = last(canonicalpath)
        sympath = Expr(:., symname)
        importex = Expr(:import, Expr(:(:), modpath, sympath))
        exportex = Expr(:export, symname)

        push!(ex.args, importex, exportex)
        Core.eval(JETInterface, ex)
        push!(JETInterface.DOCUMENTED_NAMES, symname)
    end
end

reexport_as_api!(JETInterface,
                 AbstractAnalyzer,
                 AnalyzerState,
                 ReportPass,
                 get_cache_key,
                 InferenceErrorReport,
                 aggregation_policy,
                 add_new_report!,
                 get_msg,
                 get_spec_args,
                 var"@reportdef",
                 VSCode.vscode_source,
                 VSCode.vscode_diagnostics_order,
                 )

# builtin analyzers
# =================

include("analyzers/jetanalyzer.jl")
include("analyzers/optanalyzer.jl")

# exports
# =======

export
    # generic entries & default jetanalyzer
    report_file,
    report_and_watch_file,
    report_package,
    report_text,
    @report_call,
    report_call,
    @test_call,
    test_call,
    # optanalyzer
    @report_opt,
    report_opt,
    @test_opt,
    test_opt

end
