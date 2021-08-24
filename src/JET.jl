module JET

let
    README = normpath(dirname(@__DIR__), "README.md")
    include_dependency(README)
    @doc read(README, String) JET
end

# not sure why, but a benchmark showed this is faster
Base.Experimental.@optlevel 1

const CC = Core.Compiler

@static isdefined(CC, :AbstractInterpreter) || throw(ErrorException("JET.jl only works with Julia versions 1.6 and higher"))

# imports
# =======

# `AbstractAnalyzer`
import .CC:
    # abstractinterpreterinterface.jl
    InferenceParams,
    OptimizationParams,
    get_world_counter,
    lock_mi_inference,
    unlock_mi_inference,
    add_remark!,
    may_optimize,
    may_compress,
    may_discard_trees,
    # jetcache.jl
    code_cache,
    # haskey,
    # get,
    # getindex,
    # setindex!,
    get_inference_cache,
    cache_lookup,
    # push!,
    # tfuncs.jl
    builtin_tfunction,
    return_type_tfunc,
    # abstractinterpretation.jl
    abstract_call_gf_by_type,
    abstract_call_method_with_const_args,
    abstract_call_method,
    abstract_eval_special_value,
    abstract_eval_value,
    abstract_eval_statement,
    # typeinfer.jl
    typeinf,
    _typeinf,
    finish,
    typeinf_edge,
    # JET.jl
    InferenceState

# `ConcreteInterpreter`
import JuliaInterpreter:
    # virtualprocess.jl
    step_expr!,
    evaluate_call_recurse!,
    handle_err

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
    NOT_FOUND,
    MethodMatchInfo,
    UnionSplitInfo,
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
    argtype_by_index,
    argtype_tail,
    _methods_by_ftype,
    specialize_method,
    add_backedge!,
    add_mt_backedge!,
    compute_basic_blocks,
    matching_cache_argtypes,
    is_argtype_match,
    tuple_tfunc,
    may_invoke_generator,
    inlining_enabled

import Base:
    parse_input_line,
    unwrap_unionall,
    rewrap_unionall,
    uniontypes,
    IdSet

import Base.Meta:
    _parse_string,
    lower

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
    finish!,
    is_return,
    is_quotenode_egal,
    moduleof,
    @lookup

import MacroTools: MacroTools, @capture, normalise, striplines

using InteractiveUtils

using Pkg, Pkg.TOML

# common
# ======

const JET_DEV_MODE = parse(Bool, get(ENV, "JET_DEV_MODE", "false"))

const CONFIG_FILE_NAME = ".JET.toml"

const State     = Union{InferenceState,OptimizationState}
const StateAtPC = Tuple{State,Int}

# hooks
# -----

const INIT_HOOKS = Function[]
push_inithook!(f) = push!(INIT_HOOKS, f)
__init__() = foreach(@nospecialize(f)->f(), INIT_HOOKS)

# compat
# ------

const BACKEDGE_CALLBACK_ENABLED = :callbacks in fieldnames(Core.MethodInstance)

@static BACKEDGE_CALLBACK_ENABLED || push_inithook!() do
@warn """with your Julia version, JET.jl may not be able to update analysis result
correctly after refinement of a method in deeper call sites
"""
end

@static if isdefined(CC, :LimitedAccuracy)
    import .CC: ignorelimited
else
    ignorelimited(@nospecialize(x)) = x
end

@static if isdefined(Base, Symbol("@aggressive_constprop"))
    import Base: @aggressive_constprop
else
    macro aggressive_constprop(x) esc(x) end # not available
end

# early take in https://github.com/JuliaLang/julia/pull/41040
function gen_call_with_extracted_types_and_kwargs(__module__, fcn, ex0)
    kws = Expr[]
    arg = ex0[end] # Mandatory argument
    for i in 1:length(ex0)-1
        x = ex0[i]
        if x isa Expr && x.head === :(=) # Keyword given of the form "foo=bar"
            if length(x.args) != 2
                return Expr(:call, :error, "Invalid keyword argument: $x")
            end
            push!(kws, Expr(:kw, esc(x.args[1]), esc(x.args[2])))
        else
            return Expr(:call, :error, "@$fcn expects only one non-keyword argument")
        end
    end
    return InteractiveUtils.gen_call_with_extracted_types(__module__, fcn, arg, kws)
end

# macros
# ------

# to circumvent https://github.com/JuliaLang/julia/issues/37342, we inline these `isa`
# condition checks at surface AST level
macro isexpr(ex, args...)
    ex   = esc(ex)
    args = map(esc, args)
    return :($(GlobalRef(Core, :isa))($(ex), $(GlobalRef(Core, :Expr))) && $(_isexpr_check)($(ex), $(args...)))
end

_isexpr_check(ex::Expr, head::Symbol)         = ex.head === head
_isexpr_check(ex::Expr, heads)                = in(ex.head, heads)
_isexpr_check(ex::Expr, head::Symbol, n::Int) = ex.head === head && length(ex.args) == n
_isexpr_check(ex::Expr, heads, n::Int)        = in(ex.head, heads) && length(ex.args) == n

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

iskwarg(@nospecialize(x)) = @isexpr(x, :(=))

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
  functions overloaded against `AbstractAnalyzer` can be called from the native method of `f`;
e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
@invoke abstract_call_gf_by_type(analyzer::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                 max_methods::Int)
```
"""
macro invoke(ex)
    f, args, kwargs = destructure_callex(ex)
    arg2typs = map(args) do x
        if @isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize")
            x = last(x.args)
        end
        @isexpr(x, :(::)) ? (x.args...,) : (x, GlobalRef(Core, :Any))
    end
    args, argtypes = first.(arg2typs), last.(arg2typs)
    return esc(:($(GlobalRef(Core, :invoke))($(f), Tuple{$(argtypes...)}, $(args...); $(kwargs...))))
end

"""
    @invokelatest f(args...; kwargs...)

Provides a convenient way to call [`Base.invokelatest`](https://docs.julialang.org/en/v1/base/base/#Base.invokelatest).
`@invokelatest f(args...; kwargs...)` will simply be expanded into
`Base.invokelatest(f, args...; kwargs...)`.
"""
macro invokelatest(ex)
    f, args, kwargs = destructure_callex(ex)
    return esc(:($(GlobalRef(Base, :invokelatest))($(f), $(args...); $(kwargs...))))
end

function destructure_callex(ex)
    @assert @isexpr(ex, :call) "call expression f(args...; kwargs...) should be given"

    f = first(ex.args)
    args = []
    kwargs = []
    for x in ex.args[2:end]
        if @isexpr(x, :parameters)
            append!(kwargs, x.args)
        elseif @isexpr(x, :kw)
            push!(kwargs, x)
        else
            push!(args, x)
        end
    end

    return f, args, kwargs
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

See also: [`EGAL_TYPES`](@ref)
"""
macro withmixedhash(typedef)
    @assert @isexpr(typedef, :struct) "struct definition should be given"
    name = esc(typedef.args[2])
    fld2typs = map(filter(!islnn, typedef.args[3].args)) do x
        if @isexpr(x, :(::))
            fld, typex = x.args
            typ = Core.eval(__module__, typex)
            fld, typ
        else
            (x, Any)
        end
    end
    @assert !isempty(fld2typs) "no fields given, nothing to hash"

    h_init = UInt === UInt64 ? rand(UInt64) : rand(UInt32)
    hash_body = quote h = $(h_init) end
    for (fld, typ) in fld2typs
        push!(hash_body.args, :(h = $(Base.hash)(x.$fld, h)))
    end
    push!(hash_body.args, :(return h))
    hash_func = :(function Base.hash(x::$name, h::$UInt); $(hash_body); end)
    eq_body = foldr(fld2typs; init = true) do (fld, typ), x
        eq_ex = if all(in(EGAL_TYPES), typenames(typ))
            :(x1.$fld === x2.$fld)
        else
            :(x1.$fld == x2.$fld)
        end
        Expr(:&&, eq_ex, x)
    end
    eq_func = :(function Base.:(==)(x1::$name, x2::$name); $(eq_body); end)

    return quote
        Base.@__doc__ $(typedef)
        $(hash_func)
        $(eq_func)
    end
end

"""
    const EGAL_TYPES = $(EGAL_TYPES)

Keeps names of types that should be compared by `===` rather than `==`.

See also: [`@withmixedhash`](@ref)
"""
const EGAL_TYPES = Set{Symbol}((:Type, :Symbol, :MethodInstance))

typenames(@nospecialize(t::DataType)) = [t.name.name]
typenames(u::Union)                   = collect(Base.Iterators.flatten(typenames.(CC.uniontypes(u))))
typenames(u::UnionAll)                = [u.body.name.name]

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
    @assert @isexpr(funcdef, :(=)) || @isexpr(funcdef, :function) "function definition should be given"

    defsig = funcdef.args[1]
    if @isexpr(defsig, :where)
        defsig = first(defsig.args)
    end
    args = defsig.args::Vector{Any}
    thisname = first(args)
    i = findfirst(a->@isexpr(a, :parameters), args)
    if isnothing(i)
        @warn "no JET configurations are defined for `$thisname`"
        insert!(args, 2, Expr(:parameters, :(jetconfigs...)))
    else
        kwargs = args[i]
        found = false
        for kwarg in kwargs.args
            if @isexpr(kwarg, :...)
                found = true
                continue
            end
            kwargex = first(kwarg.args)
            kwargname = (@isexpr(kwargex, :(::)) ? first(kwargex.args) : kwargex)::Symbol
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

# common

@inline get_stmt((sv, pc)::StateAtPC)         = @inbounds sv.src.code[pc]
@inline get_lin((sv, pc)::StateAtPC)          = @inbounds (sv.src.linetable::Vector)[sv.src.codelocs[pc]]::LineInfoNode
@inline get_ssavaluetype((sv, pc)::StateAtPC) = @inbounds sv.src.ssavaluetypes[pc]

@inline get_slottype(s::Union{StateAtPC,State}, slot)      = get_slottype(s, slot_id(slot))
@inline get_slottype((sv, pc)::StateAtPC,       slot::Int) = get_slottype(sv, slot)
@inline get_slottype(sv::State,                 slot::Int) = @inbounds sv.slottypes[slot]

@inline get_slotname(s::Union{StateAtPC,State}, slot)      = get_slotname(s, slot_id(slot))
@inline get_slotname((sv, pc)::StateAtPC,       slot::Int) = @inbounds sv.src.slotnames[slot]
@inline get_slotname(sv::State,                 slot::Int) = @inbounds sv.src.slotnames[slot]

# InfernceState

# we can retrieve program-counter-level slottype during inference
@inline get_slottype(s::Tuple{InferenceState,Int}, slot::Int) = @inbounds (get_states(s)[slot]::VarState).typ
@inline get_states((sv, pc)::Tuple{InferenceState,Int})       = @inbounds sv.stmt_types[pc]::VarTable

@inline get_currpc(sv::InferenceState)  = min(sv.currpc, length(sv.src.code))
@inline get_result(sv::InferenceState)  = sv.bestguess

function is_constant_propagated(frame::InferenceState)
    return !frame.cached && CC.any(frame.result.overridden_by_const)
end

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

# lattice

ignorenotfound(@nospecialize(t)) = t === NOT_FOUND ? Bottom : t

# includes
# ========

# interface
include("interfaces.jl")
# abstract interpretaion based analysis
include("tfuncs.jl")
include("abstractinterpretation.jl")
include("typeinfer.jl")
include("analyzer.jl")
include("jetcache.jl")
include("locinfo.jl")
# top-level analysis
include("graph.jl")
include("virtualprocess.jl")
# watch mode
include("watch.jl")

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
JETToplevelResult(analyzer, res, source; jetconfigs...) =
    return JETToplevelResult(analyzer, res, source, jetconfigs)
@eval Base.iterate(res::JETToplevelResult, state=1) =
    return state > $(fieldcount(JETToplevelResult)) ? nothing : (getfield(res, state), state+1)

"""
    res::JETCallResult

Represents the result of JET's analysis on a function call.
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.type`: the return type of the function call
- `res.source::String`: the identity key of this analysis
- `res.jetconfigs`: [JET configurations](@ref JET-configurations) used for this analysis

`JETCallResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically choosen and render the analysis result.
"""
struct JETCallResult{Analyzer<:AbstractAnalyzer,JETConfigs}
    analyzer::Analyzer
    type
    source::String
    jetconfigs::JETConfigs
    JETCallResult(analyzer::Analyzer, @nospecialize(type), source;
                  jetconfigs...) where {Analyzer<:AbstractAnalyzer} =
        new{Analyzer,typeof(jetconfigs)}(analyzer, type, source, jetconfigs)
end
@eval Base.iterate(res::JETCallResult, state=1) =
    return state > $(fieldcount(JETCallResult)) ? nothing : (getfield(res, state), state+1)

function get_critical_reports(res::VirtualProcessResult)
    # non-empty `ret.toplevel_error_reports` means critical errors happened during
    # the AST transformation, so they always have precedence over `ret.inference_error_reports`
    return !isempty(res.toplevel_error_reports) ? res.toplevel_error_reports :
           res.inference_error_reports
end

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

# UIs
# ===

# default UI (console)
include("print.jl")
include("reasons.jl")

# entry
# =====

"""
    report_file(filename::AbstractString;
                jetconfigs...) -> JETToplevelResult

Analyzes `filename` and returns [`JETToplevelResult`](@ref).

This function will look for `$CONFIG_FILE_NAME` configuration file in the directory of `filename`,
  and search _up_ the file tree until any `$CONFIG_FILE_NAME` is (or isn't) found.
When found, the configurations specified in the file will be applied.
See [Configuration File](@ref) for more details.

!!! tip
    When you want to analyze your package, but any file actually using it isn't available, the
      `analyze_from_definitions` option can be useful (see [`ToplevelConfig`](@ref)'s `analyze_from_definitions` option). \\
    For example, JET can analyze JET itself like below:
    ```julia
    # from the root directory of JET.jl
    julia> report_file("src/JET";
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
    See [Logging Configurations](@ref) for more details.
"""
function report_file(filename::AbstractString;
                     source::Union{Nothing,AbstractString} = nothing,
                     jetconfigs...) where T
    isfile(filename) || throw(ArgumentError("$filename doesn't exist"))

    configfile = find_config_file(dirname(abspath(filename)))
    if isnothing(configfile)
        jetconfigs = set_toplevel_logger_if_missing(jetconfigs)
    else
        config = parse_config_file(configfile)
        jetconfigs = overwrite_options(config, jetconfigs)
        jetconfigs = set_toplevel_logger_if_missing(jetconfigs)
        with_logger(get(jetconfigs, :toplevel_logger, nothing), ≥(INFO_LOGGER_LEVEL), "toplevel") do io
            println(io, "applied JET configurations in $configfile")
        end
    end

    if isnothing(source)
        source = string(nameof(var"#self#"), "(\"$filename\")")
    end

    return report_text(read(filename, String), filename; source, jetconfigs...)
end

function set_toplevel_logger_if_missing(@nospecialize(jetconfigs))
    haskey(jetconfigs, :toplevel_logger) && return jetconfigs
    default_toplevel_logger_config =
        kwargs((; toplevel_logger = IOContext(stdout::IO, LOGGER_LEVEL_KEY => DEFAULT_LOGGER_LEVEL)))
    return overwrite_options(jetconfigs, default_toplevel_logger_config)
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

When [`$report_file`](@ref) or [`$report_and_watch_file`](@ref) is called, it will look for
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
    @isexpr(ret, :incomplete) && error(first(ret.args))
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
`package` can be either a module or a string. In the latter case it must be the name of a
package in your current environment.

This function configures analysis with the following configurations:
- `analyze_from_definitions = true`: allows JET to enter analysis without top-level call sites;
  this is useful for package analysis since a package itself usually has only definitions
  but not usages (i.e. call sites)
- `concretization_patterns = [:(x_ = y_), :(const x_ = y_)]`: concretizes every global variable instantiations;
  concretizations are generally preferred for successful analysis as far as they're cheap,
  and global variable instantiations that occur in a package definition are _usually_ very cheap
See [`ToplevelConfig`](@ref) for more details.

---

    report_package([io::IO = stdout];
                   jetconfigs...) -> res::ReportResult

Like above but analyzes the package of the current project.

See also: [`report_file`](@ref)
"""
function report_package(package::Union{AbstractString,Module,Nothing} = nothing;
                        analyze_from_definitions::Bool = true,
                        concretization_patterns = [:(x_ = y_), :(const x_ = y_)],
                        jetconfigs...)
    filename = get_package_file(package)
    return report_file(filename; analyze_from_definitions, jetconfigs...)
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

function analyze_toplevel!(analyzer::AbstractAnalyzer, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.uninferred = src
    mi.specTypes = Tuple{}

    transform_abstract_global_symbols!(analyzer, src)
    mi.def = get_toplevelmod(analyzer)

    result = InferenceResult(mi);
    # toplevel frame doesn't need to be cached (and so it won't be optimized), nor should
    # go through JET's code generation error check
    frame = InferenceState(result, src, #=cached=# false, analyzer);

    return analyze_frame!(analyzer, frame)
end

# HACK this is an native hack to re-use `AbstractInterpreter`'s approximated slot types for
# assignments of abstract global variables, which are represented as toplevel symbols at this point;
# the idea is just to transform them into slots from symbols and use their approximated type
# on their assignment.
# NOTE that `transform_abstract_global_symbols!` will produce really invalid code for
# actual interpretation or execution, but all the statements won't be interpreted anymore
# by `ConcreteInterpreter` nor executed anyway since toplevel frames aren't cached
function transform_abstract_global_symbols!(analyzer::AbstractAnalyzer, src::CodeInfo)
    nslots = length(src.slotnames)
    abstrct_global_variables = Dict{Symbol,Int}()
    concretized = get_concretized(analyzer)

    # linear scan, and find assignments of abstract global variables
    for (i, stmt) in enumerate(src.code::Vector{Any})
        if !(concretized[i])
            if @isexpr(stmt, :(=))
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
end

# TODO `analyze_builtin!` ?
function analyze_gf_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}))
    mm = get_single_method_match(tt, InferenceParams(analyzer).MAX_METHODS, get_world_counter(analyzer))
    return analyze_method_signature!(analyzer, mm.method, mm.spec_types, mm.sparams)
end

function get_single_method_match(@nospecialize(tt), lim, world)
    mms = _methods_by_ftype(tt, lim, world)
    @assert !isa(mms, Bool) "unable to find matching method for $(tt)"

    filter!(mm::MethodMatch->mm.spec_types===tt, mms)
    @assert length(mms) == 1 "unable to find single target method for $(tt)"

    return first(mms)::MethodMatch
end

analyze_method!(analyzer::AbstractAnalyzer, m::Method) =
    analyze_method_signature!(analyzer, m, m.sig, method_sparams(m))

function method_sparams(m::Method)
    s = TypeVar[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(s, sig.var)
        sig = sig.body
    end
    return svec(s...)
end

analyze_method_signature!(analyzer::AbstractAnalyzer, m::Method, @nospecialize(atype), sparams::SimpleVector) =
    analyze_method_instance!(analyzer, specialize_method(m, atype, sparams))

function analyze_method_instance!(analyzer::AbstractAnalyzer, mi::MethodInstance)
    result = InferenceResult(mi)

    frame = InferenceState(result, #=cached=# true, analyzer)
    isnothing(frame) && return analyzer, nothing

    return analyze_frame!(analyzer, frame)
end

function InferenceState(result::InferenceResult, cached::Bool, analyzer::AbstractAnalyzer)
    ReportPass(analyzer)(GeneratorErrorReport, analyzer, result.linfo)
    return @invoke InferenceState(result::InferenceResult, cached::Bool, analyzer::AbstractInterpreter)
end

@reportdef struct GeneratorErrorReport <: InferenceErrorReport
    @nospecialize(err) # actual error wrapped
end
get_msg(::Type{GeneratorErrorReport}, analyzer::AbstractAnalyzer, linfo::MethodInstance, @nospecialize(err)) =
    return sprint(showerror, err)

# XXX what's the "soundness" of a `@generated` function ?
# adapated from https://github.com/JuliaLang/julia/blob/f806df603489cfca558f6284d52a38f523b81881/base/compiler/utilities.jl#L107-L137
function (::SoundBasicPass)(::Type{GeneratorErrorReport}, analyzer::AbstractAnalyzer, mi::MethodInstance)
    m = mi.def::Method
    if isdefined(m, :generator)
        # analyze_method_instance!(analyzer, linfo) XXX doesn't work
        may_invoke_generator(mi) || return
        try
            ccall(:jl_code_for_staged, Any, (Any,), mi)
        catch err
            # if user code throws error, wrap and report it
            add_new_report!(GeneratorErrorReport(analyzer, mi, err), analyzer)
        end
    end
end

function analyze_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    typeinf(analyzer, frame)

    # report `throw` calls "appropriately";
    # if the final return type here is `Bottom`-annotated, it _may_ mean the control flow
    # didn't catch some of the `UncaughtExceptionReport`s stashed within `uncaught_exceptions(analyzer)`,
    if get_result(frame) === Bottom
        if !isempty(get_uncaught_exceptions(analyzer))
            append!(get_reports(analyzer), get_uncaught_exceptions(analyzer))
        end
    end

    return analyzer, frame
end

# interactive
# ===========

# TODO improve inferrability by making `analyzer` argument positional

"""
    @report_call [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
  [`report_call`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](@ref) can be given as the optional
  arguments like this:
```julia
# reports `rand(::Type{Bool})` with `aggressive_constant_propagation` configuration turned off
julia> @report_call aggressive_constant_propagation=false rand(Bool)
```
"""
macro report_call(ex0...)
    return gen_call_with_extracted_types_and_kwargs(__module__, :report_call, ex0)
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
    analyzer, frame = analyze_gf_by_type!(analyzer, tt)

    if isnothing(frame)
        # if there is `GeneratorErrorReport`, it means the code generation happened and failed
        rt = any(r->isa(r, GeneratorErrorReport), get_reports(analyzer)) ? Bottom : Any
    else
        rt = get_result(frame)
    end

    if isnothing(source)
        source = string(nameof(var"@report_call"), " ", sprint(show_tuple_as_call, Symbol(""), tt))
    end

    return JETCallResult(analyzer, rt, source; jetconfigs...)
end

# Test.jl integration
# ===================

include("test.jl")

# exports
# =======

"""
    JETInterfaces

This `baremodule` exports names that form the APIs of [JET.jl Pluggable Analysis Framework](@ref).
If you are going to define a plug-in analysis, `using JET.JETInterfaces` will load most useful
names described below.
"""
baremodule JETInterfaces
const DOCUMENTED_NAMES = Symbol[] # will be used in docs/make.jl
end

function reexport_as_api!(xs...; documented = true)
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
        Core.eval(JETInterfaces, ex)
        documented && push!(JETInterfaces.DOCUMENTED_NAMES, symname)
    end
end

reexport_as_api!(AbstractAnalyzer,
                 AnalyzerState,
                 ReportPass,
                 InferenceErrorReport,
                 get_msg,
                 get_spec_args,
                 var"@reportdef",
                 add_new_report!,
                 )
reexport_as_api!(subtypes(InferenceErrorReport)...; documented = false)
reexport_as_api!(subtypes(ReportPass)...; documented = false)

# export entries
export
    report_file,
    report_and_watch_file,
    report_package,
    report_text,
    @report_call,
    report_call,
    @test_call,
    test_call

end
