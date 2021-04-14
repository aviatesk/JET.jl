@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module JET

const CC = Core.Compiler

@static isdefined(CC, :AbstractInterpreter) || throw(ErrorException("JET.jl only works with Julia versions 1.6 and higher"))

# imports
# =======

# `JETInterpreter`
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
    # optimize.jl
    OptimizationState,
    optimize

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
    Intrinsics,
    Builtin,
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
    Slot,
    ReturnNode,
    SimpleVector,
    svec

import .CC:
    AbstractInterpreter,
    NativeInterpreter,
    InferenceState,
    InferenceResult,
    InternalCodeCache,
    WorldRange,
    WorldView,
    Bottom,
    NOT_FOUND,
    MethodMatchInfo,
    UnionSplitInfo,
    MethodLookupResult,
    VarState,
    VarTable,
    slot_id,
    widenconst,
    ⊑,
    CallMeta,
    is_throw_call,
    tmerge,
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
    tuple_tfunc

import Base:
    parse_input_line,
    unwrap_unionall,
    rewrap_unionall,
    Fix1,
    Fix2,
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
    pushall!

import JuliaInterpreter:
    bypass_builtins,
    maybe_evaluate_builtin,
    collect_args,
    is_return,
    is_quotenode_egal,
    @lookup

import MacroTools: @capture

using InteractiveUtils

using Pkg, Pkg.TOML

# common
# ======

const CONFIG_FILE_NAME = ".JET.toml"

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

"""
    @invoke f(arg::T, ...; kwargs...)

Provides a convenient way to call [`invoke`](https://docs.julialang.org/en/v1/base/base/#Core.invoke);
`@invoke f(arg1::T1, arg2::T2; kwargs...)` will be expanded into `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)`.
When an argument's type annotation is omitted, it's specified as `Any` argument, e.g.
`@invoke f(arg1::T, arg2)` will be expanded into `invoke(f, Tuple{T,Any}, arg1, arg2)`.

This could be used to call down to `NativeInterpreter`'s abstract interpretation method of
  `f` while passing `JETInterpreter` so that subsequent calls of abstract interpretation
  functions overloaded against `JETInterpreter` can be called from the native method of `f`;
e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
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
    name = typedef.args[2]
    flddef = typedef.args[3]
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
    hash_func = :(function Base.hash(x::$name, h::UInt); $(hash_body); end)
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
        $(typedef)
        $(hash_func)
        $(eq_func)
    end
end

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

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
            @assert thisname === othername "`$thisname` uses `$kwargname` JET configuration name which is already used by `$othername`"
        end
        found || push!(kwargs.args, :(jetconfigs...))
    end

    return esc(funcdef)
end
const _JET_CONFIGURATIONS = Dict{Symbol,Symbol}()

# utils
# -----

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

# NOTE: these methods are supposed to be called while abstract interpretaion
# and as such aren't valid after finishing it (i.e. optimization, etc.)
@inline get_currpc(frame::InferenceState)                          = min(frame.currpc, length(frame.src.code))
@inline get_stmt(frame::InferenceState, pc = get_currpc(frame))    = @inbounds frame.src.code[pc]
@inline get_states(frame::InferenceState, pc = get_currpc(frame))  = @inbounds frame.stmt_types[pc]::VarTable
@inline get_codeloc(frame::InferenceState, pc = get_currpc(frame)) = @inbounds frame.src.codelocs[pc]
@inline get_lin(frame::InferenceState, loc = get_codeloc(frame))   = @inbounds (frame.src.linetable::Vector)[loc]::LineInfoNode

@inline get_slotname(frame::InferenceState, slot::Int)             = @inbounds frame.src.slotnames[slot]
@inline get_slotname(frame::InferenceState, slot::Slot)            = get_slotname(frame, slot_id(slot))
@inline get_slottype(frame::InferenceState, slot::Int)             = @inbounds (get_states(frame)[slot]::VarState).typ
@inline get_slottype(frame::InferenceState, slot::Slot)            = get_slottype(frame, slot_id(slot))

@inline get_result(frame::InferenceState)                          = frame.bestguess

ignorenotfound(@nospecialize(t)) = t === NOT_FOUND ? Bottom : t

# includes
# ========

include("reports.jl")
include("abstractinterpreterinterface.jl")
include("jetcache.jl")
include("tfuncs.jl")
include("abstractinterpretation.jl")
include("typeinfer.jl")
include("optimize.jl")
include("virtualprocess.jl")
include("watch.jl")
include("print.jl")

# entry
# =====

"""
    res::ReportResult

- `res.included_files::Set{String}`: files analyzed by JET
- `res.any_reported::Bool`: indicates if there was any error point reported
"""
const ReportResult = @NamedTuple begin
    included_files::Set{String}
    any_reported::Bool
end

function report_result(io::IO, res::VirtualProcessResult; jetconfigs...)
    print_result = print_reports(io, res; jetconfigs...)
    return (; included_files = res.included_files,
              any_reported   = print_result)::ReportResult
end

"""
    report_file([io::IO = stdout],
                filename::AbstractString,
                mod::Module = Main;
                jetconfigs...) -> res::ReportResult

Analyzes `filename`, prints the collected error reports to the `io` stream, and finally returns $(@doc ReportResult)

The following optional positional arguments can be specified:
- `mod::Module`: the module context in which the top-level execution will be simulated

This function will look for `$CONFIG_FILE_NAME` configuration file in the directory of `filename`,
  and search _up_ the file tree until any `$CONFIG_FILE_NAME` is (or isn't) found.
When found, the configurations specified in the file will overwrite the given `jetconfigs`.
See [Configuration File](@ref) for more details.

!!! tip
    When you want to analyze your package, but any file using it isn't available, the
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
function report_file(io::IO,
                     filename::AbstractString,
                     mod::Module = Main;
                     # enable top-level info logger by default for entry from file
                     toplevel_logger::Union{Nothing,IO} = IOContext(io, LOGGER_LEVEL_KEY => INFO_LOGGER_LEVEL),
                     jetconfigs...)
    res = analyze_file(filename, mod; toplevel_logger, jetconfigs...)
    return report_result(io, res; jetconfigs...)
end
report_file(args...; jetconfigs...) = report_file(stdout::IO, args...; jetconfigs...)

function analyze_file(filename, args...; jetconfigs...)
    configfile = find_config_file(dirname(abspath(filename)))
    if !isnothing(configfile)
        config = parse_config(configfile)
        jetconfigs = overwrite_options(jetconfigs, config)
        with_logger(get(jetconfigs, :toplevel_logger, nothing), ≥(INFO_LOGGER_LEVEL), "toplevel") do io
            println(io, "applied JET configurations in $configfile")
        end
    end

    text = read(filename, String)
    return analyze_text(text, filename, args...; jetconfigs...)
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
```
aggressive_constant_propagation = false # turn off aggressive constant propagation
... # other configurations
```

Note that the following configurations should be string(s) of valid Julia code:
- `concretization_patterns`: vector of string of Julia code, which can be `parse`d into a
  Julia expression pattern expected by [`MacroTools.@capture` macro](https://fluxml.ai/MacroTools.jl/stable/pattern-matching/).
- `toplevel_logger`: string of Julia code, which can be `parse`d and `eval`uated into `Union{IO,Nothing}`
- `inference_logger`: string of Julia code, which can be `parse`d and `eval`uated into `Union{IO,Nothing}`

E.g. the configurations below are equivalent:
- configurations via keyword arguments
  ```julia
  report_file(somefile;
              concretization_patterns = [:(GLOBAL_CODE_STORE = x_)],
              toplevel_logger = IOContext(open("toplevel.txt", "w"), :JET_LOGGER_LEVEL => 1))
  ```
- configurations via a configuration file
  $(let
      text = read(normpath(@__DIR__, "..", "test", "fixtures", "..JET.toml"), String)
      lines = split(text, '\n')
      pushfirst!(lines, "```toml"); push!(lines, "```")
      join(lines, "\n  ")
  end)
"""
function parse_config(tomlfile)
    config_dict = TOML.parsefile(tomlfile)
    concretization_patterns = get(config_dict, "concretization_patterns", nothing)
    if !isnothing(concretization_patterns)
        @assert isa(concretization_patterns, Vector{String}) "`concretization_patterns` should be array of string of Julia expression"
        config_dict["concretization_patterns"] = Meta.parse.(concretization_patterns)
    end
    toplevel_logger = get(config_dict, "toplevel_logger", nothing)
    if !isnothing(toplevel_logger)
        @assert isa(toplevel_logger, String) "`toplevel_logger` should be string of Julia code"
        config_dict["toplevel_logger"] = Core.eval(@__MODULE__, Meta.parse(toplevel_logger))
    end
    inference_logger = get(config_dict, "inference_logger", nothing)
    if !isnothing(inference_logger)
        @assert isa(inference_logger, String) "`inference_logger` should be string of Julia code"
        config_dict["inference_logger"] = Core.eval(@__MODULE__, Meta.parse(inference_logger))
    end
    return kwargs(config_dict)
end

function kwargs(dict)
    ns = (Symbol.(keys(dict))...,)
    vs = (collect(values(dict))...,)
    return pairs(NamedTuple{ns}(vs))
end

overwrite_options(old, new) = kwargs(merge(old, new))

"""
    report_package([io::IO = stdout],
                   package::Union{AbstractString,Module};
                   jetconfigs...) -> res::ReportResult

Analyzes `package` in the same way as `report_file` with the option
`analyze_from_definitions=true`. See [`report_file`](@ref) for details.
`package` can be either a module or a string. In the latter case it
must be the name of a package in your current environment.

    report_package([io::IO = stdout];
                   jetconfigs...) -> res::ReportResult

Like above but analyzes the package in the current project.

See also: [`report_file`](@ref)
"""
function report_package(io::IO,
                        package::Union{AbstractString,Module,Nothing} = nothing;
                        analyze_from_definitions::Bool = true,
                        toplevel_logger::Union{Nothing,IO} = IOContext(io, LOGGER_LEVEL_KEY => INFO_LOGGER_LEVEL),
                        jetconfigs...)
    res = analyze_package(package; jetconfigs...)
    return report_result(io, res; analyze_from_definitions, toplevel_logger, jetconfigs...)
end
report_package(args...; jetconfigs...) = report_package(stdout::IO, args...; jetconfigs...)

function analyze_package(package, args...; jetconfigs...)
    filename = get_package_file(package)
    return analyze_file(filename; jetconfigs...)
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
    report_text([io::IO = stdout],
                text::AbstractString,
                filename::AbstractString = "top-level",
                mod::Module = Main;
                jetconfigs...) -> res::ReportResult

Analyzes `text`, prints the collected error reports to the `io` stream, and finally returns $(@doc ReportResult)

The following optional positional arguments can be specified:
- `filename`: the file containing `text` (if exists)
- `mod::Module`: the module context in which the top-level execution will be simulated
"""
function report_text(io::IO,
                     text::AbstractString,
                     filename::AbstractString = "top-level",
                     mod::Module = Main;
                     jetconfigs...)
    res = analyze_text(text, filename, mod; jetconfigs...)
    return report_result(io, res; jetconfigs...)
end
report_text(args...; jetconfigs...) = report_text(stdout::IO, args...; jetconfigs...)

function analyze_text(text::AbstractString,
                      filename::AbstractString = "top-level",
                      actualmod::Module = Main,
                      virtualmod::Module = gen_virtual_module(actualmod);
                      jetconfigs...)
    interp = JETInterpreter(; jetconfigs...)
    config = ToplevelConfig(; jetconfigs...)
    return virtual_process(text,
                           filename,
                           actualmod,
                           virtualmod,
                           interp,
                           config,
                           )
end

function analyze_toplevel!(interp::JETInterpreter, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.uninferred = src
    mi.specTypes = Tuple{}

    transform_abstract_global_symbols!(interp, src)
    mi.def = interp.toplevelmod

    result = InferenceResult(mi);
    # toplevel frame doesn't need to be cached (and so it won't be optimized)
    frame = InferenceState(result, src, #=cached=# false, interp)::InferenceState;

    return analyze_frame!(interp, frame)
end

# HACK this is an native hack to re-use `AbstractInterpreter`'s approximated slot types for
# assignments of abstract global variables, which are represented as toplevel symbols at this point;
# the idea is just to transform them into slots from symbols and use their approximated type
# on their assignment.
# NOTE that `transform_abstract_global_symbols!` will produce really invalid code for
# actual interpretation or execution, but all the statements won't be interpreted anymore
# by `ConcreteInterpreter` nor executed anyway since toplevel frames aren't cached
function transform_abstract_global_symbols!(interp::JETInterpreter, src::CodeInfo)
    nslots = length(src.slotnames)
    abstrct_global_variables = Dict{Symbol,Int}()
    concretized = interp.concretized

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

    interp.global_slots = Dict(idx => slotname for (slotname, idx) in abstrct_global_variables)
end

# TODO `analyze_call_builtin!` ?
function analyze_gf_by_type!(interp::JETInterpreter, @nospecialize(tt::Type{<:Tuple}))
    mm = get_single_method_match(tt, InferenceParams(interp).MAX_METHODS, get_world_counter(interp))
    return analyze_method_signature!(interp, mm.method, mm.spec_types, mm.sparams)
end

function get_single_method_match(@nospecialize(tt), lim, world)
    mms = _methods_by_ftype(tt, lim, world)
    @assert !isa(mms, Bool) "unable to find matching method for $(tt)"

    filter!(mm::MethodMatch->mm.spec_types===tt, mms)
    @assert length(mms) == 1 "unable to find single target method for $(tt)"

    return first(mms)::MethodMatch
end

analyze_method!(interp::JETInterpreter, m::Method) =
    analyze_method_signature!(interp, m, m.sig, method_sparams(m))

function method_sparams(m::Method)
    s = TypeVar[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(s, sig.var)
        sig = sig.body
    end
    return svec(s...)
end

function analyze_method_signature!(interp::JETInterpreter,
                                   m::Method,
                                   @nospecialize(atype),
                                   sparams::SimpleVector,
                                   )
    mi = specialize_method(m, atype, sparams)

    result = InferenceResult(mi)

    frame = InferenceState(result, #=cached=# true, interp)::InferenceState

    return analyze_frame!(interp, frame)
end

function analyze_frame!(interp::JETInterpreter, frame::InferenceState)
    typeinf(interp, frame)

    # report `throw` calls "appropriately";
    # if the final return type here is `Bottom`-annotated, it _may_ mean the control flow
    # didn't catch some of the `UncaughtExceptionReport`s stashed within `interp.uncaught_exceptions`,
    if get_result(frame) === Bottom
        if !isempty(interp.uncaught_exceptions)
            append!(interp.reports, interp.uncaught_exceptions)
        end
    end

    return interp, frame
end

# test, interactive
# =================

"""
    @analyze_call [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
  [`analyze_call`](@ref) on the resulting expression.
As with `@code_typed` and its family, any of [JET configurations](@ref) can be given as the optional
  arguments like this:
```julia
# analyzes `rand(::Type{Bool})` with `aggressive_constant_propagation` configuration turned off
julia> @analyze_call aggressive_constant_propagation=false rand(Bool)
```
"""
macro analyze_call(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :analyze_call, ex0)
end

"""
    analyze_call(f, types = Tuple{}; jetconfigs...) -> (interp::JETInterpreter, frame::InferenceFrame)

Analyzes the generic function call with the given type signature, and returns:
- `interp::JETInterpreter`, which contains analyzed error reports and such
- `frame::InferenceFrame`, which is the final state of the abstract interpretation
"""
function analyze_call(@nospecialize(f), @nospecialize(types = Tuple{}); jetconfigs...)
    ft = Core.Typeof(f)
    if isa(types, Type)
        u = unwrap_unionall(types)
        tt = rewrap_unionall(Tuple{ft, u.parameters...}, types)
    else
        tt = Tuple{ft, types...}
    end

    interp = JETInterpreter(; jetconfigs...)
    return analyze_gf_by_type!(interp, tt)
end

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
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_call, ex0)
end

"""
    report_call(f, types = Tuple{}; jetconfigs...) -> result_type::Any

Analyzes the generic function call with the given type signature, and then prints collected
  error points to `stdout`, and finally returns the result type of the call.
"""
function report_call(@nospecialize(f), @nospecialize(types = Tuple{}); jetconfigs...)
    interp, frame = analyze_call(f, types; jetconfigs...)
    print_reports(interp.reports; jetconfigs...)
    return get_result(frame)
end

# for inspection
macro lwr(ex) QuoteNode(lower(__module__, ex)) end
macro src(ex) QuoteNode(first(lower(__module__, ex).args)) end

# exports
# =======

export
    report_file,
    report_and_watch_file,
    report_package,
    report_text,
    @analyze_call,
    analyze_call,
    @report_call,
    report_call

end
