module JET

# exports
# =======

export
    # generic entries & default jetanalyzer
    @report_call, report_call, @test_call, test_call,
    # optanalyzer
    @report_opt, report_opt, @test_opt, test_opt,
    # toplevel entries
    report_file, report_and_watch_file, report_package, report_text,
    # configurations
    LastFrameModule, AnyFrameModule

let README = normpath(dirname(@__DIR__), "README.md")
    s = read(README, String)
    s = replace(s, ":bangbang:"=>"!!! note")
    @doc s JET
    include_dependency(README)
end

Base.Experimental.@optlevel 1

const CC = Core.Compiler

# imports
# =======

# `AbstractAnalyzer`
import .CC:
    #= cicache.jl =#
    # get, getindex, haskey, push!, setindex!,
    #= types.jl =#
    InferenceParams, OptimizationParams, add_remark!, bail_out_call, bail_out_toplevel_call,
    code_cache, get_inference_cache, get_world_counter, lock_mi_inference, may_compress,
    may_discard_trees, may_optimize, unlock_mi_inference, verbose_stmt_info, method_table,
    #= inferenceresult.jl =#
    cache_lookup,
    #= inferencestate.jl =#
    InferenceState,
    #= tfuncs.jl =#
    builtin_tfunction, return_type_tfunc,
    #= abstractinterpretation.jl =#
    abstract_call, abstract_call_gf_by_type, abstract_call_method,
    abstract_call_method_with_const_args, abstract_eval_value_expr, abstract_eval_special_value,
    abstract_eval_statement, abstract_eval_value, abstract_invoke, add_call_backedges!,
    concrete_eval_call, concrete_eval_eligible, const_prop_entry_heuristic,
    #= typeinfer.jl =#
    _typeinf, finish!, finish, transform_result_for_cache, typeinf, typeinf_edge,
    #= optimize.jl =#
    inlining_policy

# `ConcreteInterpreter`
import JuliaInterpreter:
    #= interpreter.jl =#
    evaluate_call_recurse!, handle_err, step_expr!

# Test.jl integration
import Test:
    record

# usings
# ======
# TODO: really use `using` instead

import Core:
    Argument, Builtin, CodeInfo, CodeInstance, Const, GlobalRef, GotoIfNot, GotoNode,
    IntrinsicFunction, Intrinsics, LineInfoNode, MethodInstance, MethodMatch, MethodTable,
    NewvarNode, ReturnNode, SSAValue, SimpleVector, Slot, SlotNumber, svec

import .CC:
    AbstractInterpreter, ArgInfo, BasicBlock, Bottom, CFG, CallMeta, ConstCallInfo,
    InferenceResult, InternalCodeCache, InvokeCallInfo, LimitedAccuracy, MethodCallResult,
    MethodLookupResult, MethodMatchInfo, MethodMatches, NOT_FOUND, OptimizationState,
    UnionSplitInfo, UnionSplitMethodMatches, VarState, VarTable, WorldRange, WorldView,
    InternalMethodTable, CachedMethodTable, OverlayMethodTable,
    _methods_by_ftype, argextype, argtype_by_index, argtype_tail, argtypes_to_type,
    compute_basic_blocks, get_compileable_sig, hasintersect, has_free_typevars,
    ignorelimited, inlining_enabled, instanceof_tfunc, is_throw_call, isType, isconstType,
    issingletontype, may_invoke_generator, singleton_type, slot_id, specialize_method,
    switchtupleunion, tmerge, widenconst, ⊑

import Base:
    @invoke, @invokelatest, IdSet, default_tt, destructure_callex, parse_input_line,
    rewrap_unionall, uniontypes, unwrap_unionall

import Base.Meta:
    _parse_string, isexpr, lower

import Base.Experimental:
    @MethodTable, @overlay

using LoweredCodeUtils, JuliaInterpreter

import LoweredCodeUtils:
    #=NamedVar,=# add_control_flow!, #=add_named_dependencies!, add_requests!,=#
    add_ssa_preds!, add_typedefs!, callee_matches, find_typedefs, ismethod, istypedef,
    print_with_code, pushall!, rng

import JuliaInterpreter:
    @lookup, _INACTIVE_EXCEPTION, bypass_builtins, collect_args, #=finish!,=#
    is_quotenode_egal, is_return, maybe_evaluate_builtin, moduleof

import MacroTools:
    @capture, MacroTools, normalise, striplines

using InteractiveUtils

using Pkg, Pkg.TOML

import Test:
    Broken, DefaultTestSet, Error, Fail, FallbackTestSet, FallbackTestSetException, Pass,
    Result, TESTSET_PRINT_ENABLE, Test, get_testset

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

function anypush!(a::Vector{Any}, @nospecialize x...)
    na = length(a)
    nx = length(x)
    Base._growend!(a, nx)
    for i = 1:nx
        Base.arrayset(true, a, x[i], na+i)
    end
    return a
end

@static if !@isdefined(getglobal)
    const getglobal = getfield
end

@static isdefined(CC, :StmtInfo) && import .CC: StmtInfo

@static if VERSION ≥ v"1.10.0-DEV.96"
    using Base: _which
else
    function _which(@nospecialize(tt::Type);
        method_table::Union{Nothing,MethodTable,Core.Compiler.MethodTableView}=nothing,
        world::UInt=get_world_counter(),
        raise::Bool=false)
        if method_table === nothing
            table = Core.Compiler.InternalMethodTable(world)
        elseif isa(method_table, MethodTable)
            table = Core.Compiler.OverlayMethodTable(world, method_table)
        else
            table = method_table
        end
        match, = Core.Compiler.findsup(tt, table)
        if match === nothing
            raise && error("no unique matching method found for the specified argument types")
            return nothing
        end
        return match
    end
end

# macros
# ------

islnn(@nospecialize(x)) = isa(x, LineNumberNode)

# for inspection
macro lwr(ex) QuoteNode(lower(__module__, ex)) end
macro src(ex) QuoteNode(first(lower(__module__, ex).args)) end

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

# TODO remove this macro
"""
    @jetconfigurable [duplicated_names...] function config_func(args...; configurations...)
        ...
    end

Assert that there is no configuration naming conflict across `@jetconfigurable` methods.
Also add the dummy splat keyword argument `jetconfigs...` to the argument list if necessary
so that any configuration of other `@jetconfigurable` methods can be passed on to it.
And also insert a call to `validate_configs` at the head of the method's body.
"""
macro jetconfigurable(exs...)
    if length(exs) == 1
        funcdef = only(exs)
        duplicated_names = Symbol[]
    else
        funcdef = last(exs)
        duplicated_names = Symbol[(isa(name, QuoteNode) ? name.value : name)::Symbol for name in exs[1:end-1]]
    end
    @assert isexpr(funcdef, :(=)) || isexpr(funcdef, :function) "function definition should be given"
    funcdef = funcdef::Expr

    defsig = funcdef.args[1]
    if isexpr(defsig, :where)
        defsig = first(defsig.args)
    end
    defsig = defsig::Expr
    args = defsig.args::Vector{Any}
    thisname = first(args)::Symbol
    i = findfirst(@nospecialize(a)->isexpr(a, :parameters), args)
    if isnothing(i)
        @warn "no JET configurations are defined for `$thisname`"
        insert!(args, 2, Expr(:parameters, :(jetconfigs...)))
        configsname = :jetconfigs
    else
        kwargs = args[i]::Expr
        found = false
        for kwarg in kwargs.args
            if isexpr(kwarg, :...)
                found = true
                configsname = first(kwarg.args)::Symbol
                continue
            end
            kwargex = first(kwarg.args)
            kwargname = (isexpr(kwargex, :(::)) ? first(kwargex.args) : kwargex)::Symbol
            othername = get!(_JET_CONFIGURATIONS, kwargname, thisname)
            if thisname !== othername && kwargname ∉ duplicated_names
                throw("`$thisname` uses `$kwargname` JET configuration name which is already used by `$othername`")
            end
        end
        if !found
            push!(kwargs.args, :(jetconfigs...))
            configsname = :jetconfigs
        end
    end

    # insert validation to check if there are no invalid names
    assertion = :($validate_configs($configsname))
    body = funcdef.args[2]
    if isexpr(body, :block)
        i = findfirst(!islnn, body.args)::Int
        insert!(body.args, i, assertion)
    else
        funcdef.args[2] = Expr(:block, LineNumberNode(@__LINE__, @__FILE__), assertion, body)
    end

    return esc(funcdef)
end
const _JET_CONFIGURATIONS = Dict{Symbol,Symbol}() # TODO split this up

function validate_configs(jetconfigs, valid_keys = keys(_JET_CONFIGURATIONS))
    for (key, val) in jetconfigs
        if key ∉ valid_keys
            valrepr = LazyPrinter(io::IO->show(io,val))
            throw(JETConfigError(lazy"Given unexpected configuration: `$key = $valrepr`", key, val))
        end
    end
    return nothing
end

struct JETConfigError <: Exception
    msg::AbstractString
    key::Symbol
    val
    JETConfigError(msg::AbstractString, key::Symbol, @nospecialize(val)) = new(msg, key, val)
end
Base.showerror(io::IO, err::JETConfigError) = print(io, "JETConfigError: ", err.msg)

@static if VERSION ≥ v"1.10.0-DEV.117"
    using .CC: @nospecs
else
    using Base: is_function_def

    @doc """
        @nospecs def

    Adds `@nospecialize` annotation to non-annotated arguments of `def`.
    ```julia
    (Core.Compiler) julia> @macroexpand @nospecs function tfunc(𝕃::AbstractLattice, x, y::Bool, zs...)
                               x, ys
                           end
    :(function tfunc(\$(Expr(:meta, :specialize, :(𝕃::AbstractLattice))), x, y::Bool, zs...)
          #= REPL[3]:1 =#
          \$(Expr(:meta, :nospecialize, :x, :zs))
          #= REPL[3]:2 =#
          (x, ys)
      end)
    ```
    """
    macro nospecs(ex)
        is_function_def(ex) || throw(ArgumentError("expected function definition"))
        args, body = ex.args
        while isexpr(args, :where)
            args = args.args[1]
        end
        if isexpr(args, :call)
            args = args.args[2:end] # skip marking `@nospecialize` on the function itself
        else
            @assert isexpr(args, :tuple) # anonymous function
            args = args.args
        end
        names = Symbol[]
        for arg in args
            isexpr(arg, :macrocall) && continue
            if isexpr(arg, :...)
                arg = arg.args[1]
            elseif isexpr(arg, :kw)
                arg = arg.args[1]
            end
            isexpr(arg, :(::)) && continue
            @assert arg isa Symbol
            push!(names, arg)
        end
        @assert isexpr(body, :block)
        if !isempty(names)
            lin = first(body.args)::LineNumberNode
            nospec = Expr(:macrocall, Symbol("@nospecialize"), lin, names...)
            insert!(body.args, 2, nospec)
        end
        return esc(ex)
    end
end

# utils
# -----

# hash

function compute_hash(objs...)
    @assert length(objs) ≠ 0 "given no objects to be hashed"
    return _compute_hash(objs...)
end
_compute_hash(o, objs...) = hash(o, _compute_hash(objs...))
_compute_hash() = @static UInt === UInt64 ? 0xa49bd446c0a5d90e : 0xe45361ac

# state

const State     = Union{InferenceState,OptimizationState}
const StateAtPC = Tuple{State,Int}
const LineTable = Union{Vector{Any},Vector{LineInfoNode}}

get_stmt((sv, pc)::StateAtPC) = sv.src.code[pc]
get_lin((sv, pc)::StateAtPC) = begin
    codeloc = sv.src.codelocs[pc]
    linetable = sv.src.linetable::LineTable
    if 1 <= codeloc <= length(linetable)
        return linetable[codeloc]::LineInfoNode
    elseif isa(sv, OptimizationState) && codeloc == 0
        return nothing
    else
        # Packages might dynamically generate code, which does not reference
        # a source, see https://github.com/aviatesk/JET.jl/issues/273
        @static if fieldtype(LineInfoNode, :line) === Int32
            return LineInfoNode(sv.mod, :unknown, :unknown, Int32(0), Int32(0))
        else
            return LineInfoNode(sv.mod, :unknown, :unknown, 0, 0)
        end
    end
end
get_ssavaluetype((sv, pc)::StateAtPC) = (sv.src.ssavaluetypes::Vector{Any})[pc]

get_slottype(s::Union{StateAtPC,State}, slot) = get_slottype(s, slot_id(slot))
get_slottype((sv, pc)::StateAtPC, slot::Int) = get_slottype(sv, slot)
get_slottype(sv::State, slot::Int) = sv.slottypes[slot]

get_slotname(s::Union{StateAtPC,State}, slot) = get_slotname(s, slot_id(slot))
get_slotname((sv, pc)::StateAtPC, slot::Int) = sv.src.slotnames[slot]
get_slotname(sv::State, slot::Int) = sv.src.slotnames[slot]

# check if we're in a toplevel module
istoplevel(sv::State) = istoplevel(sv.linfo)
istoplevel(linfo::MethodInstance) = isa(linfo.def, Module)

# we can retrieve program-counter-level slottype during inference
get_slottype(s::Tuple{InferenceState,Int}, slot::Int) = (get_states(s)[slot]::VarState).typ
get_states((sv, pc)::Tuple{InferenceState,Int}) = stmt_types(sv)[pc]::VarTable
get_currpc(sv::InferenceState) = min(sv.currpc, length(sv.src.code))

struct StmtTypes
    sv::InferenceState
end
function stmt_types(sv::InferenceState)
    @static if hasfield(InferenceState, :bb_vartables)
        return StmtTypes(sv)
    else
        return sv.stmt_types
    end
end
@static if hasfield(InferenceState, :bb_vartables)
function Base.getindex(st::StmtTypes, pc::Int)
    block = CC.block_for_inst(st.sv.cfg, pc)
    return st.sv.bb_vartables[block]::VarTable
end
end

function is_compileable_mi(mi::MethodInstance)
    def = mi.def
    isa(def, Method) || return false
    return get_compileable_sig(def, mi.specTypes, mi.sparam_vals) !== nothing
end

get_linfo(sv::State) = sv.linfo
get_linfo(result::InferenceResult) = result.linfo
get_linfo(linfo::MethodInstance) = linfo

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

Base.show(io::IO, cache::CachedMethodTable) =
    print(io, typeof(cache), "(", Core.Compiler.length(cache.cache), " entries)")

# lattice

ignorenotfound(@nospecialize(t)) = t === NOT_FOUND ? Bottom : t
safewidenconst(@nospecialize t) = widenconst(ignorelimited(ignorenotfound(t)))

# method table
unwrap_method_table(::InternalMethodTable) = nothing
unwrap_method_table(method_table::OverlayMethodTable) = method_table.mt
unwrap_method_table(method_table::CachedMethodTable) = unwrap_method_table(method_table.table)

# logging

const JET_LOGGER_LEVEL = :JET_LOGGER_LEVEL
const JET_LOGGER_LEVEL_INFO = const DEFAULT_LOGGER_LEVEL = 0
const JET_LOGGER_LEVEL_DEBUG = 1
const JET_LOGGER_LEVELS = Dict(JET_LOGGER_LEVEL_INFO  => :info, JET_LOGGER_LEVEL_DEBUG => :debug)
const JET_LOGGER_LEVELS_DESC = let
    descs = map(collect(JET_LOGGER_LEVELS)) do (level, desc)
        if level == DEFAULT_LOGGER_LEVEL
            "`$level` (\"$desc\" level, default)"
        else
            "`$level` (\"$desc\" level)"
        end
    end
    join(descs, ", ")
end
jet_logger_level(@nospecialize io::IO) = get(io, JET_LOGGER_LEVEL, DEFAULT_LOGGER_LEVEL)::Int

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
- `res.source::AbstractString`: the identity key of this analysis
- `res.jetconfigs`: [JET configurations](@ref JET-configurations) used for this analysis

`JETToplevelResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically choosen and render the analysis result.
"""
struct JETToplevelResult{Analyzer<:AbstractAnalyzer,JETConfigs}
    analyzer::Analyzer
    res::VirtualProcessResult
    source::AbstractString
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
        target_modules = get_toplevel_target_modules(res.defined_modules; result.jetconfigs...)
        return configured_reports(reports; target_modules, result.jetconfigs...)
    end
end

@jetconfigurable function get_toplevel_target_modules(defined_modules;
    target_defined_modules::Bool = false)
    return target_defined_modules ? defined_modules : nothing
end

"""
    res::JETCallResult

Represents the result of JET's analysis on a function call.
- `res.result::InferenceResult`: the result of this analysis
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.source::AbstractString`: the identity key of this analysis
- `res.jetconfigs`: [JET configurations](@ref JET-configurations) used for this analysis

`JETCallResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically choosen and render the analysis result.
"""
struct JETCallResult{Analyzer<:AbstractAnalyzer,JETConfigs}
    result::InferenceResult
    analyzer::Analyzer
    source::AbstractString
    jetconfigs::JETConfigs
end
JETCallResult(result::InferenceResult, analyzer::AbstractAnalyzer, source::AbstractString;
              jetconfigs...) = JETCallResult(result, analyzer, source, jetconfigs)
@eval Base.iterate(res::JETCallResult, state=1) =
    return state > $(fieldcount(JETCallResult)) ? nothing : (getfield(res, state), state+1)

function get_result(result::JETCallResult)
    if any(@nospecialize(r::InferenceErrorReport) -> isa(r, GeneratorErrorReport), get_reports(result))
        return Bottom
    else
        return result.result.result
    end
end
function get_reports(result::JETCallResult)
    reports = get_reports(result.analyzer, result.result)
    return configured_reports(reports; result.jetconfigs...)
end

"""
Configurations for [JET's analysis results](@ref analysis-result).
These configurations are always active.

---
- `target_modules = nothing` \\
  A configuration to filter out reports by specifying module contexts where problems should be reported.

  By default (`target_modules = nothing`), JET reports all detected problems.
  If specified, a problem is reported if its module context matches any of `target_modules`
  settings and hidden otherwise. `target_modules` should be an iterator of whose element is
  either of the data types below that match [`report::InferenceErrorReport`](@ref InferenceErrorReport)'s
  context module as follows:
  - `m::Module` or `JET.LastFrameModule(m::Module)`: matches if the module context of  `report`'s innermost stack frame is `m`
  - `JET.AnyFrameModule(m::Module)`: matches if module context of any of `report`'s stack frame is `m`
  - user-type `T`: matches according to user-definition overload `match_module(::T, report::InferenceErrorReport)`
---
- `ignored_modules = nothing` \\
  A configuration to filter out reports by specifying module contexts where problems should be ignored.

  By default (`ignored_modules = nothing`), JET reports all detected problems.
  If specified, a problem is hidden if its module context matches any of `ignored_modules`
  settings and reported otherwise. `ignored_modules` should be an iterator of whose element is
  either of the data types below that match [`report::InferenceErrorReport`](@ref InferenceErrorReport)'s
  context module as follows:
  - `m::Module` or `JET.LastFrameModule(m::Module)`: matches if the module context of  `report`'s innermost stack frame is `m`
  - `JET.AnyFrameModule(m::Module)`: matches if module context of any of `report`'s stack frame is `m`
  - user-type `T`: matches according to user-definition overload `match_module(::T, report::InferenceErrorReport)`
---
- `report_config = nothing` \\
  Additional configuration layer to filter out reports with user-specified strategies.
  By default (`report_config = nothing`), JET will use the module context based configurations
  elaborated above and below. If user-type `T` is given, then JET will report problems based
  on the logic according to an user-overload `configured_reports(::T, reports::Vector{InferenceErrorReport})`,
  and the `target_modules` and `ignored_modules` configurations are not really active.
---

# Examples

```julia-repl
julia> function foo(a)
           r1 = sum(a)       # => Base: MethodError(+(::Char, ::Char)), MethodError(zero(::Type{Char}))
           r2 = undefsum(a)  # => @__MODULE__: UndefVarError(:undefsum)
           return r1, r2
       end;

# by default, JET will print all the collected reports:
julia> @report_call foo("julia")
═════ 3 possible errors found ═════
┌ @ REPL[1]:2 r1 = sum(a)
│┌ @ reduce.jl:549 Base.:(var"#sum#281")(pairs(NamedTuple()), #self#, a)
││┌ @ reduce.jl:549 sum(identity, a)
│││┌ @ reduce.jl:520 Base.:(var"#sum#280")(pairs(NamedTuple()), #self#, f, a)
││││┌ @ reduce.jl:520 mapreduce(f, Base.add_sum, a)
│││││┌ @ reduce.jl:294 Base.:(var"#mapreduce#277")(pairs(NamedTuple()), #self#, f, op, itr)
││││││┌ @ reduce.jl:294 mapfoldl(f, op, itr)
│││││││┌ @ reduce.jl:162 Base.:(var"#mapfoldl#273")(Base._InitialValue(), #self#, f, op, itr)
││││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f, op, init, itr)
│││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
││││││││││┌ @ reduce.jl:48 v = Base._foldl_impl(op, nt, itr)
│││││││││││┌ @ reduce.jl:62 v = op(v, y[1])
││││││││││││┌ @ reduce.jl:81 op.rf(acc, x)
│││││││││││││┌ @ reduce.jl:24 x + y
││││││││││││││ no matching method found for `+(::Char, ::Char)`: (x::Char + y::Char)::Union{}
│││││││││││││└────────────────
││││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op, itr)
│││││││││││┌ @ reduce.jl:370 Base.reduce_empty_iter(op, itr, Base.IteratorEltype(itr))
││││││││││││┌ @ reduce.jl:371 Base.reduce_empty(op, eltype(itr))
│││││││││││││┌ @ reduce.jl:347 Base.reduce_empty(op.rf, T)
││││││││││││││┌ @ reduce.jl:339 Base.reduce_empty(+, T)
│││││││││││││││┌ @ reduce.jl:330 zero(T)
││││││││││││││││ no matching method found for `zero(::Type{Char})`: zero(T::Type{Char})::Union{}
│││││││││││││││└─────────────────
┌ @ REPL[1]:3 r2 = undefsum(a)
│ `undefsum` is not defined
└─────────────

# with `target_modules=(@__MODULE__,)`, JET will only report the problems detected within the `@__MODULE__` module:
julia> @report_call target_modules=(@__MODULE__,) foo("julia")
════ 1 possible error found ═════
┌ @ REPL[1]:3 r2 = undefsum(a)
│ `undefsum` is not defined
└─────────────

# with `ignored_modules=(Base,)`, JET will ignore the errors detected within the `Base` module:
julia> @report_call ignored_modules=(Base,) foo("julia")
════ 1 possible error found ═════
┌ @ REPL[1]:3 r2 = undefsum(a)
│ `undefsum` is not defined
└─────────────
```
---
"""
@jetconfigurable function configured_reports(reports::Vector{InferenceErrorReport};
    report_config = nothing, target_modules = nothing, ignored_modules = nothing)
    if report_config === nothing
        report_config = ReportConfig(target_modules, ignored_modules)
    end
    return configured_reports(report_config, reports)
end

struct ReportConfig{S,T}
    target_modules::S
    ignored_modules::T
end

struct LastFrameModule mod::Module end
struct AnyFrameModule mod::Module end

match_module(mod::Module, @nospecialize(report::InferenceErrorReport)) = match_module(LastFrameModule(mod), report)
function match_module(mod::LastFrameModule, @nospecialize(report::InferenceErrorReport))
    return linfomod(last(report.vst).linfo) === mod.mod
end
function match_module(mod::AnyFrameModule, @nospecialize(report::InferenceErrorReport))
    return any(vsf->linfomod(vsf.linfo)===mod.mod, report.vst)
end
@noinline match_module(x::Any, @nospecialize(report::InferenceErrorReport)) =
    error(lazy"`JET.match_module(::$x, ::InferenceErrorReport)` is not implemented")

function configured_reports(config::ReportConfig, reports::Vector{InferenceErrorReport})
    if config.target_modules !== nothing
        reports = filter(reports) do @nospecialize report
            return any(m->match_module(m,report), config.target_modules)
        end
    end
    if config.ignored_modules !== nothing
        reports = filter(reports) do @nospecialize report
            return !any(m->match_module(m,report), config.ignored_modules)
        end
    end
    return reports
end
@noinline configured_reports(x::Any, reports::Vector{InferenceErrorReport}) =
    error(lazy"`JET.configured_reports(::$x, ::Vector{InferenceErrorReport})` is not implemented")

linfomod(linfo::MethodInstance) = (def = linfo.def; isa(def, Method) ? def.module : def)

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
const JULIA_DIR = let
    p1 = normpath(Sys.BINDIR, "..", "..")
    p2 = normpath(Sys.BINDIR, Base.DATAROOTDIR, "julia")
    ispath(normpath(p1, "base")) ? p1 : p2
end

struct LazyPrinter; f; end
Base.show(io::IO, l::LazyPrinter) = l.f(io)

# default UI (console)
include("ui/print.jl")
# UI for VSCode
include("ui/vscode.jl")

# entries
# =======

# abstractinterpret
# -----------------

# TODO `analyze_builtin!` ?
function analyze_gf_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}))
    match = find_single_match(tt, analyzer)
    return analyze_method_signature!(analyzer, match.method, match.spec_types, match.sparams)
end

function find_single_match(@nospecialize(tt), analyzer::AbstractAnalyzer)
    match = _which(tt; method_table=method_table(analyzer), world=get_world_counter(analyzer), raise=false)
    match === nothing && single_match_error(tt)
    return match
end

@noinline function single_match_error(@nospecialize tt)
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    error(lazy"unable to find single target method for `$sig`")
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

function analyze_method_signature!(analyzer::AbstractAnalyzer, m::Method, @nospecialize(atype), sparams::SimpleVector)
    mi = specialize_method(m, atype, sparams)::MethodInstance
    return analyze_method_instance!(analyzer, mi)
end

function analyze_method_instance!(analyzer::AbstractAnalyzer, mi::MethodInstance)
    result = InferenceResult(mi)

    frame = InferenceState(result, #=cache=# :global, analyzer)

    isnothing(frame) && return analyzer, result

    return analyze_frame!(analyzer, frame)
end

function InferenceState(result::InferenceResult, cache::Symbol, analyzer::AbstractAnalyzer)
    init_result!(analyzer, result) # set `JETResult` for succeeding JET analysis
    return @invoke InferenceState(result::InferenceResult, cache::Symbol, analyzer::AbstractInterpreter)
end

function analyze_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    set_entry!(analyzer, frame.linfo)
    typeinf(analyzer, frame)
    return analyzer, frame.result
end

is_entry(analyzer::AbstractAnalyzer, mi::MethodInstance) = get_entry(analyzer) === mi

# toplevel
# --------

"""
    report_file(filename::AbstractString; jetconfigs...) -> JETToplevelResult

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
    See [JET's top-level analysis configurations](@ref toplevel-config) for more details.
"""
@jetconfigurable function report_file(filename::AbstractString; jetconfigs...)
    isfile(filename) || throw(ArgumentError("$filename doesn't exist"))
    jetconfigs = set_if_missing(jetconfigs, :toplevel_logger, IOContext(stdout, JET_LOGGER_LEVEL => DEFAULT_LOGGER_LEVEL))
    configfile = find_config_file(dirname(abspath(filename)))
    if !isnothing(configfile)
        config = parse_config_file(configfile)
        merge!(jetconfigs, config) # overwrite configurations
        toplevel_logger = get(jetconfigs, :toplevel_logger, nothing)
        with_toplevel_logger(toplevel_logger; filter=≥(JET_LOGGER_LEVEL_INFO)) do @nospecialize(io)
            println(io, lazy"applied JET configurations in $configfile")
        end
    end
    entrytext = read(filename, String)
    return report_text(entrytext, filename; jetconfigs...)
end

set_if_missing(configs, args...) = (@nospecialize; set_if_missing!(kwargs_dict(configs), args...))

function set_if_missing!(configs::Dict{Symbol,Any}, key::Symbol, @nospecialize(value))
    haskey(configs, key) && return configs
    push!(configs, key => value)
    return configs
end

function kwargs_dict(@nospecialize configs)
    dict = Dict{Symbol,Any}()
    for (key, val) in configs
        dict[Symbol(key)] = val
    end
    return dict
end

function find_config_file(dir::AbstractString)
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
parse_config_file(path::AbstractString) = process_config_dict(TOML.parsefile(path))

process_config_dict(configs) = process_config_dict!(kwargs_dict(configs))

function process_config_dict!(config_dict::Dict{Symbol,Any})
    context = get(config_dict, :context, nothing)
    if !isnothing(context)
        isa(context, String) ||
            throw(JETConfigError("`context` should be string of Julia code", :context, context))
        config_dict[:context] = Core.eval(Main, trymetaparse(context, :context))
    end
    concretization_patterns = get(config_dict, :concretization_patterns, nothing)
    if !isnothing(concretization_patterns)
        isa(concretization_patterns, Vector{String}) ||
            throw(JETConfigError("`concretization_patterns` should be array of string of Julia expression", :concretization_patterns, concretization_patterns))
        config_dict[:concretization_patterns] = trymetaparse.(concretization_patterns, :concretization_patterns)
    end
    toplevel_logger = get(config_dict, :toplevel_logger, nothing)
    if !isnothing(toplevel_logger)
        isa(toplevel_logger, String) ||
            throw(JETConfigError("`toplevel_logger` should be string of Julia code", :toplevel_logger, toplevel_logger))
        config_dict[:toplevel_logger] = Core.eval(Main, trymetaparse(toplevel_logger, :toplevel_logger))
    end
    return config_dict
end

function trymetaparse(s::String, name::Symbol)
    s = strip(s)
    ret = Meta.parse(s; raise=false)
    if isexpr(ret, :error) || isexpr(ret, :incomplete)
        err = ret.args[1]
        msg = lazy"""Failed to parse configuration string.
          ∘ given: `$s`
          ∘ error: $err
        """
        throw(JETConfigError(msg, name, s))
    end
    return ret
end

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
@jetconfigurable function report_package(
    package::Union{AbstractString,Module,Nothing} = nothing;
    jetconfigs...)
    filename = get_package_file(package)
    jetconfigs = kwargs_dict(jetconfigs)
    set_if_missing!(jetconfigs, :analyze_from_definitions, true)
    set_if_missing!(jetconfigs, :concretization_patterns, [:(x_)]) # concretize all top-level code
    return report_file(filename; jetconfigs...)
end

function get_package_file(package::AbstractString)
    filename = Base.find_package(package)
    isnothing(filename) && error(lazy"unknown package $package.")
    return filename
end

function get_package_file(package::Module)
    filename = pathof(package)
    isnothing(filename) && error("cannot analyze a module defined in the REPL.")
    return filename
end

function get_package_file(::Nothing)
    project = Pkg.project()
    project.ispackage || error(lazy"active project at $(project.path) is not a package.")
    return normpath(dirname(project.path), "src", project.name::String * ".jl")
end

"""
    report_text(text::AbstractString,
                filename::AbstractString = "top-level";
                jetconfigs...) -> JETToplevelResult

Analyzes `text` and returns [`JETToplevelResult`](@ref).
"""
@jetconfigurable :analyzer function report_text(text::AbstractString,
    filename::AbstractString = "top-level";
    analyzer::Type{Analyzer} = JETAnalyzer,
    jetconfigs...) where {Analyzer<:AbstractAnalyzer}
    analyzer′ = Analyzer(; jetconfigs...)
    config = ToplevelConfig(; jetconfigs...)
    res = virtual_process(text, filename, analyzer′, config)
    analyzername = nameof(typeof(analyzer))
    source = lazy"$analyzername: report_file(\"$filename\")"
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
    @jetconfigurable function WatchConfig(;
        revise_all::Bool = true,
        revise_modules   = nothing)
        return new(revise_all,
                   revise_modules)
    end
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
function analyze_toplevel!(analyzer::AbstractAnalyzer, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{}

    mi.def = mod = get_toplevelmod(analyzer)
    src = transform_abstract_global_symbols!(analyzer, src)
    src = resolve_toplevel_symbols!(mod, src)
    @static if VERSION ≥ v"1.10.0-DEV.112"
        @atomic mi.uninferred = src
    else
        mi.uninferred = src
    end

    result = InferenceResult(mi);
    init_result!(analyzer, result) # set `JETResult` for succeeding JET analysis
    # NOTE toplevel frames don't really need to be cached, but still better to be optimized
    # in order to get reasonable `UncaughtExceptionReport`, and also, otherwise
    # `typeinf_edge` won't add "toplevel-to-callee" edges
    frame = InferenceState(result, src, #=cache=# :global, analyzer)::InferenceState

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
function resolve_toplevel_symbols!(mod::Module, src::CodeInfo)
    newsrc = copy(src)
    @ccall jl_resolve_globals_in_ir(
        #=jl_array_t *stmts=# newsrc.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# svec()::Any,
        #=int binding_effects=# 0::Int)::Cvoid
    return newsrc
end

# interactive
# -----------

# TODO improve inferrability by making `analyzer` argument positional

"""
    @report_call [jetconfigs...] f(args...)

Evaluates the arguments to the function call, determines its types, and then calls
[`report_call`](@ref) on the resulting expression.
By default, it uses [`JETAnalyzer`](@ref) that performs [the error analysis](@ref jetanalysis).
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
    report_call(f, [types];
                analyzer::Type{<:AbstractAnalyzer} = JETAnalyzer,
                jetconfigs...) -> JETCallResult
    report_call(tt::Type{<:Tuple};
                analyzer::Type{<:AbstractAnalyzer} = JETAnalyzer,
                jetconfigs...) -> JETCallResult

Analyzes a generic function call with the given type signature with `analyzer`.
By default, it uses [`JETAnalyzer`](@ref) that performs [the error analysis](@ref jetanalysis).
Finally returns the analysis result as [`JETCallResult`](@ref).
"""
@jetconfigurable function report_call(
    @nospecialize(f), @nospecialize(types = default_tt(f));
    jetconfigs...)
    tt = Base.signature_type(f, types)
    return report_call(tt; jetconfigs...)
end

@jetconfigurable :analyzer function report_call(@nospecialize(tt::Type{<:Tuple});
    analyzer::Type{Analyzer} = JETAnalyzer,
    jetconfigs...) where {Analyzer<:AbstractAnalyzer}
    analyzer = Analyzer(; jetconfigs...)
    analyzer, result = analyze_gf_by_type!(analyzer, tt)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    source = lazy"$analyzername: $sig"
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
  ┌ @ none:2 goto %4 if not cond
  │ non-boolean (Any) used in boolean context: goto %4 if not cond
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
check errors: JET-test failed at REPL[9]:3
  Expression: #= REPL[9]:3 =# JET.@test_call f(ref)
  ═════ 1 possible error found ═════
  ┌ @ REPL[7]:1 sin(ref[])
  │ no matching method found for `sin(::Nothing)` (1/2 union split): sin((ref::Base.RefValue{Union{Nothing, Int64}})[]::Union{Nothing, Int64})::Union{}
  └─────────────

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
        if isexpr(x, :(=))
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
        $Error(:test_error, $orig_expr, err, $(Base.current_exceptions()), $source)
    end) |> Base.remove_linenums!
    return testres, orig_expr
end

"""
    test_call(f, [types]; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_call(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_call(f, types; jetconfigs...`](@ref report_call) and tests that the generic
function call `f(args...)` is free from problems that `report_call` can detect.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_call`](@ref).
"""
@jetconfigurable function test_call(@nospecialize(args...);
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
            Error(:test_error, orig_expr, err, Base.current_exceptions(), source)
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
        canonicalname = string(parentmodule(x), '.', nameof(x))
        canonicalpath = let pat = r"var\"(.+)\""=>s"\1" # necessary for successful package analysis
            Symbol.(replace.(split(canonicalname, '.'), Ref(pat)))
        end
        modpath = Expr(:., canonicalpath[1:end-1]...)
        symname = last(canonicalpath)
        sympath = Expr(:., symname)
        importex = Expr(:import, Expr(:(:), modpath, sympath))
        exportex = Expr(:export, symname)
        ex = Expr(:block)
        push!(ex.args, importex, exportex)
        Core.eval(JETInterface, ex)
        push!(JETInterface.DOCUMENTED_NAMES, symname)
    end
end

reexport_as_api!(JETInterface,
    # AbstractAnalyzer API
    AbstractAnalyzer, AnalyzerState, ReportPass, AnalysisCache, aggregation_policy,
    VSCode.vscode_diagnostics_order,
    # InferenceErrorReport API
    InferenceErrorReport, copy_report, print_report_message, print_signature, report_color,
    # development utilities
    add_new_report!, var"@jetreport")

# builtin analyzers
# =================

include("analyzers/jetanalyzer.jl")
include("analyzers/optanalyzer.jl")

end
