let README = normpath(dirname(@__DIR__), "README.md")
    s = read(README, String)
    s = replace(s,
        # replace the GitHub README admonition syntax with that of the Julia documentation
        "> [!NOTE]" => "!!! note",
        "> [!WARNING]" => "!!! warning",
        "> [!IMPORTANT]" => "!!! note",
        r"^\> (.+)$"m=>s"    \1",
        r"^\>$"m=>s"")
    @doc s JET
    include_dependency(README)
end

Base.Experimental.@optlevel 1

# usings
# ======

using Core: Builtin, IntrinsicFunction, Intrinsics, SimpleVector, svec

using Core.IR

using .CC: @nospecs, ⊑,
    AbsIntState, AbstractInterpreter, AbstractLattice, ArgInfo, Bottom, CFG,
    CachedMethodTable, CallMeta, ConstCallInfo, Effects, EFFECTS_THROWS, Future,
    InferenceParams, InferenceResult, InferenceState, InternalMethodTable, InvokeCallInfo,
    MethodCallResult, MethodMatchInfo, MethodMatches, NOT_FOUND, OptimizationState,
    OptimizationParams, OverlayMethodTable, RTEffects, StatementState, StmtInfo,
    UnionSplitInfo, UnionSplitMethodMatches, VarState, VarTable, WorldRange, WorldView,
    argextype, argtype_by_index, argtypes_to_type, compute_basic_blocks,
    construct_postdomtree, hasintersect, ignorelimited, instanceof_tfunc,
    nearest_common_dominator, singleton_type, slot_id, specialize_method, tmeet, tmerge,
    typeinf_lattice, widenconst, widenlattice

using Base: IdSet, PkgId, get_world_counter, generating_output

using Base.Meta: ParseError, isexpr, lower

using Base.Experimental: @MethodTable, @overlay

using JuliaSyntax: JuliaSyntax as JS
using .JS: @K_str

using CodeTracking: CodeTracking

using LoweredCodeUtils: LoweredCodeUtils, add_ssa_preds!, callee_matches

using JuliaInterpreter: _INACTIVE_EXCEPTION, Frame, Interpreter, JuliaInterpreter

using MacroTools: @capture, normalise, striplines

using InteractiveUtils: InteractiveUtils, gen_call_with_extracted_types_and_kwargs

using Pkg: Pkg, TOML

using Revise

using Test:
    Broken, DefaultTestSet, Error, Fail, FallbackTestSet, FallbackTestSetException, Pass,
    Result, TESTSET_PRINT_ENABLE, Test, get_testset

# common
# ======

const Argtypes = Vector{Any}

const CONFIG_FILE_NAME = ".JET.toml"

# TODO define all interface functions in JETInterface?

function copy_report end
function print_report_message end
function print_signature end

"""
    JETInterface

This `baremodule` exports names that form the APIs of [`AbstractAnalyzer` Framework](@ref AbstractAnalyzer-Framework).
`using JET.JETInterface` loads all names that are necessary to define a plugin analysis.
"""
baremodule JETInterface end

# hooks
# -----

const INIT_HOOKS = Function[]
push_inithook!(f) = push!(INIT_HOOKS, f)
__init__() = foreach(@nospecialize(f)->f(), INIT_HOOKS)

global debug_toplevel_logger::IO
push_inithook!() do
    global debug_toplevel_logger = IOContext(stderr, JET_LOGGER_LEVEL=>1)
end

# compat
# ------

using .CC.IRShow: LineInfoNode
using .CC: ConstCallResult
# FIXME do this automatically when loading non-fallback version of the Compiler stdlib
# push_inithook!() do
#     @eval InteractiveUtils.@activate Compiler
# end

# macros
# ------

islnn(@nospecialize(x)) = isa(x, LineNumberNode)

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

struct JETConfigError <: Exception
    msg::AbstractString
    key::Symbol
    val
    JETConfigError(msg::AbstractString, key::Symbol, val) =
        (@nospecialize msg val; new(msg, key, val))
end
Base.showerror(io::IO, err::JETConfigError) = print(io, "JETConfigError: ", err.msg)

# utils
# -----

# hash

function compute_hash(objs...)
    @assert length(objs) ≠ 0 "given no objects to be hashed"
    return _compute_hash(objs...)
end
_compute_hash(o, objs...) = hash(o, _compute_hash(objs...))
let hash_seed = rand(UInt)
    global _compute_hash() = hash_seed
end

# state

const State     = Union{InferenceState,OptimizationState}
const StateAtPC = Tuple{State,Int}
const LineTable = Union{Vector{Any},Vector{LineInfoNode}}

get_stmt((sv, pc)::StateAtPC) = sv.src.code[pc]
get_lin((sv, pc)::StateAtPC) = _get_lin(sv, pc)
_get_lin(sv, pc) = _get_lin(sv.linfo, sv.src, pc)
function _get_lin(mi::MethodInstance, src::CodeInfo, pc::Int)
    # TODO optimize the allocation here for un-optimized debuginfo
    lins = CC.IRShow.buildLineInfoNode(src.debuginfo, mi, pc)
    if isempty(lins)
        return LineInfoNode(mi, src.debuginfo.def::Symbol, mi.def.line)
    end
    return first(lins)
end
function get_lins((sv, pc)::StateAtPC)
    return CC.IRShow.buildLineInfoNode(sv.src.debuginfo, sv.linfo, pc)
end
get_ssavaluetype((sv, pc)::StateAtPC) = (sv.src.ssavaluetypes::Vector{Any})[pc]

get_slottype(s::Union{StateAtPC,State}, slot) = get_slottype(s, slot_id(slot))
get_slottype((sv, pc)::StateAtPC, slot::Int) = get_slottype(sv, slot)
get_slottype(sv::State, slot::Int) = sv.slottypes[slot]

get_slotname(s::Union{StateAtPC,State}, slot) = get_slotname(s, slot_id(slot))
get_slotname((sv, pc)::StateAtPC, slot::Int) = sv.src.slotnames[slot]
get_slotname(sv::State, slot::Int) = sv.src.slotnames[slot]

# check if we're in a toplevel module
istoplevelframe(sv::State) = istoplevelframe(CC.frame_instance(sv))
istoplevelframe(mi::MethodInstance) = isa(mi.def, Module)

# we can retrieve program-counter-level slottype during inference
get_slottype(s::Tuple{InferenceState,Int}, slot::Int) = (get_states(s)[slot]::VarState).typ
get_states((sv, pc)::Tuple{InferenceState,Int}) = stmt_types(sv)[pc]::VarTable
get_currpc(sv::InferenceState) = min(sv.currpc, length(sv.src.code))

struct StmtTypes
    sv::InferenceState
end
stmt_types(sv::InferenceState) = StmtTypes(sv)
function Base.getindex(st::StmtTypes, pc::Int)
    block = CC.block_for_inst(st.sv.cfg, pc)
    return st.sv.bb_vartables[block]::VarTable
end

function is_compileable_mi(mi::MethodInstance)
    def = mi.def
    isa(def, Method) || return false
    return CC.get_compileable_sig(def, mi.specTypes, mi.sparam_vals) !== nothing
end

get_linfo(sv::State) = sv.linfo
get_linfo(result::InferenceResult) = result.linfo
get_linfo(linfo::MethodInstance) = linfo

is_constant_propagated(frame::InferenceState) = is_constant_propagated(frame.result)
is_constant_propagated(result::InferenceResult) = result.overridden_by_const !== nothing

struct TypeUnassigned end    # for when inference doesn't bother assigning a type to a slot (e.g. dead code)

# lattice

ignorenotfound(@nospecialize(t)) = t === NOT_FOUND ? Bottom : t
safewidenconst(@nospecialize t) = widenconst(ignorelimited(ignorenotfound(t)))

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

abstract type ConcreteInterpreter <: JuliaInterpreter.Interpreter end

"""
    interpret_world(interp::ConcreteInterpreter) -> world::Union{UInt,Nothing}

Return the world age to use for interpretation performed by the given `interp`,
or `nothing` to use the current world.

When a specific world age is returned, the interpreter will invoke functions
within that fixed world using `Base.invoke_in_world`. This makes the interpretation
implementation more robust against potential invalidations that may be caused
by loading external packages.

The default implementation returns `nothing`, meaning interpretation runs in
the latest world. Specific interpreter implementations may override this to return
a fixed world age for stability.
"""
interpret_world(::ConcreteInterpreter) = nothing

include("abstractinterpret/inferenceerrorreport.jl")
include("abstractinterpret/abstractanalyzer.jl")
include("abstractinterpret/typeinfer.jl")

"""
    print_report(io::IO, report::ToplevelErrorReport)

Prints a report of the top-level error `report` to the given `io`.
"""
function print_report end

include("toplevel/virtualprocess.jl")

# results
# =======

"""
    res::JETToplevelResult

Represents the result of JET's analysis on a top-level script.
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.res::VirtualProcessResult`: [`VirtualProcessResult`](@ref) collected from this analysis
- `res.source::AbstractString`: the identity key of this analysis
- `res.jetconfigs`: configurations used for this analysis

`JETToplevelResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically chosen and render the analysis result.
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
        return configured_reports(res.inference_error_reports; result.jetconfigs...)
    end
end

"""
    res::JETCallResult

Represents the result of JET's analysis on a function call.
- `res.result::InferenceResult`: the result of this analysis
- `res.analyzer::AbstractAnalyzer`: [`AbstractAnalyzer`](@ref) used for this analysis
- `res.source::AbstractString`: the identity key of this analysis
- `res.jetconfigs`: configurations used for this analysis

`JETCallResult` implements `show` methods for each different frontend.
An appropriate `show` method will be automatically chosen and render the analysis result.
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

"""
    rpts = JET.get_reports(result::JETCallResult)

Split `result` into a vector of reports, one per issue.
"""
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
┌ foo(a::String) @ Main ./REPL[14]:2
│┌ sum(a::String) @ Base ./reduce.jl:564
││┌ sum(a::String; kw::@Kwargs{}) @ Base ./reduce.jl:564
│││┌ sum(f::typeof(identity), a::String) @ Base ./reduce.jl:535
││││┌ sum(f::typeof(identity), a::String; kw::@Kwargs{}) @ Base ./reduce.jl:535
│││││┌ mapreduce(f::typeof(identity), op::typeof(Base.add_sum), itr::String) @ Base ./reduce.jl:307
││││││┌ mapreduce(f::typeof(identity), op::typeof(Base.add_sum), itr::String; kw::@Kwargs{}) @ Base ./reduce.jl:307
│││││││┌ mapfoldl(f::typeof(identity), op::typeof(Base.add_sum), itr::String) @ Base ./reduce.jl:175
││││││││┌ mapfoldl(f::typeof(identity), op::typeof(Base.add_sum), itr::String; init::Base._InitialValue) @ Base ./reduce.jl:175
│││││││││┌ mapfoldl_impl(f::typeof(identity), op::typeof(Base.add_sum), nt::Base._InitialValue, itr::String) @ Base ./reduce.jl:44
││││││││││┌ foldl_impl(op::Base.BottomRF{typeof(Base.add_sum)}, nt::Base._InitialValue, itr::String) @ Base ./reduce.jl:48
│││││││││││┌ _foldl_impl(op::Base.BottomRF{typeof(Base.add_sum)}, init::Base._InitialValue, itr::String) @ Base ./reduce.jl:62
││││││││││││┌ (::Base.BottomRF{typeof(Base.add_sum)})(acc::Char, x::Char) @ Base ./reduce.jl:86
│││││││││││││┌ add_sum(x::Char, y::Char) @ Base ./reduce.jl:24
││││││││││││││ no matching method found `+(::Char, ::Char)`: (x::Char + y::Char)
│││││││││││││└────────────────────
││││││││││┌ foldl_impl(op::Base.BottomRF{typeof(Base.add_sum)}, nt::Base._InitialValue, itr::String) @ Base ./reduce.jl:49
│││││││││││┌ reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String) @ Base ./reduce.jl:383
││││││││││││┌ reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String, ::Base.HasEltype) @ Base ./reduce.jl:384
│││││││││││││┌ reduce_empty(op::Base.BottomRF{typeof(Base.add_sum)}, ::Type{Char}) @ Base ./reduce.jl:360
││││││││││││││┌ reduce_empty(::typeof(Base.add_sum), ::Type{Char}) @ Base ./reduce.jl:352
│││││││││││││││┌ reduce_empty(::typeof(+), ::Type{Char}) @ Base ./reduce.jl:343
││││││││││││││││ no matching method found `zero(::Type{Char})`: zero(T::Type{Char})
│││││││││││││││└────────────────────
┌ foo(a::String) @ Main ./REPL[14]:3
│ `Main.undefsum` is not defined: r2 = undefsum(a::String)
└────────────────────

# with `target_modules=(@__MODULE__,)`, JET will only report the problems detected within the `@__MODULE__` module:
julia> @report_call target_modules=(@__MODULE__,) foo("julia")
═════ 1 possible error found ═════
┌ foo(a::String) @ Main ./REPL[14]:3
│ `Main.undefsum` is not defined: r2 = undefsum(a::String)
└────────────────────

# with `ignored_modules=(Base,)`, JET will ignore the errors detected within the `Base` module:
julia> @report_call ignored_modules=(Base,) foo("julia")
═════ 1 possible error found ═════
┌ foo(a::String) @ Main ./REPL[14]:3
│ `Main.undefsum` is not defined: r2 = undefsum(a::String)
└────────────────────
```
---
"""
function configured_reports(reports::Vector{InferenceErrorReport};
                            report_config = nothing, target_modules = nothing, ignored_modules = nothing,
                            __jetconfigs...)
    if report_config === nothing
        report_config = ReportConfig(target_modules, ignored_modules)
    end
    return configured_reports(report_config, reports)
end

struct ReportConfig{S,T}
    target_modules::S
    ignored_modules::T
end

struct LastFrameModule; mod::Module; end
struct AnyFrameModule;  mod::Module; end

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
@noinline configured_reports(@nospecialize(x::Any), ::Vector{InferenceErrorReport}) =
    error(lazy"`JET.configured_reports(report_config::$(typeof(x)), reports::Vector{InferenceErrorReport})` is not implemented")

linfomod(linfo::MethodInstance) = (def = linfo.def; isa(def, Method) ? def.module : def)

# UIs
# ===

struct PostProcessor
    actual2virtual::Pair{String,String}
    PostProcessor() = new()
    PostProcessor(::Nothing) = new()
    function PostProcessor((actualmod, virtualmod)::Actual2Virtual)
        new(string(actualmod) => string(virtualmod))
    end
end

# when virtualized, fix virtual module printing based on string manipulation;
# the "actual" modules may not be loaded into this process
function (postprocessor::PostProcessor)(s::AbstractString)
    if isdefined(postprocessor, :actual2virtual)
        actual, virtual = postprocessor.actual2virtual
        s = replace(s, virtual => actual)
        if actual == "Main"
            s = replace(s, "Main." => "")
        end
    end
    return s
end

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

const AnyJETResult = Union{JETCallResult,JETToplevelResult}

# default UI (console)
include("ui/print.jl")
# UI for VSCode
include("ui/vscode.jl")

# generic entries
# ===============

# interactive
# -----------

"""
    analyze_and_report_call!(analyzer::AbstractAnalyzer, f, [types]; jetconfigs...) -> JETCallResult
    analyze_and_report_call!(analyzer::AbstractAnalyzer, tt::Type{<:Tuple}; jetconfigs...) -> JETCallResult
    analyze_and_report_call!(analyzer::AbstractAnalyzer, mi::MethodInstance; jetconfigs...) -> JETCallResult

A generic entry point to analyze a function call with `AbstractAnalyzer`.
Finally returns the analysis result as [`JETCallResult`](@ref).
Note that this is intended to be used by developers of `AbstractAnalyzer` only.
General users should use high-level entry points like [`report_call`](@ref) and [`report_opt`](@ref).
"""
function analyze_and_report_call!(analyzer::AbstractAnalyzer, @nospecialize(f), @nospecialize(types = Base.default_tt(f));
                                  jetconfigs...)
    if f isa Core.OpaqueClosure
        return analyze_and_report_opaque_closure!(analyzer, f, types; jetconfigs...)
    end
    tt = Base.signature_type(f, types)
    return analyze_and_report_call!(analyzer, tt::Type{<:Tuple}; jetconfigs...)
end
function analyze_and_report_call!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple});
                                  jetconfigs...)
    validate_configs(analyzer, jetconfigs)
    result = analyze_gf_by_type!(analyzer, tt)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    source = lazy"$analyzername: $sig"
    return JETCallResult(result, analyzer, source; jetconfigs...)
end
function analyze_and_report_call!(analyzer::AbstractAnalyzer, mi::MethodInstance;
                                  jetconfigs...)
    validate_configs(analyzer, jetconfigs)
    result = analyze_method_instance!(analyzer, mi)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), mi.specTypes))
    source = lazy"$analyzername: $sig"
    return JETCallResult(result, analyzer, source; jetconfigs...)
end

# TODO `analyze_builtin!` ?
function analyze_gf_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}))
    match = find_single_match(tt, analyzer)
    return analyze_method_signature!(analyzer, match.method, match.spec_types, match.sparams)
end

function find_single_match(@nospecialize(tt), analyzer::AbstractAnalyzer)
    match = Base._which(tt; method_table=CC.method_table(analyzer), world=CC.get_inference_world(analyzer), raise=false)
    match === nothing && single_match_error(tt)
    return match
end

@noinline function single_match_error(@nospecialize tt)
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    error(lazy"Could not find single target method for `$sig`")
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

function analyze_and_report_opaque_closure!(analyzer::AbstractAnalyzer, oc::Core.OpaqueClosure, @nospecialize(types);
                                            jetconfigs...)
    validate_configs(analyzer, jetconfigs)
    env = Base.to_tuple_type(Any[Core.Typeof(x) for x in oc.captures])
    tt = Tuple{env, #=sig=#(Base.to_tuple_type(types)::DataType).parameters...}
    mi = specialize_method(oc.source::Method, tt, svec())
    result = analyze_method_instance!(analyzer, mi)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io->Base.show_tuple_as_call(io, Symbol(""), tt))
    source = lazy"$analyzername: $sig"
    return JETCallResult(result, analyzer, source; jetconfigs...)
end

function analyze_method_instance!(analyzer::AbstractAnalyzer, mi::MethodInstance)
    result = InferenceResult(mi)
    result.ci = ci = CC.engine_reserve(analyzer, mi)
    frame = InferenceState(result, #=cache_mode=#:global, analyzer)
    if isnothing(frame)
        CC.engine_reject(analyzer, ci)
        return result
    end
    result = analyze_frame!(analyzer, frame)
    CC.engine_reject(analyzer, ci)
    return result
end

function CC.InferenceState(result::InferenceResult, cache_mode::UInt8,  analyzer::AbstractAnalyzer)
    init_result!(analyzer, result)
    return @invoke InferenceState(result::InferenceResult, cache_mode::UInt8, analyzer::AbstractInterpreter)
end

function analyze_frame!(analyzer::AbstractAnalyzer, frame::InferenceState)
    set_entry!(analyzer, frame.linfo)
    tworld = typeinf_world(analyzer)
    if isnothing(tworld) || !USE_FIXED_WORLD
        CC.typeinf(analyzer, frame)
    else
        Base.invoke_in_world(tworld, CC.typeinf, analyzer, frame)
    end
    return frame.result
end

is_entry(analyzer::AbstractAnalyzer, mi::MethodInstance) = get_entry(analyzer) === mi

# top-level
# ---------

"""
    analyze_and_report_file!(interp::ConcreteInterpreter, filename::AbstractString; jetconfigs...) -> JETToplevelResult

A generic entry point to analyze a file with `interp::ConcreteInterpreter`.
Finally returns the analysis result as [`JETToplevelResult`](@ref).
Note that this is intended to be used by developers of `AbstractAnalyzer` and
`ConcreteInterpreter` only.
General users should use high-level entry points like [`report_file`](@ref).
"""
function analyze_and_report_file!(interp::ConcreteInterpreter, filename::AbstractString,
                                  pkgid::Union{Nothing,PkgId} = nothing;
                                  jetconfigs...)
    jetconfigs = apply_file_config(jetconfigs, filename)
    entrytext = read(filename, String)
    return analyze_and_report_text!(interp, entrytext, filename, pkgid; jetconfigs...)
end

function apply_file_config(jetconfigs, filename::AbstractString)
    isfile(filename) || throw(ArgumentError("$filename doesn't exist"))
    jetconfigs = set_if_missing(jetconfigs, :toplevel_logger, IOContext(stdout, JET_LOGGER_LEVEL => DEFAULT_LOGGER_LEVEL))
    configfile = find_config_file(dirname(abspath(filename)))
    if !isnothing(configfile)
        config = parse_config_file(configfile)
        merge!(jetconfigs, config) # overwrite configurations
        toplevel_logger = get(jetconfigs, :toplevel_logger, nothing)
        with_toplevel_logger(toplevel_logger; filter=≥(JET_LOGGER_LEVEL_INFO)) do @nospecialize(io)
            println(io, lazy"applied configurations in $configfile")
        end
    end
    return jetconfigs
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

When [`report_file`](@ref) or [`watch_file`](@ref) is called, it will look for
`$CONFIG_FILE_NAME` in the directory of the given file, and search _up_ the file tree until
a JET configuration file is (or isn't) found.
When found, the configurations specified in the file will be applied.

A configuration file can specify configurations like:
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
    Configurations specified as keyword arguments have precedence over those specified
    via a configuration file.
"""
parse_config_file(path::AbstractString) = process_config_dict(TOML.parsefile(path))

process_config_dict(configs) = process_config_dict!(kwargs_dict(configs))

function process_config_dict!(config_dict::Dict{Symbol,Any})
    context = get(config_dict, :context, nothing)
    if !isnothing(context)
        isa(context, String) || throw(JETConfigError(
            "`context` should be string of Julia code", :context, context))
        context = Core.eval(Main, trymetaparse(context, :context))
        config_dict[:context] = context
    end
    concretization_patterns = get(config_dict, :concretization_patterns, nothing)
    if !isnothing(concretization_patterns)
        isa(concretization_patterns, Vector{String}) || throw(JETConfigError(
            "`concretization_patterns` should be array of string of Julia expression",
            :concretization_patterns, concretization_patterns))
        concretization_patterns = Any[
            trymetaparse(pat, :concretization_patterns)
            for pat in concretization_patterns]
        config_dict[:concretization_patterns] = concretization_patterns
    end
    toplevel_logger = get(config_dict, :toplevel_logger, nothing)
    if !isnothing(toplevel_logger)
        isa(toplevel_logger, String) || throw(JETConfigError(
            "`toplevel_logger` should be string of Julia code",
            :toplevel_logger, toplevel_logger))
        toplevel_logger = Core.eval(Main, trymetaparse(toplevel_logger, :toplevel_logger))
        config_dict[:toplevel_logger] = toplevel_logger
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

function find_pkgmod(pkg)
    pkgid, _ = find_pkg(pkg)
    pkgmod = get(Base.loaded_modules, pkgid, nothing)
    isnothing(pkgmod) && error(lazy"Package $(pkgid.name) is not loaded.")
    return pkgmod
end

function find_pkg(pkgname::AbstractString)
    pkgenv = @lock Base.require_lock Base.identify_package_env(pkgname)
    isnothing(pkgenv) && error(lazy"Unknown package $pkgname.")
    return pkgenv
end

function find_pkg(::Nothing)
    project = Pkg.project()
    project.ispackage || error(lazy"Active project at $(project.path) is not a package.")
    return find_pkg(project.name)
end

struct SigAnalysisResult
    reports::Vector{InferenceErrorReport}
    codeinst::CodeInstance
end

"""
    analyze_and_report_package!(analyzer::AbstractAnalyzer, package::Module; jetconfigs...) -> JETToplevelResult

A generic entry point to analyze a package with `analyzer::AbstractAnalyzer`.
Finally returns the analysis result as [`JETToplevelResult`](@ref).
Note that this is intended to be used by developers of `AbstractAnalyzer` only.
General users should use high-level entry points like [`report_package`](@ref).
"""
function analyze_and_report_package!(analyzer::AbstractAnalyzer, pkgmod::Module; jetconfigs...)
    pkgid = Base.PkgId(pkgmod)
    haskey(Revise.pkgdatas, pkgid) || Revise.watch_package(pkgid)
    if !haskey(Revise.pkgdatas, pkgid)
        error(lazy"Package $pkgmod is not analyzable.")
    end

    # If Revise hasn't instantiated signatures yet, populate that cache here
    pkgdata = Revise.pkgdatas[pkgid]
    for file in Revise.srcfiles(pkgdata)
        fi = Revise.maybe_parse_from_cache!(pkgdata, file)
        Revise.maybe_extract_sigs!(fi)
    end

    start = time()
    counter, analyzed, cached = Ref(0), Ref(0), Ref(0)
    res = VirtualProcessResult(nothing)
    jetconfigs = set_if_missing(jetconfigs, :toplevel_logger, IOContext(stdout, JET_LOGGER_LEVEL => DEFAULT_LOGGER_LEVEL))
    config = ToplevelConfig(; jetconfigs...)

    # Revise's signature population may execute code, which can increment the world age,
    # so we update to the latest world age here
    newstate = AnalyzerState(AnalyzerState(analyzer); world=Base.get_world_counter())
    analyzer′ = AbstractAnalyzer(analyzer, newstate)
    if analyzer′ isa BasicJETAnalyzer
        analyzer′ = FromDefinitionJETAnalyzer(analyzer′.state, analyzer′.analysis_token, analyzer′.method_table, analyzer′.config)
    end

    n_sigs = 0
    for fi in pkgdata.fileinfos, (_, exsigs) in fi.modexsigs, (_, siginfos) in exsigs
        isnothing(siginfos) && continue
        n_sigs += length(siginfos)
    end
    for fi in pkgdata.fileinfos, (_, exsigs) in fi.modexsigs, (_, siginfos) in exsigs
        isnothing(siginfos) && continue
        for (i, siginfo) in enumerate(siginfos)
            with_toplevel_logger(config) do @nospecialize(io)
                clearline(io)
            end
            counter[] += 1
            inf_world = CC.get_inference_world(analyzer′)
            ext = Revise.get_extended_data(siginfo, :JET)
            if ext !== nothing && ext.data isa SigAnalysisResult
                prev_result = ext.data::SigAnalysisResult
                if prev_result.codeinst.max_world ≥ inf_world ≥ prev_result.codeinst.min_world
                    with_toplevel_logger(config) do @nospecialize(io)
                        (counter[] == n_sigs ? println : print)(io, "Skipped analysis for cached definition ($(counter[])/$n_sigs)")
                    end
                    cached[] += 1
                    reports = prev_result.reports
                    @goto gotreports
                end
            end
            match = Base._which(siginfo.sig;
                method_table = CC.method_table(analyzer′),
                world = inf_world,
                raise = false)
            if match !== nothing
                with_toplevel_logger(config; pre=clearline) do @nospecialize(io)
                    p = (counter[] == n_sigs ? println : print)
                    if jet_logger_level(io) ≥ JET_LOGGER_LEVEL_DEBUG
                        print(io, "Analyzing top-level definition `")
                        Base.show_tuple_as_call(io, Symbol(""), siginfo.sig)
                        p(io, "` (progress: $(counter[])/$n_sigs)")
                    else
                        p(io, "Analyzing top-level definition (progress: $(counter[])/$n_sigs)")
                    end
                end
                result = analyze_method_signature!(analyzer′,
                    match.method, match.spec_types, match.sparams)
                analyzed[] += 1
                reports = get_reports(analyzer′, result)
                siginfos[i] = Revise.replace_extended_data(siginfo, :JET, SigAnalysisResult(reports, result.ci))
                @label gotreports
                append!(res.inference_error_reports, reports)
            else
                with_toplevel_logger(config) do @nospecialize(io)
                    print(io, "Couldn't find a single matching method for the signature `")
                    Base.show_tuple_as_call(io, Symbol(""), siginfo.sig)
                    println(io, "` (progress: $(counter[])/$n_sigs)")
                end
            end
        end
    end

    with_toplevel_logger(config) do @nospecialize(io)
        sec = round(time() - start; digits = 3)
        println(io, "Analyzed all top-level definitions (all: $(counter[]) | analyzed: $(analyzed[]) | cached: $(cached[]) | took: $sec sec)")
    end

    unique!(aggregation_policy(analyzer′), res.inference_error_reports)

    analyzername = nameof(typeof(analyzer))
    pkgname = String(nameof(pkgmod))
    source = lazy"$analyzername: $pkgname"
    return JETToplevelResult(analyzer, res, source; jetconfigs...)
end

"""
    analyze_and_report_text!(interp::ConcreteInterpreter, text::AbstractString,
                             filename::AbstractString = "top-level";
                             jetconfigs...) -> JETToplevelResult

A generic entry point to analyze a top-level code with `interp::ConcreteInterpreter`.
Finally returns the analysis result as [`JETToplevelResult`](@ref).
Note that this is intended to be used by developers of `AbstractAnalyzer` and
`ConcreteInterpreter` only.
General users should use high-level entry points like [`report_text`](@ref).
"""
function analyze_and_report_text!(interp::ConcreteInterpreter, text::AbstractString,
                                  filename::AbstractString = "top-level",
                                  pkgid::Union{Nothing,PkgId} = nothing;
                                  jetconfigs...)
    analyzer = ToplevelAbstractAnalyzer(interp)
    validate_configs(analyzer, jetconfigs)
    config = ToplevelConfig(pkgid; jetconfigs...)
    iworld = interpret_world(interp)
    if isnothing(iworld) || !USE_FIXED_WORLD
        res = virtual_process(interp, text, filename, config)
    else
        res = Base.invoke_in_world(iworld, virtual_process, interp, text, filename, config)
    end
    analyzername = nameof(typeof(analyzer))
    source = lazy"$analyzername: \"$filename\""
    return JETToplevelResult(analyzer, res, source; jetconfigs...)
end

function analyze_and_report_expr!(interp::ConcreteInterpreter, x::Union{JS.SyntaxNode,Expr},
                                  filename::AbstractString = "top-level",
                                  pkgid::Union{Nothing,PkgId} = nothing;
                                  jetconfigs...)
    analyzer = ToplevelAbstractAnalyzer(interp)
    validate_configs(analyzer, jetconfigs)
    config = ToplevelConfig(pkgid; jetconfigs...)
    if x isa Expr
        # HACK build a dummy `SyntaxNode` while passing `overrideex`
        toplevelnode = JS.build_tree(JS.SyntaxNode, JS.parse!(JS.ParseStream(""); rule=:all); filename)
        overrideex = x
    else
        toplevelnode = x
        overrideex = nothing
    end
    iworld = interpret_world(interp)
    if isnothing(iworld) || !USE_FIXED_WORLD
        res = virtual_process(interp, toplevelnode, filename, config; overrideex)
    else
        res = Base.invoke_in_world(iworld, virtual_process, interp, toplevelnode, filename, config; overrideex)
    end
    analyzername = nameof(typeof(analyzer))
    source = lazy"$analyzername: \"$filename\""
    return JETToplevelResult(analyzer, res, source; jetconfigs...)
end

"""
Configurations for "watch" mode.
The configurations will only be active when used with [`watch_file`](@ref).

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
      # re-perform analysis when you make a change to `Base`
      julia> watch_file(yourfile; revise_modules = [Base])
      ```
---
"""
struct WatchConfig
    # Revise configurations
    revise_all::Bool
    revise_modules
    function WatchConfig(; revise_all::Bool = true,
                           revise_modules = nothing,
                           __jetconfigs...)
        return new(revise_all, revise_modules)
    end
end

function watch_file_with_func(func, args...; jetconfigs...)
    try
        return _watch_file_with_func(func, args...; jetconfigs...)
    catch err
        if !(err isa MethodError && err.f === _watch_file_with_func)
            rethrow(err)
        end
        error("Revise.jl is not loaded; load Revise and try again.")
    end
end

struct InsufficientWatches <: Exception
    included_files::Set{String}
end

function _watch_file_with_func(func, args...; jetconfigs...)
    local included_files::Set{String}

    config = WatchConfig(; jetconfigs...)

    included_files = let res = func(args...; jetconfigs...)
        show(res) # XXX use `display` here?
        JET.included_files(res.res)
    end

    interrupted = false
    while !interrupted
        try
            Revise.entr(collect(included_files), config.revise_modules;
                        postpone = true, all = config.revise_all) do
                next_included_files = let res = func(args...; jetconfigs...)
                    show(res) # XXX use `display` here?
                    JET.included_files(res.res)
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
            elseif (isa(err, LoadError) ||
                    (isa(err, ErrorException) && startswith(err.msg, "lowering returned an error")) ||
                    isa(err, Revise.ReviseEvalException))
                continue

            # async errors
            elseif isa(err, CompositeException)
                errs = err.exceptions
                i = findfirst(@nospecialize(e)->isa(e, TaskFailedException), errs)
                if !isnothing(i)
                    tfe = errs[i]::TaskFailedException
                    let res = tfe.task.result
                        if isa(res, InsufficientWatches)
                            included_files = res.included_files
                            continue
                        elseif (isa(res, LoadError) ||
                                (isa(res, ErrorException) && startswith(res.msg, "lowering returned an error")) ||
                                isa(res, Revise.ReviseEvalException))
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

# Test.jl integration
# -------------------

"""
    call_test_ex(funcname::Symbol, testname::Symbol, ex0, __module__, __source__)

An internal utility function to implement a `@test_call`-like macro.
See the implementation of [`@test_call`](@ref).
"""
function call_test_ex(funcname::Symbol, testname::Symbol, ex0, __module__, __source__)
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

    testres, orig_expr = _call_test_ex(funcname, testname, ex0, __module__, __source__)
    isskip, isbroken = (!isnothing(skip) && skip), (!isnothing(broken) && broken)

    return quote
        if $isskip
            Test.record(get_testset(), Broken(:skipped, $orig_expr))
        else
            testres = $testres
            if $isbroken
                if isa(testres, JETTestFailure)
                    testres = Broken($(QuoteNode(testname)), $orig_expr)
                elseif isa(testres, Pass)
                    testres = Error(:test_unbroken, $orig_expr, nothing, nothing, $(QuoteNode(__source__)))
                end
            else
                isa(testres, Pass) || ccall(:jl_breakpoint, Cvoid, (Any,), testres)
            end
            Test.record(get_testset(), testres)
        end
    end
end

function _call_test_ex(funcname::Symbol, testname::Symbol, ex0, __module__, __source__)
    analysis = gen_call_with_extracted_types_and_kwargs(__module__, funcname, ex0)
    orig_expr = QuoteNode(Expr(:macrocall, GlobalRef(@__MODULE__, testname), __source__, ex0...))
    source = QuoteNode(__source__)
    testres = :(try
        result = $analysis
        if length(get_reports(result)) == 0
            Pass($(QuoteNode(testname)), $orig_expr, nothing, nothing, $source)
        else
            JETTestFailure($orig_expr, $source, result)
        end
    catch err
        isa(err, InterruptException) && rethrow()
        Error(:test_error, $orig_expr, err, Base.current_exceptions(), $source)
    end) |> Base.remove_linenums!
    return testres, orig_expr
end

"""
    func_test(func, testname::Symbol, args...; jetconfigs...)

An internal utility function to implement a `test_call`-like function.
See the implementation of [`test_call`](@ref).
"""
function func_test(func, testname::Symbol, @nospecialize(args...);
    broken::Bool = false, skip::Bool = false,
    jetconfigs...)
    source = LineNumberNode(@__LINE__, @__FILE__)
    if isempty(jetconfigs)
        orig_expr = :($func($(args...)))
    else
        kwargs = map(((k,v),)->Expr(:kw, k, v), collect(jetconfigs))
        orig_expr = :($func($(args...); $(kwargs...)))
    end

    if skip
        Test.record(get_testset(), Broken(:skipped, orig_expr))
    else
        testres = try
            result = func(args...; jetconfigs...)
            if length(get_reports(result)) == 0
                Pass(testname, orig_expr, nothing, nothing, source)
            else
                JETTestFailure(orig_expr, source, result)
            end
        catch err
            isa(err, InterruptException) && rethrow()
            Error(:test_error, orig_expr, err, Base.current_exceptions(), source)
        end

        if broken
            if isa(testres, JETTestFailure)
                testres = Broken(testname, orig_expr)
            elseif isa(testres, Pass)
                testres = Error(:test_unbroken, orig_expr, nothing, nothing, source)
            end
        else
            isa(testres, Pass) || ccall(:jl_breakpoint, Cvoid, (Any,), testres)
        end
        Test.record(get_testset(), testres)
    end
end

# NOTE we will just show abstract call stack, and won't show backtrace of actual test executions

struct JETTestFailure <: Result
    orig_expr::Expr
    source::LineNumberNode
    result::Union{JETCallResult,JETToplevelResult}
end

const TEST_INDENTS = "  "

function Base.show(io::IO, t::JETTestFailure)
    printstyled(io, "JET-test failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    println(io, TEST_INDENTS, "Expression: ", t.orig_expr)
    # print abstract call stack, with appropriate indents
    result = repr(t.result, context=io)
    lines = replace(result, '\n'=>string('\n', TEST_INDENTS))
    print(io, TEST_INDENTS, lines)
end

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

# validation
# ==========

const GENERAL_CONFIGURATIONS = Set{Symbol}((
    # general
    :report_config, :target_modules, :ignored_modules,
    # toplevel
    :context, :analyze_from_definitions, :concretization_patterns, :virtualize, :toplevel_logger,
    # ui
    :print_toplevel_success, :print_inference_success, :fullpath, :sourceinfo, :stacktrace_types_limit,
    :vscode_console_output,
    # watch
    :revise_all, :revise_modules))
for (Params, Func) = ((InferenceParams, JETInferenceParams),
                      (OptimizationParams, JETOptimizationParams))
    push!(GENERAL_CONFIGURATIONS, Base.kwarg_decl(only(methods(Func, (Params,))))...)
end

# interface
# =========

@eval JETInterface const DOCUMENTED_NAMES = Symbol[] # will be used in docs/make.jl

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
    AbstractAnalyzer, AnalyzerState, AnalysisToken, ToplevelAbstractAnalyzer,
    valid_configurations, aggregation_policy, typeinf_world, VSCode.vscode_diagnostics_order,
    # ErrorReport API
    InferenceErrorReport, ToplevelErrorReport, copy_report, print_report,
    print_report_message, print_signature, report_color,
    # generic entry points,
    analyze_and_report_call!, call_test_ex, func_test,
    analyze_and_report_file!, analyze_and_report_package!, analyze_and_report_text!,
    # development utilities
    add_new_report!, var"@jetreport")

# builtin analyzers
# =================

include("analyzers/jetanalyzer.jl")
include("analyzers/optanalyzer.jl")

# NOTE Use the fixed world here to make `JETAnalyzer`/`OptAnalyzer` robust against potential invalidations
const JET_TYPEINF_WORLD = Ref{UInt}(typemax(UInt))

# NOTE Use the fixed world here to make `JETConcreteInterpreter` robust against potential invalidations
const JET_INTERPRET_WORLD = Ref{UInt}(typemax(UInt))

# Initialize `JET_TYPEINF_WORLD[]` and `JET_INTERPRET_WORLD[]` within `__init__`:
# Note that precompilation below will use the current world age rather than `typemax(UInt)`,
# since `Base.invoke_in_world` uses the current world age when the given world age is higher than the current one.
push_inithook!() do
    world = Base.get_world_counter()
    JET_TYPEINF_WORLD[] = world
    JET_INTERPRET_WORLD[] = world
end

module __demo__ end

using PrecompileTools
@setup_workload let
    @compile_workload let
        result = @report_call sum("julia")
        show(IOContext(devnull, :color=>true), result)
        result = @report_call rand(String)
        show(IOContext(devnull, :color=>true), result)
        result = @report_opt sum("julia")
        show(IOContext(devnull, :color=>true), result)
        result = @report_opt rand(String)
        show(IOContext(devnull, :color=>true), result)
        report_file(normpath(pkgdir(JET), "demo.jl");
            virtualize=false,
            context=__demo__,
            toplevel_logger=nothing)
    end
end
