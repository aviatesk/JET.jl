# configurations
# --------------

"""
Configurations for abstract interpretation performed by JET.
These configurations will be active for all the entries.

---
- `strict_condition_check::Bool = false` \\
  Enables strict condition check.
  JET reports an error if a condition expression type is "non-boolean". In a case when
  the condition type is `Union`, JET will report if either of union split case is
  non-boolean type, but this can lead to lots of false positive error reports when the
  code is not well-typed, because Julia `Base` defines generic functions that are commonly
  used at a conditional context but also may return "non-boolean" values, e.g.:
  - `!(::Function) -> Function`
  - `!(::Missing) -> Missing`
  - `==(::Missing, ::Any) -> Missing`
  - `==(::Any, ::Missing) -> Missing`
  and thus loosely-typed conditional expression often becomes e.g. `Union{Bool, Missing}`,
  and consequently JET will report it as "non-boolean" type
  (NOTE: in Julia `Missing` is certainly not valid conditional type).
  If this configuration is set to `false`, JET enables an heuristic to avoid those false
  positive error reports and won't report an error if a condition expression type is
  `Union` and either of its union split case is `Function` or `Missing`.

  The effect of this configuration can be described with the following examples:

  * with `strict_condition_check::Bool = false` (default)
    ```julia
    julia> test_f() = Dict('a' => 1, :b => 2) # ::Dict{Any,Int}
    test_f (generic function with 1 method)

    julia> @report_call test_f()
    No errors !
    Dict{Any, Int64}
    ```

  * with `strict_condition_check::Bool = true`
    ```julia
    julia> test_f() = Dict('a' => 1, :b => 2) # ::Dict{Any,Int}
    test_f (generic function with 1 method)

    julia> @report_call strict_condition_check = true test_f()
    ═════ 1 possible error found ═════
    ┌ @ REPL[2]:1 Main.Dict(Main.=>('a', 1), Main.=>(:b, 2))
    │┌ @ dict.jl:125 Base.Dict(ps)
    ││┌ @ dict.jl:129 Base.dict_with_eltype(#308, kv, Base.eltype(kv))
    │││┌ @ abstractdict.jl:539 Base.grow_to!(Base.dict_with_eltype(DT_apply, _5), kv)
    ││││┌ @ dict.jl:145 Base.grow_to!(dest2, itr, st)
    │││││┌ @ dict.jl:159 Base.setindex!(new, v, k)
    ││││││┌ @ dict.jl:383 Base.ht_keyindex2!(h, key)
    │││││││┌ @ dict.jl:328 goto %35 if not Base.isequal(key, Base.getindex(keys, index))
    ││││││││ for 1 of union split cases, non-boolean (Missing) used in boolean context: goto %35 if not Base.isequal(key::Symbol, Base.getindex(keys::Vector{Any}, index::Int64)::Any)::Union{Missing, Bool}
    │││││││└───────────────
    Dict{Any, Int64}
    ```

---
---

You can also configure any of the keyword parameters that `$InferenceParams` or `$OptimizationParams` can take,
e.g. `max_methods::Int = 3`, `union_splitting::Int = 4`.
Listed below are selections of those parameters that can have a potent influence on JET analysis.

---
- `ipo_constant_propagation::Bool = true` \\
  Enables constant propagation in abstract interpretation.
  It is _**highly**_ recommended that you keep this configuration `true` to get reasonable analysis result,
  because constant propagation can cut off lots of false positive errorenous code paths and
  thus produce more accurate and useful analysis results.
---
- `aggressive_constant_propagation::Bool = true` \\
  If `true`, JET will try to do constant propagation more "aggressively".
  It can lead to more accurate analysis as explained above, but also it may incur a performance cost.
  JET by default enables this configuration to get more accurate analysis result.
---
- `unoptimize_throw_blocks::Bool = false` \\
  Turn this on to skip analysis on code blocks that will eventually lead to a `throw` call.
  This configuration improves the analysis performance, but it's better to be turned off
  to get a "proper" analysis result, just because there may be other errors even in those "throw blocks".
---
"""
struct JETAnalysisParams
    strict_condition_check::Bool
    @jetconfigurable function JETAnalysisParams(; strict_condition_check::Bool = false,
                                                  )
        return new(strict_condition_check,
                   )
    end
end

# define these functions just to make them able to accept other JET configrations
JETInferenceParams(; ipo_constant_propagation::Bool        = true,
                     aggressive_constant_propagation::Bool = false,
                     unoptimize_throw_blocks::Bool         = true,
                     max_methods::Int                      = 3,
                     union_splitting::Int                  = 4,
                     apply_union_enum::Int                 = 8,
                     tupletype_depth::Int                  = 3,
                     tuple_splat::Int                      = 32,
                     __jetconfigs...) =
    return InferenceParams(; ipo_constant_propagation,
                             aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             max_methods,
                             union_splitting,
                             apply_union_enum,
                             tupletype_depth,
                             tuple_splat,
                             )
JETOptimizationParams(; inlining::Bool                = inlining_enabled(),
                        inline_cost_threshold::Int    = 100,
                        inline_nonleaf_penalty::Int   = 1000,
                        inline_tupleret_bonus::Int    = 250,
                        inline_error_path_cost::Int   = 20,
                        max_methods::Int              = 3,
                        tuple_splat::Int              = 32,
                        union_splitting::Int          = 4,
                        unoptimize_throw_blocks::Bool = true,
                        __jetconfigs...) =
    return OptimizationParams(; inlining,
                                inline_cost_threshold,
                                inline_nonleaf_penalty,
                                inline_tupleret_bonus,
                                inline_error_path_cost,
                                max_methods,
                                tuple_splat,
                                union_splitting,
                                unoptimize_throw_blocks,
                                )
# # assert here that they create same objects as the original constructors
@assert JETInferenceParams() == InferenceParams()
@assert JETOptimizationParams() == OptimizationParams()

const LOGGER_LEVEL_KEY     = :JET_LOGGER_LEVEL
const INFO_LOGGER_LEVEL    = 0
const DEBUG_LOGGER_LEVEL   = 1
const LOGGER_LEVELS        = Dict(INFO_LOGGER_LEVEL  => :info,
                                  DEBUG_LOGGER_LEVEL => :debug,
                                  )
const DEFAULT_LOGGER_LEVEL = INFO_LOGGER_LEVEL
const LOGGER_LEVELS_DESC   = let
    map(collect(LOGGER_LEVELS)) do (level, desc)
        if level == DEFAULT_LOGGER_LEVEL
            "`$level` (\"$desc\" level, default)"
        else
            "`$level` (\"$desc\" level)"
        end
    end |> (x->join(x,", "))
end
get_logger_level(io::IO)   = get(io, LOGGER_LEVEL_KEY, DEFAULT_LOGGER_LEVEL)::Int

"""
Logging configurations for JET analysis.

---
- `toplevel_logger::Union{Nothing,IO} = nothing` \\
  If `IO` object is given, it will track JET's toplevel analysis.
  Logging level can be specified with `$(repr(LOGGER_LEVEL_KEY))` `IO` property.
  Currently supported logging levels are either of $(LOGGER_LEVELS_DESC).

  Examples:
  * logs into `stdout`
    ```julia
    julia> report_file(filename; toplevel_logger = stdout)
    ```
  * logs into `io::IOBuffer` with "debug" logger level
    ```julia
    julia> report_file(filename; toplevel_logger = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL));
    ```
---
- `inference_logger::Union{Nothing,IO} = nothing` \\
  If `IO` object is given, it will track JET's abstract interpretation routine.
  Logging level can be specified with `$(repr(LOGGER_LEVEL_KEY))` `IO` property.
  Currently supported logging levels are either of $(LOGGER_LEVELS_DESC).

  Examples:
  * logs into `stdout`
    ```julia
    report_call(f, args...; inference_logger = stdout)
    ```
  * logs into `io::IOBuffer` with "debug" logger level
    ```julia
    julia> report_call(f, args...; inference_logger = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL))
    ```
---
!!! tip
    Of course you can specify both `toplevel_logger` and `inference_logger` at the same time like below:
    ```julia
    julia> report_and_watch_file(filename;
                                 toplevel_logger = IOContext(logger_io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL),
                                 inference_logger = inference_io)
    ```
"""
struct JETLogger
    # logger to track toplevel interpretation
    toplevel_logger::Union{Nothing,IO}
    # logger to track abstract interpretation routine
    inference_logger::Union{Nothing,IO}
    @jetconfigurable function JETLogger(; toplevel_logger::Union{Nothing,IO} = nothing,
                                          inference_logger::Union{Nothing,IO} = nothing,
                                          )
        if isa(toplevel_logger, IO)
            @assert get_logger_level(toplevel_logger) in keys(LOGGER_LEVELS) "toplevel_logger's $LOGGER_LEVEL_KEY should be either of $LOGGER_LEVELS_DESC"
        end
        if isa(inference_logger, IO)
            @assert get_logger_level(inference_logger) in keys(LOGGER_LEVELS) "inference_logger's $LOGGER_LEVEL_KEY should be either of $LOGGER_LEVELS_DESC"
        end
        new(toplevel_logger, inference_logger)
    end
end

mutable struct AnalyzerState
    ## native ##

    native::NativeInterpreter

    ## `AbstractAnalyzer` ##

    # additional configurations for abstract interpretation
    analysis_params::JETAnalysisParams

    # key for the JET's global cache
    cache_key::UInt

    # temporal stash to keep reports that are collected within the currently-analyzed frame
    # and should be appended to the caller when returning back to the caller frame next time
    caller_cache::Vector{InferenceErrorReport}

    # temporal stash to keep track of inference caller, to which reconstructed cached reports will be appended
    cacher::Union{Nothing,Pair{Symbol,InferenceResult}}

    ## abstract toplevel execution ##

    # will be used in toplevel analysis (skip inference on actually interpreted statements)
    concretized::BitVector

    # virtual toplevel module
    toplevelmod::Module

    # slots to represent toplevel global variables
    global_slots::Dict{Int,Symbol}

    ## debug ##

    logger::JETLogger

    # records depth of call stack
    depth::Int
end

"""
    mutable struct AnalyzerState
        ...
    end

The mutable object that holds various states that are consumed by all [`AbstractAnalyzer`](@ref)s.

---

    AnalyzerState(analyzer::AbstractAnalyzer) -> AnalyzerState

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState` interface.

A new `AnalyzerState` is supposed to be constructed using [JET configurations](@ref) passed
as keyword arguments `jetconfigs` of the [`NewAnalyzer(; jetconfigs...)`](@ref AbstractAnalyzer)
constructor, and the constructed `AnalyzerState` is usually kept within `NewAnalyzer` itself:
```julia
function NewAnalyzer(; jetconfigs...)
    ...
    state = AnalyzerState(; jetconfigs...)
    return NewAnalyzer(..., state)
end
AnalyzerState(analyzer::NewAnalyzer) = analyzer.state
```
"""
function AnalyzerState(analyzer::Analyzer) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$AnalyzerState(analyzer::$Analyzer) -> $AnalyzerState` interface.
    See the documentation of `$AbstractAnalyzer` and `$AnalyzerState`.
    """)
end

function AbstractAnalyzer(analyzer::Analyzer, state::AnalyzerState) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$AbstractAnalyzer(analyzer::$Analyzer, state::$AnalyzerState) -> $Analyzer` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# define getter/setter methods
for fld in fieldnames(AnalyzerState)
    fld === :cache_key && continue # will be defined later (in order to take in `ReportPass` hash)
    getter = Symbol("get_", fld)
    setter = Symbol("set_", fld, '!')
    @eval (@__MODULE__) @inline $getter(analyzer::AbstractAnalyzer)    = getfield(AnalyzerState(analyzer), $(QuoteNode(fld)))
    @eval (@__MODULE__) @inline $setter(analyzer::AbstractAnalyzer, v) = setfield!(AnalyzerState(analyzer), $(QuoteNode(fld)), v)
end

# constructor for fresh analysis
@jetconfigurable function AnalyzerState(world::UInt     = get_world_counter();
                                        analysis_params = nothing,
                                        inf_params      = nothing,
                                        opt_params      = nothing,
                                        concretized     = _CONCRETIZED,
                                        toplevelmod     = _TOPLEVELMOD,
                                        global_slots    = _GLOBAL_SLOTS,
                                        logger          = nothing,
                                        depth           = 0,
                                        jetconfigs...)
    isnothing(analysis_params) && (analysis_params = JETAnalysisParams(; jetconfigs...))
    isnothing(inf_params)      && (inf_params      = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params)      && (opt_params      = JETOptimizationParams(; jetconfigs...))
    isnothing(logger)          && (logger          = JETLogger(; jetconfigs...))

    return AnalyzerState(#=native::NativeInterpreter=# NativeInterpreter(world; inf_params, opt_params),
                         #=analysis_params::JETAnalysisParams=# analysis_params,
                         #=cache_key::UInt=# get_cache_key_per_config(analysis_params, inf_params),
                         #=caller_cache::Vector{InferenceErrorReport}=# InferenceErrorReport[],
                         #=cacher::Union{Nothing,InferenceResult}=# nothing,
                         #=concretized::BitVector=# concretized,
                         #=toplevelmod::Module=# toplevelmod,
                         #=global_slots::Dict{Int,Symbol}=# global_slots,
                         #=logger::JETLogger=# logger,
                         #=depth::Int=# depth,
                         )
end

# dummies for non-toplevel analysis
module __toplevelmod__ end
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = __toplevelmod__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

# constructor for sequential toplevel JET analysis
function AbstractAnalyzer(analyzer::T, concretized, toplevelmod) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(# update world age to take in newly added methods defined
                             # in a previous toplevel interpretation performed by `ConcreteInterpreter`
                             get_world_counter();
                             analysis_params = JETAnalysisParams(analyzer),
                             inf_params      = InferenceParams(analyzer),
                             opt_params      = OptimizationParams(analyzer),
                             concretized     = concretized, # or construct partial `CodeInfo` from remaining abstract statements ?
                             toplevelmod     = toplevelmod,
                             logger          = JETLogger(analyzer),
                             )
    newanalyzer = AbstractAnalyzer(analyzer, newstate)
    maybe_initialize_caches!(newanalyzer)
    return newanalyzer
end

# constructor to do additional JET analysis in the middle of parent (non-toplevel) interpretation
function AbstractAnalyzer(analyzer::T) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(get_world_counter(analyzer);
                             analysis_params = JETAnalysisParams(analyzer),
                             inf_params      = InferenceParams(analyzer),
                             opt_params      = OptimizationParams(analyzer),
                             logger          = JETLogger(analyzer),
                             depth           = get_depth(analyzer),
                             )
    newanalyzer = AbstractAnalyzer(analyzer, newstate)
    maybe_initialize_caches!(newanalyzer)
    return newanalyzer
end

Base.show(io::IO, analyzer::AbstractAnalyzer) = print(io, "AbstractAnalyzer")
Base.show(io::IO, ::MIME"application/prs.juno.inline", analyzer::AbstractAnalyzer) =
    return analyzer
Base.show(io::IO, ::MIME"application/prs.juno.inline", analyzer::NativeInterpreter) =
    return analyzer

# XXX: this should be upstreamed
function Base.show(io::IO, frame::InferenceState)
    print(io, "InfernceState for ")
    show(io, frame.linfo)
    print(io, " at pc ", frame.currpc, '/', length(frame.src.code))
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", frame::InferenceState) =
    return frame

# InferenceResult
# ---------------

const Reports = Vector{InferenceErrorReport}
const CachedReports = Vector{InferenceErrorReportCache}
const WrappedSource = Union{CodeInfo,OptimizationState,Nothing}

"""
    JETResult

`result::InferenceResult` keeps the result of inference performed by `AbstractInterpreter`,
where `result.src` holds the type-inferred source code.

JET's [`AbstractAnalyzer`](@ref) uses the `result.src` field in a different way, where
`result.src::JETResult` keeps both of error reports that are collected during inference and
the type-inferred source code.

When cached, `JETResult` is transformed into [`JETCachedResult`](@ref).
"""
struct JETResult
    reports::Reports
    wrapped_source::WrappedSource
end

"""
    JETCachedResult

When [`result::JETResult`](@ref JETResult) is being cached, it's transformed into
`cached::JETCachedResult` with its `result.reports::$Reports` converted to `cached.reports::$CachedReports`.
When working with [`AbstractAnalyzer`](@ref), we can expect `codeinf::CodeInstance` to have
the field `codeinf.inferred::JETCachedResult` as far as it's managed by [`JET_CACHE`](@ref).
"""
struct JETCachedResult
    reports::CachedReports
    wrapped_source::WrappedSource
end

const AnyJETResult = Union{JETResult,JETCachedResult}

function set_result!(result::InferenceResult)
    init = JETResult(InferenceErrorReport[], nothing)
    set_result!(result, init)
end
function set_result!(result::InferenceResult, jetresult::JETResult)
    result.src = jetresult
end
function set_source!(result::InferenceResult, source::Union{CodeInfo,OptimizationState,Nothing})
    new = JETResult(get_reports(result), source)
    set_result!(result, new)
end
function set_cached_result!(result::InferenceResult, cache::CachedReports)
    result.src = JETCachedResult(cache, get_source(result.src::JETResult))
end
get_reports((; src)::InferenceResult) = get_reports(src::JETResult)
get_reports(result::JETResult) = result.reports
get_cached_reports((; src)::InferenceResult) = get_cached_reports(src::JETCachedResult)
get_cached_reports(result::JETCachedResult) = result.reports
get_source((; src)::InferenceResult) = get_source(src::AnyJETResult)
get_source(jetresult::AnyJETResult) = jetresult.wrapped_source

"""
    add_new_report!(result::InferenceResult, report::InferenceErrorReport)

Adds new [`report::InferenceErrorReport`](@ref InferenceErrorReport) to `result::InferenceResult`.
`result.src` is supposed to be [`JETResult`](@ref).
"""
add_new_report!(result::InferenceResult, report::InferenceErrorReport) =
    return add_new_report!(get_reports(result), report)
add_new_report!(reports::Reports, report::InferenceErrorReport) =
    (push!(reports, report); return report)

add_cached_report!(caller, cached::InferenceErrorReportCache) =
    return add_new_report!(caller, restore_cached_report(cached))

add_caller_cache!(analyzer::AbstractAnalyzer, report::InferenceErrorReport) =
    return push!(get_caller_cache(analyzer), report)
add_caller_cache!(analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport}) =
    return append!(get_caller_cache(analyzer), reports)

# AbstractInterpreter API
# -----------------------

CC.InferenceParams(analyzer::AbstractAnalyzer)    = InferenceParams(get_native(analyzer))
CC.OptimizationParams(analyzer::AbstractAnalyzer) = OptimizationParams(get_native(analyzer))
CC.get_world_counter(analyzer::AbstractAnalyzer)  = get_world_counter(get_native(analyzer))

# JET only works for runtime inference
CC.lock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing
CC.unlock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing

CC.add_remark!(analyzer::AbstractAnalyzer, sv, s) = ReportPass(analyzer)(NativeRemark, analyzer, sv, s)

CC.may_optimize(analyzer::AbstractAnalyzer)      = true
CC.may_compress(analyzer::AbstractAnalyzer)      = false
CC.may_discard_trees(analyzer::AbstractAnalyzer) = false
CC.verbose_stmt_info(analyzer::AbstractAnalyzer) = false

# branch on https://github.com/JuliaLang/julia/pull/41328
@static if isdefined(CC, :is_stmt_inline)
@doc """
    inlining_policy(analyzer::AbstractAnalyzer, @nospecialize(src), stmt_flag::UInt8) -> source::Any

Implements inlining policy for `AbstractAnalyzer`.
Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
[`JETResult`](@ref) or [`JETCachedResult`](@ref), this implementation just bypasses
their wrapped source to `inlining_policy(::AbstractInterpreter, ::Any, ::UInt8)`.
"""
function CC.inlining_policy(analyzer::AbstractAnalyzer, @nospecialize(src), stmt_flag::UInt8)
    if isa(src, JETResult)
        src = get_source(src)
    elseif isa(src, JETCachedResult)
        src = get_source(src)
    end
    return @invoke CC.inlining_policy(analyzer::AbstractInterpreter, @nospecialize(src), stmt_flag::UInt8)
end
else # @static if isdefined(Compiler, :is_stmt_inline)
@doc """
    inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
    jet_inlining_policy(@nospecialize(src)) -> source::Any

`jet_inlining_policy` implements `Core.Compiler.inlining_policy` for `AbstractAnalyzer`.
Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
[`JETResult`](@ref) or [`JETCachedResult`](@ref), `jet_inlining_policy` bypasses
their wrapped source to `Core.Compiler.default_inlining_policy`.
"""
CC.inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
@inline function jet_inlining_policy(@nospecialize(src))
    if isa(src, JETResult)
        src = get_source(src)
    elseif isa(src, JETCachedResult)
        src = get_source(src)
    end
    return default_inlining_policy(src)
end
end # @static if isdefined(Compiler, :is_stmt_inline)

# AbstractAnalyzer
# ----------------

JETAnalysisParams(analyzer::AbstractAnalyzer) = get_analysis_params(analyzer)

function get_cache_key_per_config(analysis_params::JETAnalysisParams, inf_params::InferenceParams)
    h = @static UInt === UInt64 ? 0xa49bd446c0a5d90e : 0xe45361ac
    h = hash(analysis_params, h)
    h = hash(inf_params, h)
    return h
end

function get_cache_key(analyzer::AbstractAnalyzer)
    h = AnalyzerState(analyzer).cache_key
    h = hash(ReportPass(analyzer), h)
    return h
end

function maybe_initialize_caches!(analyzer::AbstractAnalyzer)
    cache_key = get_cache_key(analyzer)
    if !haskey(JET_CACHE, cache_key)
        JET_CACHE[cache_key] = IdDict{MethodInstance,CodeInstance}()
    end
end

# check if we're in a toplevel module
istoplevel(sv::State)             = istoplevel(sv.linfo)
istoplevel(linfo::MethodInstance) = isa(linfo.def, Module)

is_global_slot(analyzer::AbstractAnalyzer, slot::Int)   = slot in keys(get_global_slots(analyzer))
is_global_slot(analyzer::AbstractAnalyzer, slot::Slot)  = is_global_slot(analyzer, slot_id(slot))
is_global_slot(analyzer::AbstractAnalyzer, sym::Symbol) = sym in values(get_global_slots(analyzer))

JETLogger(analyzer::AbstractAnalyzer) = get_logger(analyzer)

@inline with_toplevel_logger(@nospecialize(f), analyzer::AbstractAnalyzer, @nospecialize(filter = ≥(DEFAULT_LOGGER_LEVEL)); kwargs...) =
    with_logger(f, JETLogger(analyzer).toplevel_logger, filter, "toplevel"; kwargs...)

@inline with_inference_logger(@nospecialize(f), analyzer::AbstractAnalyzer, @nospecialize(filter = ≥(DEFAULT_LOGGER_LEVEL)); kwargs...) =
    with_logger(f, JETLogger(analyzer).inference_logger, filter, "inference"; kwargs...)

@inline function with_logger(
    @nospecialize(f), io::Union{Nothing,IO}, @nospecialize(filter), logger_name;
    @nospecialize(pre = identity))
    isnothing(io) && return
    level = get_logger_level(io)
    filter(level) || return
    pre(io)
    print(io, "[$logger_name-$(LOGGER_LEVELS[level])] ")
    f(io)
end

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` wraps remarks by `NativeInterpreter`.
"remarks" are information that Julia's native compiler emits about how its type inference goes,
and those remarks are less interesting in term of "error checking", so currently any of JET's
pre-defined report passes doesn't make any use of `NativeRemark`.
"""
@reportdef struct NativeRemark <: InferenceErrorReport
    s::String
end
get_msg(::Type{NativeRemark}, analyzer::AbstractAnalyzer, sv, s) = s

# ignores `NativeRemark`s by default
(::SoundBasicPass)(::Type{NativeRemark}, analyzer::AbstractAnalyzer, sv::InferenceState, s) = return

# JETAnalyzer
# -----------

"""
    struct JETAnalyzer <: AbstractAnalyzer

JET.jl's default error analyzer.
"""
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API requirements

# NOTE `@aggressive_constprop` here makes sure `mode` to be propagated as constant
@aggressive_constprop @jetconfigurable function JETAnalyzer(;
    report_pass::Union{Nothing,T} = nothing,
    mode::Symbol                  = :basic,
    # default `InferenceParams` tuning
    aggressive_constant_propagation::Bool = true,
    unoptimize_throw_blocks::Bool         = false,
    # default `OptimizationParams` tuning
    inlining::Bool = false,
    jetconfigs...) where {T<:ReportPass}
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        report_pass = mode === :basic ? BasicPass() :
                      mode === :sound ? SoundPass() :
                      throw(ArgumentError("`mode` configuration should be either of `:basic` or `:sound`"))
    end
    # NOTE we always disable inlining, because:
    # - our current strategy to find undefined local variables and uncaught `throw` calls assumes un-inlined frames
    # - the cost for inlining isn't necessary for JETAnalyzer
    @assert !inlining "inlining should be disabled for JETAnalyzer"
    state = AnalyzerState(; aggressive_constant_propagation,
                            unoptimize_throw_blocks,
                            inlining,
                            jetconfigs...)
    return JETAnalyzer(report_pass, state)
end
AnalyzerState(analyzer::JETAnalyzer) =
    return analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) =
    return JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) =
    return analyzer.report_pass
