# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type of analyzers that are built on top of [JET's analyzer framework](@ref AbstractAnalyzer-Framework).

When a new type `NewAnalyzer` implements the `AbstractAnalyzer` interface, it should be declared
as subtype of `AbstractAnalyzer`, and is expected to the following interfaces:

---
1. `NewAnalyzer(; jetconfigs...) -> NewAnalyzer`: \\
   Constructs new analyzer given [JET configurations](@ref) passed as `jetconfigs`.
---
2. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`: \\
   Returns the [`AnalyzerState`](@ref) for `analyzer::NewAnalyzer`.
---
3. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`: \\
   Constructs an new `NewAnalyzer` instance in the middle of JET's [top-level analysis](@ref toplevel)
   or [abstract interpretation](@ref abstractinterpret), given the previous
   `analyzer::NewAnalyzer` and [`state::AnalyzerState`](@ref AnalyzerState).
---
4. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`: \\
   Returns [`ReportPass`](@ref) used for `analyzer::NewAnalyzer`.
---
5. `get_cache_key(analyzer::NewAnalyzer) -> cache_key::UInt`: \\
   Returns [`cache_key::UInt`](@ref get_cache_key) used for `analyzer::NewAnalyzer`.
---

See also [`AnalyzerState`](@ref), [`ReportPass`](@ref) and [`get_cache_key`](@ref).

# Example

JET.jl defines its default error analyzer `JETAnalyzer <: AbstractAnalyzer` as the following
(modified a bit for the sake of simplicity):
```julia
# the default error analyzer for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API requirements

function JETAnalyzer(;
    report_pass::ReportPass = BasicPass(),
    jetconfigs...)
    return JETAnalyzer(report_pass,
                       AnalyzerState(; jetconfigs...),
                       )
end
AnalyzerState(analyzer::JETAnalyzer) =
    return analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) =
    return JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) =
    return analyzer.report_pass
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

# interface 1
# -----------
# 1. `NewAnalyzer(; jetconfigs...) -> NewAnalyzer`

function (::Type{Analyzer})(; jetconfigs...) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$Analyzer(; jetconfigs...) -> $Analyzer` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# interface 2
# -----------
# 2. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`

const LOGGER_LEVEL_KEY = :JET_LOGGER_LEVEL
const INFO_LOGGER_LEVEL = 0
const DEBUG_LOGGER_LEVEL = 1
const LOGGER_LEVELS = Dict(INFO_LOGGER_LEVEL  => :info,
                           DEBUG_LOGGER_LEVEL => :debug,
                           )
const DEFAULT_LOGGER_LEVEL = INFO_LOGGER_LEVEL
const LOGGER_LEVELS_DESC = let
    descs = map(collect(LOGGER_LEVELS)) do (level, desc)
        if level == DEFAULT_LOGGER_LEVEL
            "`$level` (\"$desc\" level, default)"
        else
            "`$level` (\"$desc\" level)"
        end
    end
    join(descs, ", ")
end
get_logger_level(@nospecialize io::IO) = get(io, LOGGER_LEVEL_KEY, DEFAULT_LOGGER_LEVEL)::Int

"""
Logging configurations for JET analysis.

---
- `toplevel_logger::Union{Nothing,IO} = nothing` \\
  If `IO` object is given, it will track JET's toplevel analysis.
  Logging level can be specified with `$(repr(LOGGER_LEVEL_KEY))` `IO` property.
  Currently supported logging levels are either of $(LOGGER_LEVELS_DESC).

  Examples:
  * logs into `stdout`
    ```julia-repl
    julia> report_file(filename; toplevel_logger = stdout)
    ```
  * logs into `io::IOBuffer` with "debug" logger level
    ```julia-repl
    julia> report_file(filename; toplevel_logger = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL));
    ```
---
- `inference_logger::Union{Nothing,IO} = nothing` \\
  If `IO` object is given, it will track JET's abstract interpretation routine.
  Logging level can be specified with `$(repr(LOGGER_LEVEL_KEY))` `IO` property.
  Currently supported logging levels are either of $(LOGGER_LEVELS_DESC).

  Examples:
  * logs into `stdout`
    ```julia-repl
    julia> report_call(f, args...; inference_logger = stdout)
    ```
  * logs into `io::IOBuffer` with "debug" logger level
    ```julia-repl
    julia> report_call(f, args...; inference_logger = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL))
    ```
---
!!! tip
    Of course you can specify both `toplevel_logger` and `inference_logger` at the same time like below:
    ```julia-repl
    julia> report_and_watch_file(filename;
                                 toplevel_logger = IOContext(logger_io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL),
                                 inference_logger = inference_io)
    ```
"""
struct JETLogger
    # logger to track toplevel interpretation
    toplevel_logger # ::Union{Nothing,IO}
    # logger to track abstract interpretation routine
    inference_logger # ::Union{Nothing,IO}
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
mutable struct AnalyzerState
    ## native ##

    native::NativeInterpreter

    ## `AbstractAnalyzer` ##

    # identity hash key for this state
    param_key::UInt

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

    # some `AbstractAnalyzer` may want to use this inforamion
    entry::Union{Nothing,MethodInstance}

    ## debug ##

    logger::JETLogger

    # records depth of call stack
    depth::Int
end

# define shortcut getter/setter methods for `AbstractAnalyzer`s
for fld in fieldnames(AnalyzerState)
    getter = Symbol("get_", fld)
    setter = Symbol("set_", fld, '!')
    @eval (@__MODULE__) @inline $getter(analyzer::AbstractAnalyzer)    = getfield(AnalyzerState(analyzer), $(QuoteNode(fld)))
    @eval (@__MODULE__) @inline $setter(analyzer::AbstractAnalyzer, v) = setfield!(AnalyzerState(analyzer), $(QuoteNode(fld)), v)
end

# constructor for fresh analysis
@jetconfigurable function AnalyzerState(world::UInt  = get_world_counter();
                                        inf_params   = nothing,
                                        opt_params   = nothing,
                                        concretized  = _CONCRETIZED,
                                        toplevelmod  = _TOPLEVELMOD,
                                        global_slots = _GLOBAL_SLOTS,
                                        logger       = nothing,
                                        depth        = 0,
                                        jetconfigs...)
    isnothing(inf_params) && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params) && (opt_params = JETOptimizationParams(; jetconfigs...))
    isnothing(logger)     && (logger     = JETLogger(; jetconfigs...))

    native       = NativeInterpreter(world; inf_params, opt_params)
    param_key    = get_param_key(inf_params)
    caller_cache = InferenceErrorReport[]

    return AnalyzerState(#=native::NativeInterpreter=# native,
                         #=param_key::UInt=# param_key,
                         #=caller_cache::Vector{InferenceErrorReport}=# caller_cache,
                         #=cacher::Union{Nothing,InferenceResult}=# nothing,
                         #=concretized::BitVector=# concretized,
                         #=toplevelmod::Module=# toplevelmod,
                         #=global_slots::Dict{Int,Symbol}=# global_slots,
                         #=entry::Union{Nothing,MethodInstance}=# nothing,
                         #=logger::JETLogger=# logger,
                         #=depth::Int=# depth,
                         )
end

# dummies for non-toplevel analysis
module __toplevelmod__ end
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = __toplevelmod__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

# wrappers of `InferenceParams` and `OptimizationParams` that can accept JET configrations

"""
Configurations for abstract interpretation performed by JET.
These configurations will be active for all the entries.

You can configure any of the keyword parameters that `$InferenceParams` or `$OptimizationParams`
can take, e.g. `max_methods::Int = 3`, `union_splitting::Int = 4`.
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
@static if isdefined(CC, :mark_throw_blocks!)
    JETOptimizationParams(; inlining::Bool                = inlining_enabled(),
                            inline_cost_threshold::Int    = 100,
                            inline_nonleaf_penalty::Int   = 1000,
                            inline_tupleret_bonus::Int    = 250,
                            inline_error_path_cost::Int   = 20,
                            max_methods::Int              = 3,
                            tuple_splat::Int              = 32,
                            union_splitting::Int          = 4,
                            __jetconfigs...) =
        return OptimizationParams(; inlining,
                                    inline_cost_threshold,
                                    inline_nonleaf_penalty,
                                    inline_tupleret_bonus,
                                    inline_error_path_cost,
                                    max_methods,
                                    tuple_splat,
                                    union_splitting,
                                    )
else # @static if isdefined(CC, :mark_throw_blocks!)
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
end # @static if isdefined(CC, :mark_throw_blocks!)
# # assert here that they create same objects as the original constructors
@assert JETInferenceParams() == InferenceParams()
@assert JETOptimizationParams() == OptimizationParams()

function AnalyzerState(analyzer::Analyzer) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$AnalyzerState(analyzer::$Analyzer) -> $AnalyzerState` interface.
    See the documentation of `$AbstractAnalyzer` and `$AnalyzerState`.
    """)
end

# interface 3
# -----------
# 3. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`

function AbstractAnalyzer(analyzer::Analyzer, state::AnalyzerState) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$AbstractAnalyzer(analyzer::$Analyzer, state::$AnalyzerState) -> $Analyzer` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# constructor for additional JET analysis in the middle of parent (non top-level) abstractinterpret
function AbstractAnalyzer(analyzer::T) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(get_world_counter(analyzer);
                             inf_params      = InferenceParams(analyzer),
                             opt_params      = OptimizationParams(analyzer),
                             logger          = JETLogger(analyzer),
                             depth           = get_depth(analyzer),
                             )
    newanalyzer = AbstractAnalyzer(analyzer, newstate)
    maybe_initialize_caches!(newanalyzer)
    return newanalyzer
end

# constructor for sequential toplevel JET analysis
function AbstractAnalyzer(analyzer::T, concretized, toplevelmod) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(# update world age to take in newly added methods defined
                             # in a previous toplevel interpretation performed by `ConcreteInterpreter`
                             get_world_counter();
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

# interface 4
# -----------
# 4. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`

"""
    abstract type ReportPass end

An interface type that represents `AbstractAnalyzer`'s report pass.
`analyzer::AbstractAnalyzer` injects report passes using the `(::ReportPass)(::Type{InferenceErrorReport}, ::AbstractAnalyzer, state, ...)`
interface, which provides a flexible and efficient layer to configure the analysis done by `AbstractAnalyzer`.

---

    ReportPass(analyzer::AbstractAnalyzer) -> ReportPass

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `ReportPass(analyzer::NewAnalyzer) -> ReportPass` interface.

`ReportPass` allows `NewAnalyzer` to provide a very flexible configuration layer for `NewAnalyzer`'s analysis;
an user can define their own `ReportPass` to control how `NewAnalyzer` collects report errors
while still using the analysis routine implemented by `NewAnalyzer`.

# Example

For example, [`JETAnalyzer`](@ref) accepts a custom `ReportPass` passed as [JET configurations](@ref)
(see the example documentation of [`AbstractAnalyzer`](@ref) for details).
And we can setup a custom report pass `IgnoreAllExceptGlobalUndefVar`, that ignores all the
reports that are otherwise collected by `JETAnalyzer` except `GlobalUndefVarErrorReport`:
```julia
# custom report pass that ignores all the reports except `GlobalUndefVarErrorReport`
struct IgnoreAllExceptGlobalUndefVar <: ReportPass end

# ignores all the reports analyzed by `JETAnalyzer`
(::IgnoreAllExceptGlobalUndefVar)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return

# forward to `BasicPass` to collect `GlobalUndefVarErrorReport`
function (::IgnoreAllExceptGlobalUndefVar)(::Type{GlobalUndefVarErrorReport}, @nospecialize(args...))
    BasicPass()(GlobalUndefVarErrorReport, args...)
end

no_method_error()    = 1 + "1"
undef_global_error() = undefvar
report_call(; report_pass=IgnoreAllExceptGlobalUndefVar()) do
    if rand(Bool)
        return no_method_error()    # "no matching method found" error report won't be reported here
    else
        return undef_global_error() # "variable undefvar is not defined" error report will be reported
    end
end
```
"""
abstract type ReportPass end

function ReportPass(::Analyzer) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$ReportPass(analyzer::$Analyzer) -> $ReportPass` interface.
    See the documentation of `$AbstractAnalyzer` and `$ReportPass`.
    """)
end
# some specific reports are necessary to be collected during `AbstractAnalyzer`'s core routine
# and ignore them by default (analyzer can opt-in to collect them by overloading this with
# their own report pass)
# otherwise, it means malformed report pass call, and we should inform users of it
function (rp::ReportPass)(T, @nospecialize(args...))
    if !(T === NativeRemark ||
         T === InvalidConstantRedefinition ||
         T === InvalidConstantDeclaration)
        throw(MethodError(rp, (T, args...)))
    end
    return false
end

# interface 5
# -----------
# 5. `get_cache_key(analyzer::NewAnalyzer) -> cache_key::UInt`

"""
    get_cache_key(analyzer::AbstractAnalyzer) -> cache_key::UInt

Returns the cache key for this `analyzer::AbstractAnalyzer`.
`AbstractAnalyzer`s that have different cache keys will use different cache so that their
analysis results are completely separated.

See also [`JET_CACHE`](@ref).
"""
function get_cache_key(::Analyzer) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$get_cache_key(analyzer::$Analyzer) -> UInt` interface.
    See the documentation of `$AbstractAnalyzer` and `$get_cache_key`.
    """)
end

# InferenceResult
# ===============
# define how AbstractAnalyzer manages `InferenceResult`

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

# AbstractInterpreter
# ===================
# provide default implementations for the API requirements

CC.InferenceParams(analyzer::AbstractAnalyzer)    = InferenceParams(get_native(analyzer))
CC.OptimizationParams(analyzer::AbstractAnalyzer) = OptimizationParams(get_native(analyzer))
CC.get_world_counter(analyzer::AbstractAnalyzer)  = get_world_counter(get_native(analyzer))

# JET only works for runtime inference
CC.lock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing
CC.unlock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing

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
get_msg(::Type{NativeRemark}, sv, s) = s
CC.add_remark!(analyzer::AbstractAnalyzer, sv, s) = ReportPass(analyzer)(NativeRemark, sv, s) # ignored by default

CC.may_optimize(analyzer::AbstractAnalyzer)      = true
CC.may_compress(analyzer::AbstractAnalyzer)      = false
CC.may_discard_trees(analyzer::AbstractAnalyzer) = false
CC.verbose_stmt_info(analyzer::AbstractAnalyzer) = false

# branch on https://github.com/JuliaLang/julia/pull/41328 & https://github.com/JuliaLang/julia/pull/42082
@static if IS_AFTER_42082
@doc """
    inlining_policy(
        analyzer::AbstractAnalyzer, @nospecialize(src), stmt_flag::UInt8,
        mi::MethodInstance, argtypes::Argtypes) -> source::Any

Implements inlining policy for `AbstractAnalyzer`.
Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
[`JETResult`](@ref) or [`JETCachedResult`](@ref), this implementation just forwards
their wrapped source to `inlining_policy(::AbstractInterpreter, ::Any, ::UInt8)`.
"""
function CC.inlining_policy(
    analyzer::AbstractAnalyzer, @nospecialize(src), stmt_flag::UInt8,
    mi::MethodInstance, argtypes::Argtypes)
    if isa(src, JETResult)
        src = get_source(src)
    elseif isa(src, JETCachedResult)
        src = get_source(src)
    end
    return @invoke CC.inlining_policy(
        analyzer::AbstractInterpreter, @nospecialize(src), stmt_flag::UInt8,
        mi::MethodInstance, argtypes::Argtypes)
end
else # @static if IS_AFTER_42082
@doc """
    inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
    jet_inlining_policy(@nospecialize(src)) -> source::Any

`jet_inlining_policy` implements `Core.Compiler.inlining_policy` for `AbstractAnalyzer`.
Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
[`JETResult`](@ref) or [`JETCachedResult`](@ref), `jet_inlining_policy` forwards
their wrapped source to `Core.Compiler.default_inlining_policy`.
"""
CC.inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
@inline function jet_inlining_policy(@nospecialize(src))
    if isa(src, JETResult)
        src = get_source(src)
    elseif isa(src, JETCachedResult)
        src = get_source(src)
    end
    return CC.default_inlining_policy(src)
end
end # @static if IS_AFTER_42082

# AbstractAnalyzer
# ================
# AbstractAnalyzer specific APIs

function get_param_key(inf_params::InferenceParams)
    h = @static UInt === UInt64 ? 0xa49bd446c0a5d90e : 0xe45361ac
    h = hash(inf_params, h)
    return h
end

function maybe_initialize_caches!(analyzer::AbstractAnalyzer)
    cache_key = get_cache_key(analyzer)
    if !haskey(JET_CACHE, cache_key)
        JET_CACHE[cache_key] = IdDict{MethodInstance,CodeInstance}()
    end
end

is_global_slot(analyzer::AbstractAnalyzer, slot::Int)   = slot in keys(get_global_slots(analyzer))
is_global_slot(analyzer::AbstractAnalyzer, slot::Slot)  = is_global_slot(analyzer, slot_id(slot))
is_global_slot(analyzer::AbstractAnalyzer, sym::Symbol) = sym in values(get_global_slots(analyzer))

JETLogger(analyzer::AbstractAnalyzer) = get_logger(analyzer)

@nospecialize

@inline function with_toplevel_logger(f, analyzer::AbstractAnalyzer, args...; kwargs...)
    io = JETLogger(analyzer).toplevel_logger
    isa(io, IO) && with_logger(f, io, :toplevel, args...; kwargs...)
end
@inline function with_inference_logger(f, analyzer::AbstractAnalyzer, args...; kwargs...)
    io = JETLogger(analyzer).inference_logger
    isa(io, IO) && with_logger(f, io, :inference, args...; kwargs...)
end
function with_logger(f, io::IO, mode, filter = ≥(DEFAULT_LOGGER_LEVEL); pre = identity)
    level = get_logger_level(io)
    filter(level) || return
    pre(io)
    print(io, "[$mode-$(LOGGER_LEVELS[level])] ")
    f(io)
end

@specialize

"""
    aggregation_policy(analyzer::AbstractAnalyzer)

Defines how `analyzer` aggregates [`InferenceErrorReport`](@ref)s.
Defaults to `default_aggregation_policy`.

---

    default_aggregation_policy(report::InferenceErrorReport) -> DefaultReportIdentity

Returns the default identity of `report::InferenceErrorReport`, where `DefaultReportIdentity`
aggregates reports based on "error location" of each `report`.
`DefaultReportIdentity` aggregates `InferenceErrorReport`s aggressively in a sense that it
ignores the identity of error point's `MethodInstance`, under the assumption that errors are
identical as far as they're collected at the same file and line.
"""
aggregation_policy(::AbstractAnalyzer) = default_aggregation_policy
function default_aggregation_policy(@nospecialize(report#=::InferenceErrorReport=#))
    return DefaultReportIdentity(
        typeof(report)::DataType,
        report.sig,
        # VirtualFrameNoLinfo(first(report.vst)),
        VirtualFrameNoLinfo(last(report.vst)),
        )
end
@withmixedhash struct VirtualFrameNoLinfo
    file::Symbol
    line::Int
    sig::Signature
    # linfo::MethodInstance # ignore the idenity of `MethodInstace`
    VirtualFrameNoLinfo(vf::VirtualFrame) = new(vf.file, vf.line, vf.sig)
end
@withmixedhash struct DefaultReportIdentity
    T::DataType
    sig::Signature
    # entry_frame::VirtualFrameNoLinfo
    error_frame::VirtualFrameNoLinfo
end
