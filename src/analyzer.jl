# configurations
# --------------

"""
Configurations for JET analysis.
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
"""
struct JETAnalysisParams
    strict_condition_check::Bool
    @jetconfigurable function JETAnalysisParams(; strict_condition_check::Bool  = false,
                                                  )
        return new(strict_condition_check,
                   )
    end
end

"""
Configurations for Julia's native type inference routine.
These configurations will be active for all the entries.

You can specify all the keyword parameters of `Core.Compiler.InferenceParams`, e.g.
  `max_methods::Int = 3`, `union_splitting::Int = 4`.
Listed here are selections of those parameters that can have a potent influence on JET analysis.

---
- `ipo_constant_propagation::Bool = true` \\
  Enables constant propagation in abstract interpretation.
  It is _**highly**_ recommended that you keep this configuration `true` to get reasonable analysis,
  because constant propagation can cut off lots of false positive errorenous code paths and
  thus lead to more accurate and useful analysis results.
---
- `aggressive_constant_propagation::Bool = true` \\
  If `true`, JET will try to do constant propagation more "aggressively".
  As explained above, it can lead to more accurate analysis, but also lead to worse analysis
  performance at the cost of that.
---
- `unoptimize_throw_blocks::Bool = false` \\
  Turn this on to skip analysis on code blocks that will eventually lead to a `throw` call.
  This configuration may improve the analysis performance, but it's better to be turned off
    for JET analysis, because there may be other errors even in those code blocks.
"""
@jetconfigurable JETInferenceParams(; ipo_constant_propagation::Bool        = true,
                                      aggressive_constant_propagation::Bool = true,
                                      unoptimize_throw_blocks::Bool         = false,
                                      max_methods::Int                      = 3,
                                      union_splitting::Int                  = 4,
                                      apply_union_enum::Int                 = 8,
                                      tupletype_depth::Int                  = 3,
                                      tuple_splat::Int                      = 32,
                                      ) =
    return InferenceParams(; ipo_constant_propagation,
                             aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             max_methods,
                             union_splitting,
                             apply_union_enum,
                             tupletype_depth,
                             tuple_splat,
                             )

# here we just try to sync `OptimizationParams` with  `InferenceParams`
# (the same JET configurations will be passed on here)
JETOptimizationParams(; # inlining::Bool                = inlining_enabled(),
                        # inline_cost_threshold::Int    = 100,
                        # inline_nonleaf_penalty::Int   = 1000,
                        # inline_tupleret_bonus::Int    = 250,
                        # inline_error_path_cost::Int   = 20,
                        max_methods::Int              = 3,
                        tuple_splat::Int              = 32,
                        union_splitting::Int          = 4,
                        unoptimize_throw_blocks::Bool = false,
                        _jetconfigs...) =
    # NOTE we always disable inlining, because our current strategy to find undefined
    # local variable assumes un-inlined frames
    # TODO enable inlining to get better JET analysis performance ?
    # XXX but the self-profiling with `inlining = true` showed performance regression ...
    return OptimizationParams(; inlining = false,
                                # inline_cost_threshold,
                                # inline_nonleaf_penalty,
                                # inline_tupleret_bonus,
                                # inline_error_path_cost,
                                max_methods,
                                tuple_splat,
                                union_splitting,
                                unoptimize_throw_blocks,
                                )

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
    end |> Fix2(join, ", ")
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
    analyze_call(f, args...; inference_logger = stdout)
    ```
  * logs into `io::IOBuffer` with "debug" logger level
    ```julia
    julia> analyze_call(f, args...; inference_logger = IOContext(io, $(repr(LOGGER_LEVEL_KEY)) => $DEBUG_LOGGER_LEVEL))
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

# XXX we need to consider world range ?
struct AnalysisResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    overridden_by_const::CC.BitVector
    cache::Vector{InferenceErrorReportCache}
end

mutable struct AnalyzerState
    ### native ###

    native::NativeInterpreter

    ### JET.jl specific ###

    ## general ##

    # additional configurations for abstract interpretation
    analysis_params::JETAnalysisParams

    # key for the JET's global cache
    cache_key::UInt

    # reports found so far
    reports::Vector{InferenceErrorReport}

    # stashes `UncaughtExceptionReport`s that are not caught so far
    uncaught_exceptions::Vector{UncaughtExceptionReport}

    # keeps reports that should be updated when returning back the parent frame (i.e. the next time we get back to inter-procedural context)
    to_be_updated::Set{InferenceErrorReport}

    # keeps track of the current inference frame (needed for report cache reconstruction)
    current_frame::Union{Nothing,InferenceState}

    # local report cache for constant analysis
    cache::Vector{AnalysisResult}

    ## virtual toplevel execution ##

    # will be used in toplevel analysis (skip inference on actually interpreted statements)
    concretized::BitVector

    # virtual toplevel module
    toplevelmod::Module

    # toplevel modules concretized by JET (only active within sequential toplevel analysis)
    toplevelmods::Set{Module}

    # slots to represent toplevel global variables
    global_slots::Dict{Int,Symbol}

    ## debug ##

    logger::JETLogger

    # records depth of call stack
    depth::Int

    function AnalyzerState(native::NativeInterpreter, args...)
        @assert !native.opt_params.inlining "inlining should be disabled for AbstractAnalyzer"
        new(native, args...)
    end
end

# define getter methods
for fld in fieldnames(AnalyzerState)
    fld === :cache_key && continue # will be defined later (in order to take in `ReportPass` hash)
    fn = Symbol("get_", fld)
    @eval (@__MODULE__) @inline $fn(analyzer::AbstractAnalyzer) =
        getproperty(AnalyzerState(analyzer), $(QuoteNode(fld)))
end

# constructor for fresh analysis
@jetconfigurable function AnalyzerState(world::UInt     = get_world_counter();
                                        analysis_params = nothing,
                                        current_frame   = nothing,
                                        cache           = AnalysisResult[],
                                        inf_params      = nothing,
                                        opt_params      = nothing,
                                        concretized     = _CONCRETIZED,
                                        toplevelmod     = _TOPLEVELMOD,
                                        toplevelmods    = _TOPLEVELMODS,
                                        global_slots    = _GLOBAL_SLOTS,
                                        logger          = nothing,
                                        depth           = 0,
                                        jetconfigs...)
    isnothing(analysis_params) && (analysis_params = JETAnalysisParams(; jetconfigs...))
    isnothing(inf_params)      && (inf_params      = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params)      && (opt_params      = JETOptimizationParams(; jetconfigs...))
    isnothing(logger)          && (logger          = JETLogger(; jetconfigs...))

    return AnalyzerState(NativeInterpreter(world; inf_params, opt_params),
                         analysis_params,
                         get_cache_key_per_config(analysis_params, inf_params),
                         InferenceErrorReport[],
                         UncaughtExceptionReport[],
                         Set{InferenceErrorReport}(),
                         current_frame,
                         cache,
                         concretized,
                         toplevelmod,
                         toplevelmods,
                         global_slots,
                         logger,
                         depth,
                         )
end

# dummies for non-toplevel analysis
module __toplevelmod__ end
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = __toplevelmod__
const _TOPLEVELMODS = Set{Module}()
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
                             toplevelmods    = push!(get_toplevelmods(analyzer), toplevelmod),
                             logger          = JETLogger(analyzer),
                             )
    newinterp = AbstractAnalyzer(analyzer, newstate)
    maybe_initialize_caches!(analyzer)
    return newinterp
end

# constructor to do additional JET analysis in the middle of parent (non-toplevel) interpretation
function AbstractAnalyzer(analyzer::T) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(get_world_counter(analyzer);
                             analysis_params = JETAnalysisParams(analyzer),
                             current_frame   = get_current_frame(analyzer),
                             cache           = get_cache(analyzer),
                             inf_params      = InferenceParams(analyzer),
                             opt_params      = OptimizationParams(analyzer),
                             logger          = JETLogger(analyzer),
                             depth           = get_depth(analyzer),
                             )
    newinterp = AbstractAnalyzer(analyzer, newstate)
    maybe_initialize_caches!(analyzer)
    return newinterp
end

function Base.show(io::IO, analyzer::AbstractAnalyzer)
    rn = length(get_reports(analyzer))
    en = length(get_uncaught_exceptions(analyzer))
    print(io, "AbstractAnalyzer with $(rn) reports and $(en) uncaught exceptions")
    frame = get_current_frame(analyzer)
    if !isnothing(frame)
        print(io, " at ")
        show(io, frame.linfo)
    end
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", analyzer::AbstractAnalyzer) =
    return analyzer
Base.show(io::IO, ::MIME"application/prs.juno.inline", analyzer::NativeInterpreter) =
    return analyzer

# XXX: this should be upstreamed
function Base.show(io::IO, frame::InferenceState)
    print(io, "InfernceState for ")
    show(io, frame.linfo)
    print(io, " with currpc ", frame.currpc, '/', length(frame.src.code))
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", frame::InferenceState) =
    return frame

# AbstractInterpreter API
# -----------------------

CC.InferenceParams(analyzer::AbstractAnalyzer)    = InferenceParams(get_native(analyzer))
CC.OptimizationParams(analyzer::AbstractAnalyzer) = OptimizationParams(get_native(analyzer))
CC.get_world_counter(analyzer::AbstractAnalyzer)  = get_world_counter(get_native(analyzer))

# JET only works for runtime inference
CC.lock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing
CC.unlock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing

CC.add_remark!(analyzer::AbstractAnalyzer, sv, s) = report_pass!(NativeRemark, analyzer, sv, s)

CC.may_optimize(analyzer::AbstractAnalyzer)      = true
CC.may_compress(analyzer::AbstractAnalyzer)      = false
CC.may_discard_trees(analyzer::AbstractAnalyzer) = false

# AbstractAnalyzer
# ----------------

JETAnalysisParams(analyzer::AbstractAnalyzer) = get_analysis_params(analyzer)

@inline report_pass!(T::Type{<:InferenceErrorReport}, analyzer::AbstractAnalyzer, linfo::Union{InferenceState,MethodInstance}, @nospecialize(spec_args...)) =
    ReportPass(analyzer)(T, analyzer, linfo, spec_args...)

function report!(T::Type{<:InferenceErrorReport}, analyzer::AbstractAnalyzer,  @nospecialize(spec_args...))
    push!(get_reports(analyzer), T(analyzer, spec_args...))
end
function report!(T::Type{UncaughtExceptionReport}, analyzer::AbstractAnalyzer,  @nospecialize(spec_args...))
    push!(get_uncaught_exceptions(analyzer), T(analyzer, spec_args...))
end

function restore_cached_report!(cache::InferenceErrorReportCache, analyzer::AbstractAnalyzer)
    report = restore_cached_report(cache)
    if isa(report, UncaughtExceptionReport)
        push!(get_uncaught_exceptions(analyzer), report)
    else
        push!(get_reports(analyzer), report)
    end
    return report
end

function get_cache_key_per_config(analysis_params::JETAnalysisParams, inf_params::InferenceParams)
    h = @static UInt === UInt64 ? 0xa49bd446c0a5d90e : 0xe45361ac
    h = hash(analysis_params, h)
    h = hash(inf_params, h)
    return h
end

function get_cache_key(analyzer::AbstractAnalyzer)
    h = analyzer.state.cache_key
    h = hash(ReportPass(analyzer), h)
    return h
end

@inline function maybe_initialize_caches!(analyzer::AbstractAnalyzer)
    cache_key = get_cache_key(analyzer)
    haskey(JET_REPORT_CACHE, cache_key) || (JET_REPORT_CACHE[cache_key] = IdDict())
    haskey(JET_CODE_CACHE, cache_key)   || (JET_CODE_CACHE[cache_key]   = IdDict())
end

# check if we're in a toplevel module
@inline istoplevel(analyzer::AbstractAnalyzer, sv::InferenceState)    = istoplevel(analyzer, sv.linfo)
@inline istoplevel(analyzer::AbstractAnalyzer, linfo::MethodInstance) = get_toplevelmod(analyzer) === linfo.def

@inline istoplevelmod(analyzer::AbstractAnalyzer, mod::Module) = mod in get_toplevelmods(analyzer)

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

# the default abstract interpreter for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API requirements

# NOTE `@aggressive_constprop` here makes sure `mode` to be propagated as constant
@aggressive_constprop @jetconfigurable function JETAnalyzer(;
    report_pass::Union{Nothing,T} = nothing,
    mode::Symbol                  = :basic,
    jetconfigs...) where {T<:ReportPass}
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        report_pass = mode === :basic ? BasicPass() :
                      mode === :sound ? SoundPass() :
                      throw(ArgumentError("`mode` configuration should be either of `:basic` or `:sound`"))
    end
    return JETAnalyzer(report_pass,
                       AnalyzerState(; jetconfigs...),
                       )
end
AnalyzerState(analyzer::JETAnalyzer)                          = analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) = JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer)                             = analyzer.report_pass
