# configurations
# --------------

# TODO more configurations, e.g. ignore user-specified modules and such
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
---
- `ignore_native_remarks::Bool = true` \\
  If `true`, JET won't construct nor cache reports of "native remarks", which may speed up analysis time.
  "Native remarks" are information that Julia's native compiler emits about how type inference routine goes,
  and those remarks are less interesting in term of "error checking", so JET ignores them by default.
"""
struct JETAnalysisParams
    strict_condition_check::Bool
    ignore_native_remarks::Bool
    @jetconfigurable JETAnalysisParams(; strict_condition_check::Bool = false,
                                         ignore_native_remarks::Bool  = true,
                                         ) =
        return new(strict_condition_check,
                   ignore_native_remarks,
                   )
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
    # XXX but the self-profiling with `inlining = true` shows performance regression ...
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
const LOGGER_LEVELS        = Dict(INFO_LOGGER_LEVEL  => "(info level, default)",
                                  DEBUG_LOGGER_LEVEL => "(debug level)",
                                  )
const LOGGER_LEVELS_DESC   = let
    map(collect(LOGGER_LEVELS)) do (level, desc)
        "`$level` $desc"
    end |> Fix2(join, ", ")
end
const DEFAULT_LOGGER_LEVEL = INFO_LOGGER_LEVEL
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

mutable struct JETInterpreter <: AbstractInterpreter
    ### native ###

    native::NativeInterpreter

    ### JET.jl specific ###

    ## general ##

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

    # configurations for JET analysis
    analysis_params::JETAnalysisParams

    # tracks there has been any error reported in `abstract_call_method`
    # that information will help JET decide it's worth to do constant prop' even if
    # `NativeInterpreter` doesn't find it useful (i.e. the return type can't be improved anymore)
    # NOTE this field is supposed to be computed at each call of `abstract_call_method`, and
    # then immediately reset within the next `abstract_call_method_with_const_args` call
    anyerror::Bool

    ## virtual toplevel execution ##

    # for sequential assignment of abstract global variables
    id::Symbol

    # toplevel profiling (skip inference on actually interpreted statements)
    concretized::BitVector

    # virtual toplevel module
    toplevelmod::Module

    # slots to represent toplevel global variables
    global_slots::Dict{Int,Symbol}

    ## debug ##

    logger::JETLogger

    # records depth of call stack
    depth::Int

    function JETInterpreter(native::NativeInterpreter, args...)
        @assert !native.opt_params.inlining "inlining should be disabled for JETInterpreter analysis"
        new(native, args...)
    end
end

# constructor for fresh analysis
@jetconfigurable function JETInterpreter(world::UInt     = get_world_counter();
                                         current_frame   = nothing,
                                         cache           = AnalysisResult[],
                                         analysis_params = nothing,
                                         inf_params      = nothing,
                                         opt_params      = nothing,
                                         id              = gensym(:JETInterpreterID),
                                         concretized     = _CONCRETIZED,
                                         toplevelmod     = _TOPLEVELMOD,
                                         global_slots    = _GLOBAL_SLOTS,
                                         logger          = nothing,
                                         depth           = 0,
                                         jetconfigs...)
    isnothing(analysis_params) && (analysis_params = JETAnalysisParams(; jetconfigs...))
    isnothing(inf_params)      && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params)      && (opt_params = JETOptimizationParams(; jetconfigs...))
    isnothing(logger)          && (logger = JETLogger(; jetconfigs...))
    return JETInterpreter(NativeInterpreter(world; inf_params, opt_params),
                          InferenceErrorReport[],
                          UncaughtExceptionReport[],
                          Set{InferenceErrorReport}(),
                          current_frame,
                          cache,
                          analysis_params,
                          false,
                          id,
                          concretized,
                          toplevelmod,
                          global_slots,
                          logger,
                          depth,
                          )
end
# dummies to interpret non-toplevel frames
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = @__MODULE__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

# constructor for sequential toplevel JET analysis
function JETInterpreter(interp::JETInterpreter, concretized, toplevelmod)
    return JETInterpreter(# update world age to take in newly added methods defined
                          # in a previous toplevel interpretation performed by `ConcreteInterpreter`
                          get_world_counter();
                          analysis_params = JETAnalysisParams(interp),
                          inf_params      = InferenceParams(interp),
                          opt_params      = OptimizationParams(interp),
                          concretized     = concretized, # or construct partial `CodeInfo` from remaining abstract statements ?
                          toplevelmod     = toplevelmod,
                          logger          = JETLogger(interp),
                          )
end

# constructor to do additional JET analysis in the middle of parent (non-toplevel) interpretation
function JETInterpreter(interp::JETInterpreter)
    return JETInterpreter(get_world_counter(interp);
                          current_frame   = interp.current_frame,
                          cache           = interp.cache,
                          analysis_params = JETAnalysisParams(interp),
                          inf_params      = InferenceParams(interp),
                          opt_params      = OptimizationParams(interp),
                          logger          = JETLogger(interp),
                          depth           = interp.depth,
                          )
end

function Base.show(io::IO, interp::JETInterpreter)
    rn = length(interp.reports)
    en = length(interp.uncaught_exceptions)
    print(io, "JETInterpreter with $(rn) reports and $(en) uncaught exceptions")
    frame = interp.current_frame
    if !isnothing(frame)
        print(io, " at ")
        show(io, frame.linfo)
    end
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", interp::JETInterpreter) =
    return interp
Base.show(io::IO, ::MIME"application/prs.juno.inline", interp::NativeInterpreter) =
    return interp

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

CC.InferenceParams(interp::JETInterpreter)    = InferenceParams(interp.native)
CC.OptimizationParams(interp::JETInterpreter) = OptimizationParams(interp.native)
CC.get_world_counter(interp::JETInterpreter)  = get_world_counter(interp.native)

# JET only works for runtime inference
CC.lock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing
CC.unlock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing

# function CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String)
#     JETAnalysisParams(interp).ignore_native_remarks && return
#     push!(interp.native_remarks, NativeRemark(interp, sv, s))
#     return
# end
CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String) = return

CC.may_optimize(interp::JETInterpreter)      = true
CC.may_compress(interp::JETInterpreter)      = false
CC.may_discard_trees(interp::JETInterpreter) = false

# JETInterpreter specific
# -----------------------

JETAnalysisParams(interp::JETInterpreter) = interp.analysis_params

JETLogger(interp::JETInterpreter) = interp.logger

get_id(interp::JETInterpreter) = interp.id

# TODO do report filtering or something configured by `JETAnalysisParams(interp)`
function report!(interp::JETInterpreter, report::InferenceErrorReport)
    push!(interp.reports, report)
end

function stash_uncaught_exception!(interp::JETInterpreter, report::UncaughtExceptionReport)
    push!(interp.uncaught_exceptions, report)
end

is_global_slot(interp::JETInterpreter, slot::Int)   = slot in keys(interp.global_slots)
is_global_slot(interp::JETInterpreter, slot::Slot)  = is_global_slot(interp, slot_id(slot))
is_global_slot(interp::JETInterpreter, sym::Symbol) = sym in values(interp.global_slots)

@inline with_toplevel_logger(@nospecialize(f), interp::JETInterpreter, @nospecialize(filter = ≥(DEFAULT_LOGGER_LEVEL))) =
    with_logger(f, JETLogger(interp).toplevel_logger, filter, "toplevel")

@inline with_inference_logger(@nospecialize(f), interp::JETInterpreter, @nospecialize(filter = ≥(DEFAULT_LOGGER_LEVEL))) =
    with_logger(f, JETLogger(interp).inference_logger, filter, "inference")

@inline function with_logger(
    @nospecialize(f), io::Union{Nothing,IO}, @nospecialize(filter), logger_name)
    isnothing(io) && return
    level = get_logger_level(io)
    filter(level) || return
    print(io, "[$logger_name-$(LOGGER_LEVELS[level])] ")
    f(io)
end
