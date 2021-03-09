# configurations
# --------------

# TODO more configurations, e.g. ignore user-specified modules and such
"""
Configurations for JET analysis:

- `strict_condition_check::Bool = false` \\
  Enables strict condition check.
  JET reports an error if a condition expression type is "non-boolean" type. In a case when
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

  The effect of this configuration can be described with the following example:

  > with `strict_condition_check::Bool = false` (default)
  ```julia
  julia> test_f() = Dict('a' => 1, :b => 2) # ::Dict{Any,Int}
  test_f (generic function with 1 method)

  julia> @report_call test_f()
  No errors !
  Dict{Any, Int64}
  ```

  > with `strict_condition_check::Bool = true`
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
Configurations for Julia's native type inference routine:

- `ipo_constant_propagation::Bool = true` \\
  Enables constant propagation in abstract interpretation.
  It is _**highly**_ recommended that you keep this configuration `true` to get reasonable analysis,
  because constant propagation can cut off lots of false positive errorenous code paths and
  thus lead to more accurate and useful analysis results.

- `aggressive_constant_propagation::Bool = true` \\
  If `true`, JET will try to do constant propagation more "aggressively".
  As explained above, it can lead to more accurate analysis, but also lead to worse analysis
  performance at the cost of that.

- `unoptimize_throw_blocks::Bool = false` \\
  Turn this on to skip analysis on code blocks that will eventually lead to a `throw` call.
  This configuration may improve the analysis performance, but it's better to be turned off
    for JET analysis, because there may be other errors even in those code blocks.

!!! note
    You can also specify all the other parameters that `Core.Compiler.InferenceParams` can accept,
    e.g. `max_methods::Int = 3`, `union_splitting::Int = 4`, etc.
"""
@jetconfigurable JETInferenceParams(; ipo_constant_propagation::Bool = true,
                                      aggressive_constant_propagation::Bool = true,
                                      unoptimize_throw_blocks::Bool = false,
                                      max_methods::Int = 3,
                                      union_splitting::Int = 4,
                                      apply_union_enum::Int = 8,
                                      tupletype_depth::Int = 3,
                                      tuple_splat::Int = 32,
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
JETOptimizationParams(; # inlining::Bool = inlining_enabled(),
                        # inline_cost_threshold::Int = 100,
                        # inline_nonleaf_penalty::Int = 1000,
                        # inline_tupleret_bonus::Int = 250,
                        # inline_error_path_cost::Int = 20,
                        max_methods::Int = 3,
                        tuple_splat::Int = 32,
                        union_splitting::Int = 4,
                        unoptimize_throw_blocks::Bool = false,
                        _jetconfigs...) =
    # NOTE we always disable inlining, because our current strategy to find undefined
    # local variable assumes un-inlined frames
    # TODO enable inlining to get better JET analysis performance ?
    # XXX but the self-profiling with `inlining = true` shows performance regression ...
    return OptimizationParams(; inlining = false,
                                # inline_cost_threshold::Int = 100,
                                # inline_nonleaf_penalty::Int = 1000,
                                # inline_tupleret_bonus::Int = 250,
                                # inline_error_path_cost::Int = 20,
                                max_methods,
                                tuple_splat,
                                union_splitting,
                                unoptimize_throw_blocks,
                                )

# XXX we need to consider world range ?
struct AnalysisResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    overridden_by_const::CC.BitVector
    cache::Vector{InferenceErrorReportCache}
    # adapted from https://github.com/JuliaLang/julia/blob/3129a5bef56bb7216024ae606c02b413b00990e3/base/compiler/types.jl#L32-L35
    function AnalysisResult(linfo::MethodInstance, given_argtypes::Vector{Any}, cache::Vector{InferenceErrorReportCache})
        argtypes, overridden_by_const = matching_cache_argtypes(linfo, given_argtypes)
        return new(linfo, argtypes, overridden_by_const, cache)
    end
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

    # records depth of call stack
    depth::Int

    function JETInterpreter(native::NativeInterpreter, args...)
        @assert !native.opt_params.inlining "inlining should be disabled for JETInterpreter analysis"
        new(native, args...)
    end
end

# constructor for fresh analysis
@jetconfigurable function JETInterpreter(world           = get_world_counter();
                                         current_frame   = nothing,
                                         cache           = AnalysisResult[],
                                         analysis_params = nothing,
                                         inf_params      = nothing,
                                         opt_params      = nothing,
                                         id              = gensym(:JETInterpreterID),
                                         concretized     = _CONCRETIZED,
                                         toplevelmod     = _TOPLEVELMOD,
                                         global_slots    = _GLOBAL_SLOTS,
                                         depth           = 0,
                                         jetconfigs...)
    isnothing(analysis_params) && (analysis_params = JETAnalysisParams(; jetconfigs...))
    isnothing(inf_params)      && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params)      && (opt_params = JETOptimizationParams(; jetconfigs...))
    return JETInterpreter(NativeInterpreter(world; inf_params, opt_params),
                          InferenceErrorReport[],
                          UncaughtExceptionReport[],
                          Set{InferenceErrorReport}(),
                          current_frame,
                          cache,
                          analysis_params,
                          id,
                          concretized,
                          toplevelmod,
                          global_slots,
                          0,
                          )
end
# dummies to interpret non-toplevel frames
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = @__MODULE__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

# constructor to do additional JET analysis in the middle of parent (non-toplevel) interpretation
function JETInterpreter(interp::JETInterpreter)
    return JETInterpreter(get_world_counter(interp);
                          current_frame   = interp.current_frame,
                          cache           = interp.cache,
                          analysis_params = JETAnalysisParams(interp),
                          inf_params      = InferenceParams(interp),
                          opt_params      = OptimizationParams(interp),
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
