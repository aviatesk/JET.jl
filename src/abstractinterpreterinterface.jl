# TODO world range
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

struct AnalysisParams
    # disables caching of native remarks (that may speed up profiling time)
    filter_native_remarks::Bool
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
    analysis_params::AnalysisParams

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
    isnothing(analysis_params) && (analysis_params = gen_analysis_params(; jetconfigs...))
    isnothing(inf_params)      && (inf_params = gen_inf_params(; jetconfigs...))
    isnothing(opt_params)      && (opt_params = gen_opt_params())
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
#     AnalysisParams(interp).filter_native_remarks && return
#     push!(interp.native_remarks, NativeRemark(interp, sv, s))
#     return
# end
CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String) = return

CC.may_optimize(interp::JETInterpreter)      = true
CC.may_compress(interp::JETInterpreter)      = false
CC.may_discard_trees(interp::JETInterpreter) = false

# JETInterpreter specific
# -----------------------

AnalysisParams(interp::JETInterpreter) = interp.analysis_params

@jetconfigurable function gen_inf_params(; # more constant prop, more correct reports ?
                                           aggressive_constant_propagation::Bool = true,
                                           # turn this on to skip analysis on `throw` blocks;
                                           # this is better to be turned off for JET analysis because
                                           # there may be other errors in blocks that lead to a `throw` call
                                           # while we will report the uncaught `throw`s anyway
                                           unoptimize_throw_blocks::Bool = false,
                                           )
    return @static VERSION ≥ v"1.6.0-DEV.837" ?
           InferenceParams(; aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             ) :
           InferenceParams(; aggressive_constant_propagation,
                             )
end

function gen_opt_params()
    return OptimizationParams(; # inlining should be disabled for `JETInterpreter`, otherwise
                                # virtual stack frame traversing will fail for frames after
                                # optimizer runs on
                                inlining = false,
                                )
end

# TODO configurable analysis, e.g. ignore user-specified modules and such
@jetconfigurable function gen_analysis_params(; filter_native_remarks::Bool = true,
                                                )
    return AnalysisParams(filter_native_remarks)
end

get_id(interp::JETInterpreter) = interp.id

# TODO do report filtering or something configured by `AnalysisParams(interp)`
function report!(interp::JETInterpreter, report::InferenceErrorReport)
    push!(interp.reports, report)
end

function stash_uncaught_exception!(interp::JETInterpreter, report::UncaughtExceptionReport)
    push!(interp.uncaught_exceptions, report)
end

is_global_slot(interp::JETInterpreter, slot::Int)   = slot in keys(interp.global_slots)
is_global_slot(interp::JETInterpreter, slot::Slot)  = is_global_slot(interp, slot_id(slot))
is_global_slot(interp::JETInterpreter, sym::Symbol) = sym in values(interp.global_slots)
