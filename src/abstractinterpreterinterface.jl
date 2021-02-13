const LocalCache = Dict{Vector{Any},Vector{InferenceErrorReportCache}}

struct AnalysisParams
    # disables caching of native remarks (that may speed up profiling time)
    filter_native_remarks::Bool
end

mutable struct JETInterpreter <: AbstractInterpreter
    #= native =#

    native::NativeInterpreter

    #= JET.jl specific =#

    # local report cache for constant analysis
    cache::LocalCache

    # for sequential assignment of virtual global variables
    id::Symbol

    # reports found so far
    reports::Vector{InferenceErrorReport}

    # stashes `UncaughtExceptionReport`s that are not caught so far
    uncaught_exceptions::Vector{UncaughtExceptionReport}

    # stashes `NativeRemark`s
    native_remarks::Vector{NativeRemark}

    # toplevel profiling (skip inference on actually interpreted statements)
    concretized::BitVector

    # configurations for analysis performed by this interpreter
    analysis_params::AnalysisParams

    # keeps track of the current inference frame (needed for report cache reconstruction)
    current_frame::Union{Nothing,InferenceState}

    # debugging
    depth::Int

    @jetconfigurable function JETInterpreter(world               = get_world_counter();
                                             id                  = gensym(:JETInterpreterID),
                                             reports             = InferenceErrorReport[],
                                             uncaught_exceptions = UncaughtExceptionReport[],
                                             native_remarks      = NativeRemark[],
                                             concretized         = BitVector(),
                                             )
        inf_params      = gen_inf_params(; jetconfigs...)
        opt_params      = gen_opt_params()
        @assert !opt_params.inlining "inlining should be disabled for JETInterpreter analysis"
        analysis_params = gen_analysis_params(; jetconfigs...)

        native = NativeInterpreter(world; inf_params, opt_params)
        return new(native,
                   LocalCache(),
                   id,
                   reports,
                   uncaught_exceptions,
                   native_remarks,
                   concretized,
                   analysis_params,
                   nothing,
                   0,
                   )
    end
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

function CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String)
    AnalysisParams(interp).filter_native_remarks && return
    push!(interp.native_remarks, NativeRemark(interp, sv, s))
    return
end

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
                                           # while we will report the `throw` anyway when the return type is
                                           # annotated as `Bottom`
                                           # unoptimize_throw_blocks::Bool = (@static VERSION ≥ v"1.7.0-DEV.525" ? false : true),
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
