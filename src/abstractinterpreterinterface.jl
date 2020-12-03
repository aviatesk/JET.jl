struct AnalysisParams
    # disables caching of native remarks (that may speed up profiling time)
    filter_native_remarks::Bool

    function AnalysisParams(; filter_native_remarks::Bool = true,
                              # dummy kwargs so that kwargs for other functions can be passed on
                              __kwargs...)
        return new(filter_native_remarks,
                   )
    end
end

const LocalCache = Dict{Vector{Any},Vector{InferenceErrorReportCache}}

struct JETInterpreter <: AbstractInterpreter
    #= native =#

    native::NativeInterpreter
    optimize::Bool
    compress::Bool
    discard_trees::Bool

    #= JET.jl specific =#

    # local report cache for constant analysis
    cache::LocalCache

    # for sequential assignment of virtual global variables
    id::Symbol

    stackframes::Vector{InferenceState}

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

    # debugging
    depth::Ref{Int}

    function JETInterpreter(world               = get_world_counter();
                            inf_params          = gen_inf_params(),
                            opt_params          = gen_opt_params(),
                            optimize            = true,
                            compress            = false,
                            discard_trees       = false,
                            id                  = gensym(:JETInterpreterID),
                            stackframes         = InferenceState[],
                            reports             = InferenceErrorReport[],
                            uncaught_exceptions = UncaughtExceptionReport[],
                            native_remarks      = NativeRemark[],
                            concretized         = BitVector(),
                            analysis_params     = AnalysisParams(),
                            # dummy kwargs so that kwargs for other functions can be passed on
                            __kwargs...)
        @assert !opt_params.inlining "inlining should be disabled for JETInterpreter analysis"

        native = NativeInterpreter(world; inf_params, opt_params)
        return new(native,
                   optimize,
                   compress,
                   discard_trees,
                   LocalCache(),
                   id,
                   stackframes,
                   reports,
                   uncaught_exceptions,
                   native_remarks,
                   concretized,
                   analysis_params,
                   Ref(0),
                   )
    end
end

# AbstractInterpreter API
# -----------------------

CC.InferenceParams(interp::JETInterpreter) = InferenceParams(interp.native)
CC.OptimizationParams(interp::JETInterpreter) = OptimizationParams(interp.native)
CC.get_world_counter(interp::JETInterpreter) = get_world_counter(interp.native)
CC.get_inference_cache(interp::JETInterpreter) = get_inference_cache(interp.native)

# JET only works for runtime inference
CC.lock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing
CC.unlock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing

function CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String)
    AnalysisParams(interp).filter_native_remarks && return
    push!(interp.native_remarks, NativeRemark(interp, sv, s))
    return
end

CC.may_optimize(interp::JETInterpreter) = interp.optimize
CC.may_compress(interp::JETInterpreter) = interp.compress
CC.may_discard_trees(interp::JETInterpreter) = interp.discard_trees

# JETInterpreter specific
# -----------------------

AnalysisParams(interp::JETInterpreter) = interp.analysis_params

function gen_inf_params(; # more constant prop, more correct reports ?
                          aggressive_constant_propagation::Bool = true,
                          # turn this off to get profiles on `throw` blocks, this might be good to default
                          # to `true` since `throw` calls themselves will be reported anyway
                          unoptimize_throw_blocks::Bool = true,
                          # dummy kwargs so that kwargs for other functions can be passed on
                          __kwargs...)
    return @static VERSION ≥ v"1.6.0-DEV.837" ?
           InferenceParams(; aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             ) :
           InferenceParams(; aggressive_constant_propagation,
                             )
end

function gen_opt_params(; # inlining should be disabled for `JETInterpreter`, otherwise virtual stack frame
                          # traversing will fail for frames after optimizer runs on
                          inlining = false,
                          # dummy kwargs so that kwargs for other functions can be passed on
                          __kwargs...)
    return OptimizationParams(; inlining,
                                )
end

get_id(interp::JETInterpreter) = interp.id

# TODO do report filtering or something configured by `AnalysisParams(interp)`
function report!(interp::JETInterpreter, report::InferenceErrorReport)
    push!(interp.reports, report)
    return
end
