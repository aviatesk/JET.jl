struct AnalysisParams
    # disables caching of native remarks (that may speed up profiling time)
    filter_native_remarks::Bool

    @jetconfigurable function AnalysisParams(; filter_native_remarks::Bool = true,
                                               )
        return new(filter_native_remarks,
                   )
    end
end

const LocalCache = Dict{Vector{Any},Vector{InferenceErrorReportCache}}

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

    # stashes explicitly declared local variables (FILO, i.e. the last element is for the current frame)
    locals_stack::Vector{Set{Int}}

    # debugging
    depth::Int

    @jetconfigurable function JETInterpreter(world               = get_world_counter();
                                          inf_params          = gen_inf_params(),
                                          opt_params          = gen_opt_params(),
                                          id                  = gensym(:JETInterpreterID),
                                          reports             = InferenceErrorReport[],
                                          uncaught_exceptions = UncaughtExceptionReport[],
                                          native_remarks      = NativeRemark[],
                                          concretized         = BitVector(),
                                          analysis_params     = AnalysisParams(),
                                          )
        @assert !opt_params.inlining "inlining should be disabled for JETInterpreter analysis"

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
                   Set{Symbol}[],
                   0,
                   )
    end
end

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

CC.may_optimize(interp::JETInterpreter)      = false
CC.may_compress(interp::JETInterpreter)      = false
CC.may_discard_trees(interp::JETInterpreter) = false

# JETInterpreter specific
# -----------------------

AnalysisParams(interp::JETInterpreter) = interp.analysis_params

@jetconfigurable function gen_inf_params(; # more constant prop, more correct reports ?
                                           aggressive_constant_propagation::Bool = true,
                                           # turn this off to get profiles on `throw` blocks, this might be good to default
                                           # to `true` since `throw` calls themselves will be reported anyway
                                           unoptimize_throw_blocks::Bool = true,
                                           )
    return @static VERSION ≥ v"1.6.0-DEV.837" ?
           InferenceParams(; aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             ) :
           InferenceParams(; aggressive_constant_propagation,
                             )
end

@jetconfigurable function gen_opt_params(; # inlining should be disabled for `JETInterpreter`, otherwise virtual stack frame
                                           # traversing will fail for frames after optimizer runs on
                                           inlining = false,
                                           )
    return OptimizationParams(; inlining,
                                )
end

get_id(interp::JETInterpreter) = interp.id

# TODO do report filtering or something configured by `AnalysisParams(interp)`
function report!(interp::JETInterpreter, report::InferenceErrorReport)
    push!(interp.reports, report)
end

function stash_uncaught_exception!(interp::JETInterpreter, report::UncaughtExceptionReport)
    push!(interp.uncaught_exceptions, report)
end

locals(interp::JETInterpreter) = last(interp.locals_stack)

function setup_locals!(interp::JETInterpreter, frame::InferenceState)
    locals = Set{Int}()
    # slotnames = frame.src.slotnames
    for node in frame.src.code
        if isa(node, NewvarNode)
            push!(locals, slot_id(node.slot))
        end
    end
    push!(interp.locals_stack, locals)
end

remove_locals!(interp::JETInterpreter, frame::InferenceState) = pop!(interp.locals_stack)
