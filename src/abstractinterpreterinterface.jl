# `AbstractInterpreter` API
# -------------------------

struct TPInterpreter <: AbstractInterpreter
    #= native =#

    native::NativeInterpreter
    optimize::Bool
    compress::Bool
    discard_trees::Bool

    #= TypeProfiler.jl specific =#

    # caching
    # -------
    # for escaping reporting duplicated cached reports
    id::Symbol
    # for constructing virtual stack frame from cached reports
    current_frame::Ref{InferenceState}
    # keeping reports from frame that always `throw`
    exception_reports::Vector{Pair{Int,ExceptionReport}}

    # virtual toplevel execution
    # --------------------------
    # tracks this the currrent frame is toplevel (and should assign virtual global variables)
    istoplevel::Bool
    # keeps virtual global variables
    virtual_globalvar_table::Dict{Module,Dict{Symbol,Any}} # maybe we don't need this nested dicts

    # profiling
    # ---------
    # disabling caching of native remarks can speed up profiling time
    filter_native_remarks::Bool
    # reports found so far
    reports::Vector{InferenceErrorReport}

    function TPInterpreter(world::UInt = get_world_counter();
                           inf_params::InferenceParams = InferenceParams(;
                               # turn off this to get profiles on `throw` blocks,
                               # this might be good to default to `true` since `throw` calls
                               # themselves will be reported anyway
                               unoptimize_throw_blocks = true,
                           ),
                           opt_params::OptimizationParams = OptimizationParams(;
                               inlining = false,
                           ),
                           optimize::Bool = true,
                           compress::Bool = false,
                           discard_trees::Bool = false,
                           istoplevel::Bool = false,
                           virtual_globalvar_table::AbstractDict = Dict(),
                           filter_native_remarks::Bool = true,
                           )
        @assert !opt_params.inlining "inlining should be disabled"

        native = NativeInterpreter(world; inf_params, opt_params)
        id     = gensym(:TPInterpreterID)
        return new(native,
                   optimize,
                   compress,
                   discard_trees,
                   id,
                   Ref{InferenceState}(),
                   [],
                   istoplevel,
                   virtual_globalvar_table,
                   filter_native_remarks,
                   [],
                   )
    end
end

InferenceParams(interp::TPInterpreter) = InferenceParams(interp.native)
OptimizationParams(interp::TPInterpreter) = OptimizationParams(interp.native)
get_world_counter(interp::TPInterpreter) = get_world_counter(interp.native)
get_inference_cache(interp::TPInterpreter) = get_inference_cache(interp.native)

# TP only works for runtime inference
lock_mi_inference(::TPInterpreter, ::MethodInstance) = nothing
unlock_mi_inference(::TPInterpreter, ::MethodInstance) = nothing

function add_remark!(interp::TPInterpreter, ::InferenceState, report::InferenceErrorReport)
    push!(interp.reports, report)
    return
end
function add_remark!(interp::TPInterpreter, sv::InferenceState, s::String)
    interp.filter_native_remarks && return
    add_remark!(interp, sv, NativeRemark(interp, sv, s))
    return
end

may_optimize(interp::TPInterpreter) = interp.optimize
may_compress(interp::TPInterpreter) = interp.compress
may_discard_trees(interp::TPInterpreter) = interp.discard_trees
