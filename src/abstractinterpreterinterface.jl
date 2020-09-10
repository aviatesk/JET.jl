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
    virtual_globalvar_table::Dict{Module,Dict{Symbol,Any}}

    # profiling
    # ---------
    # disabling caching of native remarks can speed up profiling time
    filter_native_remarks::Bool
    # reports found so far
    reports::Vector{InferenceErrorReport}

    function TPInterpreter(world::UInt = get_world_counter();
                           inf_params::InferenceParams = gen_inf_params(),
                           opt_params::OptimizationParams = gen_opt_params(),
                           optimize::Bool = true,
                           compress::Bool = false,
                           discard_trees::Bool = false,
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
                   virtual_globalvar_table,
                   filter_native_remarks,
                   [],
                   )
    end
end

function gen_inf_params()
    return @static if VERSION ≥ v"1.6.0-DEV.837"
        InferenceParams(;
            # turn this off to get profiles on `throw` blocks, this might be good to default
            # to `true` since `throw` calls themselves will be reported anyway
            unoptimize_throw_blocks = true,
        )
    else
        InferenceParams()
    end
end

function gen_opt_params()
    return OptimizationParams(;
        # inlining should be disable for `TPInterpreter`, otherwise virtual stack frame
        # traversing will fail for frames after optimizer runs on
        inlining = false,
    )
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
