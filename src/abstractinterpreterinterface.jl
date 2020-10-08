# `AbstractInterpreter` API
# -------------------------

struct TPInterpreter <: AbstractInterpreter
    #= native =#

    native::NativeInterpreter
    optimize::Bool
    compress::Bool
    discard_trees::Bool

    #= TypeProfiler.jl specific =#

    # for escaping force inference on "erroneous" cached frames, sequential assignment of virtual global variable
    id::Symbol

    # reports found so far
    reports::Vector{InferenceErrorReport}

    # keeps `throw` calls that are not caught within a single frame
    exception_reports::Vector{Pair{Int,ExceptionReport}}

    # disables caching of native remarks (that may vastly speed up profiling time)
    filter_native_remarks::Bool

    # toplevel profiling (skip inference on actually interpreted statements)
    not_abstracted::BitVector

    function TPInterpreter(world                   = get_world_counter();
                           inf_params              = gen_inf_params(),
                           opt_params              = gen_opt_params(),
                           optimize                = true,
                           compress                = false,
                           discard_trees           = false,
                           id                      = gensym(:TPInterpreterID),
                           reports                 = [],
                           exception_reports       = [],
                           filter_native_remarks   = true,
                           not_abstracted          = [],
                           )
        @assert !opt_params.inlining "inlining should be disabled"

        native = NativeInterpreter(world; inf_params, opt_params)
        return new(native,
                   optimize,
                   compress,
                   discard_trees,
                   id,
                   reports,
                   exception_reports,
                   filter_native_remarks,
                   not_abstracted,
                   )
    end
end

# API
# ---

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

# specific
# --------

function gen_inf_params()
    return @static if VERSION ≥ v"1.6.0-DEV.837"
        InferenceParams(;
            # more constant prop, more correct reports ?
            aggressive_constant_propagation = true,
            # turn this off to get profiles on `throw` blocks, this might be good to default
            # to `true` since `throw` calls themselves will be reported anyway
            unoptimize_throw_blocks = true,
        )
    else
        InferenceParams(;
            aggressive_constant_propagation = true,
        )
    end
end

function gen_opt_params()
    return OptimizationParams(;
        # inlining should be disable for `TPInterpreter`, otherwise virtual stack frame
        # traversing will fail for frames after optimizer runs on
        inlining = false,
    )
end

get_id(interp::TPInterpreter) = interp.id
