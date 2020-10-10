# `AbstractInterpreter` API
# -------------------------

struct JETInterpreter <: AbstractInterpreter
    #= native =#

    native::NativeInterpreter
    optimize::Bool
    compress::Bool
    discard_trees::Bool

    #= JET.jl specific =#

    # for escaping force inference on "erroneous" cached frames, sequential assignment of virtual global variable
    id::Symbol

    # reports found so far
    reports::Vector{InferenceErrorReport}

    # keeps `throw` calls that are not caught within a single frame
    exception_reports::Vector{Pair{Int,ExceptionReport}}

    # disables caching of native remarks (that may vastly speed up profiling time)
    filter_native_remarks::Bool

    # toplevel profiling (skip inference on actually interpreted statements)
    concretized::BitVector

    function JETInterpreter(world                 = get_world_counter();
                            inf_params            = gen_inf_params(),
                            opt_params            = gen_opt_params(),
                            optimize              = true,
                            compress              = false,
                            discard_trees         = false,
                            id                    = gensym(:JETInterpreterID),
                            reports               = [],
                            exception_reports     = [],
                            filter_native_remarks = true,
                            concretized           = [],
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
                   concretized,
                   )
    end
end

# API
# ---

CC.InferenceParams(interp::JETInterpreter) = InferenceParams(interp.native)
CC.OptimizationParams(interp::JETInterpreter) = OptimizationParams(interp.native)
CC.get_world_counter(interp::JETInterpreter) = get_world_counter(interp.native)
CC.get_inference_cache(interp::JETInterpreter) = get_inference_cache(interp.native)

# JET only works for runtime inference
CC.lock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing
CC.unlock_mi_inference(::JETInterpreter, ::MethodInstance) = nothing

function CC.add_remark!(interp::JETInterpreter, ::InferenceState, report::InferenceErrorReport)
    push!(interp.reports, report)
    return
end
function CC.add_remark!(interp::JETInterpreter, sv::InferenceState, s::String)
    interp.filter_native_remarks && return
    add_remark!(interp, sv, NativeRemark(interp, sv, s))
    return
end

CC.may_optimize(interp::JETInterpreter) = interp.optimize
CC.may_compress(interp::JETInterpreter) = interp.compress
CC.may_discard_trees(interp::JETInterpreter) = interp.discard_trees

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
        # inlining should be disable for `JETInterpreter`, otherwise virtual stack frame
        # traversing will fail for frames after optimizer runs on
        inlining = false,
    )
end

get_id(interp::JETInterpreter) = interp.id
