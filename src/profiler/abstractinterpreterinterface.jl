# define `AbstractInterpreter` API

struct TPInterpreter <: AbstractInterpreter
    native::NativeInterpreter
    remarks::Vector{Tuple{MethodInstance,Int,String}}
    optimize::Bool
end

TPInterpreter(; optimize = false) = TPInterpreter(NativeInterpreter(), [], optimize)

InferenceParams(tpi::TPInterpreter) = InferenceParams(tpi.native)
OptimizationParams(tpi::TPInterpreter) = OptimizationParams(tpi.native)
get_world_counter(tpi::TPInterpreter) = get_world_counter(tpi.native)
get_inference_cache(tpi::TPInterpreter) = get_inference_cache(tpi.native)
code_cache(tpi::TPInterpreter) = code_cache(tpi.native)

# TP only works for runtime inference
lock_mi_inference(::TPInterpreter, ::MethodInstance) = nothing
unlock_mi_inference(::TPInterpreter, ::MethodInstance) = nothing

function add_remark!(tpi::TPInterpreter, sv::InferenceState, remark)
    push!(tpi.remarks, (sv.linfo, sv.currpc, remark))
end

may_optimize(tpi::TPInterpreter) = tpi.optimize
may_compress(::TPInterpreter) = false
may_discard_trees(::TPInterpreter) = false
