module Profiler

# overloads
import Core.Compiler:
    # `AbstractInterpreter` API defined in abstractinterpreterinterface.jl
    InferenceParams, OptimizationParams, get_world_counter, get_inference_cache, code_cache,
    lock_mi_inference, unlock_mi_inference, add_remark!, may_optimize, may_compress,
    may_discard_trees,
    # abstractinterpretation.jl
    abstract_call_gf_by_type, abstract_call_known, abstract_call,
    abstract_eval_special_value, abstract_eval_value_expr, abstract_eval_value,
    abstract_eval_statement

# TODO: use `using` instead
# loaded symbols
import Core:
    MethodInstance, TypeofBottom

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, Bottom,
    widenconst, ⊑, typeintersect, Builtin, CallMeta, argtypes_to_type, MethodMatchInfo,
    UnionSplitInfo, MethodLookupResult, Const, VarTable, SSAValue, abstract_eval_ssavalue,
    Slot, slot_id, GlobalRef, GotoIfNot, _methods_by_ftype, specialize_method, typeinf

import Base:
    Meta.isexpr, Iterators.flatten, to_tuple_type

import ..TypeProfiler:
    ErrorReport, NoMethodErrorReport, InvalidBuiltinCallErrorReport, UndefVarErrorReport,
    NonBooleanCondErrorReport, NativeRemark

include("abstractinterpreterinterface.jl")
include("abstractinterpretation.jl")
include("tfuncs.jl")

# TODO: handle multiple applicable methods
# FIXME: cached method specializations won't let abstract interpretation to happen and so suppress error reports
# maybe we can do either of
# - always invalidate cached method specializations
# - or attach error reports to each method specializations and use that when using cached method specialization cache
function profile_call_gf!(interp::TPInterpreter,
                          @nospecialize(tt::Type{<:Tuple}),
                          world::UInt = get_world_counter(interp)
                          )
    ms = _methods_by_ftype(tt, -1, world)
    (ms === false || length(ms) != 1) && error("Unable to find single applicable method for $tt")

    atypes, sparams, m = ms[1]

    # grab the appropriate method instance for these types
    mi = specialize_method(m, atypes, sparams)

    # create an InferenceResult to hold the result
    result = InferenceResult(mi)

    # create an InferenceState to begin inference, give it a world that is always newest
    frame = InferenceState(result, #=cached=# true, interp)

    # run type inference on this frame
    typeinf(interp, frame)

    return interp, frame
end
profile_call_gf!(@nospecialize(tt::Type{<:Tuple}), world::UInt = get_world_counter(); kwargs...) =
    return profile_call_gf!(TPInterpreter(world; kwargs...), tt)

# TODO:
# profile_call_builtin!

end  # module Profiler
