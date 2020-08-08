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

@nospecialize

# TODO: handle multiple applicable methods
function profile!(interp::TPInterpreter, tt::Type{<:Tuple}, world::UInt = get_world_counter(interp))
    # `get_world_counter` here will always make the method the newest as in REPL
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

    return frame # and `interp` now holds traced information
end

function profile_call!(tt::Type{<:Tuple}, world = get_world_counter(); kwargs...)
    interp = TPInterpreter(world; kwargs...)
    return profile_call!(interp, tt)
end
function profile_call!(interp::TPInterpreter, tt::Type{<:Tuple})
    frame = profile!(interp, tt)
    return interp, frame
end

@specialize

end  # module Profiler
