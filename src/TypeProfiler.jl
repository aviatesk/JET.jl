@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module TypeProfiler

# imports
# -------

import Core.Compiler:
    # `AbstractInterpreter` API defined in abstractinterpreterinterface.jl
    InferenceParams, OptimizationParams, get_world_counter, get_inference_cache, code_cache,
    lock_mi_inference, unlock_mi_inference, add_remark!, may_optimize, may_compress,
    may_discard_trees,
    # abstractinterpretation.jl
    abstract_call_gf_by_type, abstract_call_known, abstract_call,
    abstract_eval_special_value, abstract_eval_value_expr, abstract_eval_value,
    abstract_eval_statement, builtin_tfunction, typeinf_local

# usings
# ------

# TODO: really use `using` instead
import Core:
    TypeofBottom

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, CodeInfo,
    MethodInstance, Bottom, NOT_FOUND, MethodMatchInfo, UnionSplitInfo, MethodLookupResult,
    Const, VarTable, SSAValue, SlotNumber, Slot, slot_id, GlobalRef, GotoIfNot, ReturnNode,
    widenconst, isconstType, typeintersect, ⊑, Builtin, CallMeta,
    argtypes_to_type, abstract_eval_ssavalue, _methods_by_ftype, specialize_method, typeinf

import Base:
    parse_input_line, to_tuple_type

import Base.Meta:
    isexpr, _parse_string

using FileWatching

# includes
# --------

include("reports.jl")
include("virtualprocess.jl")
include("abstractinterpreterinterface.jl")
include("abstractinterpretation.jl")
include("tfuncs.jl")
include("print.jl")
include("profile.jl")
include("watch.jl")

# exports
# -------

export
    profile_file, profile_and_watch_file, profile_text, @profile_call

end
