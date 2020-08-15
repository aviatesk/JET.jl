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
    abstract_eval_statement, typeinf_local

# usings
# ------

# TODO: really use `using` instead
import Core:
    MethodInstance, TypeofBottom

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, NOT_FOUND,
    Bottom, widenconst, ⊑, isconstType, typeintersect, Builtin, CallMeta, argtypes_to_type,
    MethodMatchInfo, UnionSplitInfo, MethodLookupResult, Const, VarTable, SSAValue,
    abstract_eval_ssavalue, Slot, slot_id, GlobalRef, GotoIfNot, _methods_by_ftype,
    specialize_method, typeinf

import Base:
    parse_input_line, to_tuple_type

import Base.Meta:
    isexpr, _parse_string

# includes
# --------

include("reports.jl")
include("ast.jl")
include("abstractinterpreterinterface.jl")
include("abstractinterpretation.jl")
include("tfuncs.jl")
include("print.jl")
include("profile.jl")

# exports
# -------

export
    profile_file, profile_text, @profile_call

end
