@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module TypeProfiler

# imports
# -------

import Core.Compiler:
    # `AbstractInterpreter` API defined in abstractinterpreterinterface.jl
    InferenceParams, OptimizationParams, get_world_counter, get_inference_cache, code_cache,
    lock_mi_inference, unlock_mi_inference, add_remark!, may_optimize, may_compress,
    may_discard_trees,
    # cicache.jl
    WorldView,
    # tfuncs.jl
    builtin_tfunction, return_type_tfunc,
    # abstractinterpretation.jl
    abstract_call_gf_by_type, abstract_call_known, abstract_call,
    abstract_eval_special_value, abstract_eval_value_expr, abstract_eval_value,
    abstract_eval_statement, typeinf_local,
    # typeinfer.jl
    typeinf, typeinf_edge

# usings
# ------

# TODO: really use `using` instead
import Core:
    TypeofBottom, SimpleVector, LineInfoNode

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, CodeInfo,
    InternalCodeCache, CodeInstance, WorldRange,
    MethodInstance, Bottom, NOT_FOUND, MethodMatchInfo, UnionSplitInfo, MethodLookupResult,
    Const, VarTable, SSAValue, SlotNumber, Slot, slot_id, GlobalRef, GotoIfNot, ReturnNode,
    widenconst, isconstType, typeintersect, ⊑, Builtin, CallMeta, is_throw_call,
    argtypes_to_type, abstract_eval_ssavalue, _methods_by_ftype, specialize_method, typeinf

import Base:
    parse_input_line, to_tuple_type, Fix1, Fix2

import Base.Meta:
    isexpr, _parse_string

import Base.Iterators:
    flatten

using FileWatching, Requires

const CC = Core.Compiler

# includes
# --------

const INIT_HOOKS = Function[]
push_inithook!(f) = push!(INIT_HOOKS, f)
__init__() = foreach(f->f(), INIT_HOOKS)

include("reports.jl")
include("abstractinterpreterinterface.jl")
include("abstractinterpretation.jl")
include("tfuncs.jl")
include("tpcache.jl")
include("print.jl")
include("profile.jl")
include("virtualprocess.jl")
include("watch.jl")

# exports
# -------

export
    profile_file, profile_and_watch_file, profile_text, @profile_call

end
