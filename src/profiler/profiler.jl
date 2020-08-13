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

import ..TypeProfiler:
    print_report

# TODO: use `using` instead
# loaded symbols
import Core:
    MethodInstance, TypeofBottom

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, Bottom,
    widenconst, ⊑, typeintersect, Builtin, CallMeta, argtypes_to_type, MethodMatchInfo,
    UnionSplitInfo, MethodLookupResult, Const, VarTable, SSAValue, abstract_eval_ssavalue,
    Slot, slot_id, GlobalRef, GotoIfNot, _methods_by_ftype, specialize_method, typeinf,
    isconstType

import Base:
    Meta.isexpr, to_tuple_type

import ..TypeProfiler:
    ErrorReport, ERROR_COLOR,  NOERROR_COLOR, RAIL_COLORS, print_rails, fullpath,
    print_reports

include("inferenceerrorreport.jl")
include("abstractinterpreterinterface.jl")
include("abstractinterpretation.jl")
include("tfuncs.jl")

# TODO:
# - handle multiple applicable methods ?
# - `profile_call_builtin!` ?

# FIXME:
# cached method specializations won't let abstract interpretation to happen again and
# so may suppress error reports
#
# mre would be
# ```julia
# julia> @profile_call sum("julia") # error reported
# julia> sum("julia") # cause actual error
# julia> @profile_call sum("julia") # error won't reported again
# ```
#
# maybe we can do either of
# - always invalidate cached method specializations
# - or attach error reports to each method specializations and use that when using cached method specialization cache

profile_call_gf(@nospecialize(tt::Type{<:Tuple}), world::UInt = get_world_counter(); kwargs...) =
    return profile_call_gf!(TPInterpreter(world; kwargs...), tt)
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

macro profile_call(ex, kwargs...)
    @assert Meta.isexpr(ex, :call) "function call expression should be given"
    f = ex.args[1]
    args = ex.args[2:end]

    quote let
        interp, frame = $(profile_call)($(esc(f)), $(map(esc, args)...))
        $(print_reports)(stdout, interp.reports; $(map(esc, kwargs)...))
        $(get_rettyp)(frame)
    end end
end

@nospecialize

function profile_call(f, args...)
    tt = to_tuple_type(typeof′.([f, args...]))
    return profile_call_gf(tt)
end

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

get_rettyp(frame::InferenceState) = frame.result.result # want to widen const ?

export
    @profile_call, profile_call, profile_call_gf, profile_call_gf!

end  # module Profiler
