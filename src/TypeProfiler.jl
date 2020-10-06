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
    abstract_eval_statement,
    # typeinfer.jl
    typeinf

# usings
# ------

# TODO: really use `using` instead
import Core:
    TypeofBottom, SimpleVector, LineInfoNode

import Core.Compiler:
    AbstractInterpreter, NativeInterpreter, InferenceState, InferenceResult, CodeInfo,
    InternalCodeCache, CodeInstance, WorldRange, CachedMethodTable, method_table,
    MethodInstance, Bottom, NOT_FOUND, MethodMatchInfo, UnionSplitInfo, MethodLookupResult,
    Const, VarTable, SSAValue, SlotNumber, Slot, slot_id, GlobalRef, GotoIfNot, ReturnNode,
    widenconst, isconstType, typeintersect, ⊑, Builtin, CallMeta, is_throw_call, tmerge,
    argtypes_to_type, abstract_eval_ssavalue, _methods_by_ftype, specialize_method,
    add_backedge!

import Base:
    parse_input_line, to_tuple_type, Fix1, Fix2, IdSet

import Base.Meta:
    isexpr, _parse_string, lower

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
include("actualinterpretation.jl")
include("tfuncs.jl")
include("tpcache.jl")
include("print.jl")
include("virtualprocess.jl")
include("watch.jl")

# entry
# -----

function profile_file(io::IO,
                      filename::AbstractString,
                      mod::Module = Main;
                      kwargs...)
    text = read(filename, String)
    return profile_text(io, text, filename, mod; kwargs...)
end
profile_file(args...; kwargs...) = profile_file(stdout::IO, args...; kwargs...)

function profile_text(io::IO,
                      text::AbstractString,
                      filename::AbstractString = "top-level",
                      mod::Module = Main;
                      profiling_logger::Union{Nothing,IO} = nothing,
                      filter_native_remarks::Bool = true,
                      kwargs...)
    included_files, reports, postprocess = report_errors(profiling_logger,
                                                         mod,
                                                         text,
                                                         filename;
                                                         filter_native_remarks
                                                         )
    return included_files, print_reports(io, reports, postprocess; kwargs...)
end
profile_text(args...; kwargs...) = profile_text(stdout::IO, args...; kwargs...)

report_errors(::Nothing, args...; kwargs...) = report_errors(args...; kwargs...)
function report_errors(logger::IO, args...; kwargs...)
    print(logger, "profiling from ", #= filename =# last(args), " ...")
    s = time()

    ret = report_errors(args...; kwargs...)

    sec = round(time() - s; digits = 3)

    print(logger, '\b'^3)
    println(logger, "(finished in $(sec) sec)")

    return ret
end

function report_errors(actualmod, text, filename; filter_native_remarks = true)
    virtualmod = gen_virtual_module(actualmod)

    interp = TPInterpreter(; filter_native_remarks) # dummy
    actualmodsym = Symbol(actualmod)
    ret, interp = virtual_process!(text, filename, actualmodsym, virtualmod, interp)

    return ret.included_files,
           # non-empty `ret.toplevel_error_reports` means critical errors happened during
           # the AST transformation, so they always have precedence over `ret.inference_error_reports`
           !isempty(ret.toplevel_error_reports) ? ret.toplevel_error_reports : ret.inference_error_reports,
           gen_postprocess(virtualmod, actualmod)
end

# fix virtual module printing based on string manipulation; the "actual" modules may not be
# loaded into this process
function gen_postprocess(virtualmod, actualmod)
    virtual = string(virtualmod)
    actual  = string(actualmod)
    return actualmod == Main ?
        Fix2(replace, "Main." => "") ∘ Fix2(replace, virtual => actual) :
        Fix2(replace, virtual => actual)
end

function profile_toplevel!(interp::TPInterpreter, mod::Module, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.uninferred = src
    mi.specTypes = Tuple{}
    mi.def = mod

    # initialize for toplevel execution
    resize!(interp.locals, length(src.slotnames))
    resize!(interp.ssavalues, src.ssavaluetypes::Int)

    result = InferenceResult(mi);
    frame = InferenceState(result, src, #= cached =# true, interp);

    return profile_frame!(interp, frame)
end

# TODO:
# - handle multiple applicable methods ?
# - `profile_call_builtin!` ?

profile_call_gf(@nospecialize(tt::Type{<:Tuple}), world::UInt = get_world_counter(); kwargs...) =
    return profile_call_gf!(TPInterpreter(world; kwargs...), tt)
function profile_call_gf!(interp::TPInterpreter,
                          @nospecialize(tt::Type{<:Tuple}),
                          world::UInt = get_world_counter(interp)
                          )
    ms = _methods_by_ftype(tt, -1, world)
    @assert !(ms === false || length(ms) != 1) "unable to find single applicable method for $(tt)"

    atypes, sparams, m = first(ms)

    mi = specialize_method(m, atypes, sparams)

    result = InferenceResult(mi)

    frame = InferenceState(result, #= cached =# true, interp)

    return profile_frame!(interp, frame)
end

# testing, interactive session
# ----------------------------

@nospecialize

function profile_call(f, argtypes::Type...; kwargs...)
    tt = to_tuple_type([typeof′(f), argtypes...])
    return profile_call_gf(tt; kwargs...)
end

profile_call(f, argtypes; kwargs...) = profile_call(f, argtypes...; kwargs...)

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

macro profile_call(ex, kwargs...)
    @assert isexpr(ex, :call) "function call expression should be given"
    f = first(ex.args)
    args = ex.args[2:end]

    return quote let
        argtypes = $(typeof′).(($(map(esc, args)...),))
        interp, frame = $(profile_call)($(esc(f)), argtypes)
        $(print_reports)(stdout::IO, interp.reports; $(map(esc, kwargs)...))
        $(get_result)(frame) # maybe want to widen const ?
    end end
end

# exports
# -------

export
    profile_file, profile_and_watch_file, profile_text, @profile_call

end
