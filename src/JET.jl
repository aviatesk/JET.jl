@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module JET

# imports
# -------

# `JETInterpreter`
import Core.Compiler:
    # abstractinterpreterinterface.jl
    InferenceParams,
    OptimizationParams,
    get_world_counter,
    get_inference_cache,
    lock_mi_inference,
    unlock_mi_inference,
    add_remark!,
    may_optimize,
    may_compress,
    may_discard_trees,
    # jetcache.jl
    code_cache,
    WorldView,
    # tfuncs.jl
    builtin_tfunction,
    return_type_tfunc,
    # abstractinterpretation.jl
    abstract_call_gf_by_type,
    abstract_eval_special_value,
    abstract_eval_value,
    abstract_eval_statement,
    # typeinfer.jl
    typeinf

# `ConcreteInterpreter`
import JuliaInterpreter:
    # virtualprocess.jl
    step_expr!,
    evaluate_call_recurse!,
    handle_err

# usings
# ------
# TODO: really use `using` instead

import Core:
    MethodMatch,
    LineInfoNode,
    SimpleVector,
    svec

import Core.Compiler:
    AbstractInterpreter,
    NativeInterpreter,
    InferenceState,
    InferenceResult,
    CodeInfo,
    InternalCodeCache,
    CodeInstance,
    WorldRange,
    MethodInstance,
    Bottom,
    NOT_FOUND,
    MethodMatchInfo,
    UnionSplitInfo,
    MethodLookupResult,
    Const,
    VarTable,
    SSAValue,
    SlotNumber,
    Slot,
    slot_id,
    GlobalRef,
    GotoIfNot,
    ReturnNode,
    widenconst,
    ⊑,
    Builtin,
    CallMeta,
    is_throw_call,
    tmerge,
    argtypes_to_type,
    _methods_by_ftype,
    specialize_method,
    add_backedge!

import Base:
    parse_input_line,
    to_tuple_type,
    Fix1,
    Fix2,
    IdSet

import Base.Meta:
    isexpr,
    _parse_string,
    lower

import Base.Iterators:
    flatten

using LoweredCodeUtils, JuliaInterpreter

import LoweredCodeUtils:
    istypedef,
    ismethod,
    callee_matches

import JuliaInterpreter:
    bypass_builtins,
    maybe_evaluate_builtin,
    collect_args,
    is_return,
    is_quotenode_egal

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
include("jetcache.jl")
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

    interp = JETInterpreter(; filter_native_remarks) # dummy
    ret, interp = virtual_process!(text,
                                   filename,
                                   virtualmod,
                                   Symbol(actualmod),
                                   interp
                                   )

    return ret.included_files,
           # non-empty `ret.toplevel_error_reports` means critical errors happened during
           # the AST transformation, so they always have precedence over `ret.inference_error_reports`
           !isempty(ret.toplevel_error_reports) ? ret.toplevel_error_reports : ret.inference_error_reports,
           gen_postprocess(virtualmod, actualmod)
end

gen_virtual_module(actualmod = Main) =
    return Core.eval(actualmod, :(module $(gensym(:JETVirtualModule)) end))::Module

# fix virtual module printing based on string manipulation; the "actual" modules may not be
# loaded into this process
function gen_postprocess(virtualmod, actualmod)
    virtual = string(virtualmod)
    actual  = string(actualmod)
    return actualmod == Main ?
        Fix2(replace, "Main." => "") ∘ Fix2(replace, virtual => actual) :
        Fix2(replace, virtual => actual)
end

# this dummy module will be used by `istoplevel` to check if the current inference frame is
# created by `profile_toplevel!` or not (i.e. `@generated` function)
module toplevel end

function profile_toplevel!(interp::JETInterpreter, mod::Module, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.uninferred = src
    mi.specTypes = Tuple{}
    mi.def = mod

    result = InferenceResult(mi);
    frame = InferenceState(result, src, #= cached =# true, interp);

    mi.def = toplevel # set to the dummy module

    return profile_frame!(interp, frame)
end

# TODO `profile_call_builtin!` ?
function profile_gf_by_type!(interp::JETInterpreter,
                             @nospecialize(tt::Type{<:Tuple}),
                             world::UInt = get_world_counter(interp),
                             )
    mms = _methods_by_ftype(tt, InferenceParams(interp).MAX_METHODS, world)
    @assert mms !== false "unable to find matching method for $(tt)"

    filter!(mm->mm.spec_types===tt, mms)
    @assert length(mms) == 1 "unable to find single target method for $(tt)"

    mm = first(mms)::MethodMatch

    return profile_method_signature!(interp, mm.method, mm.spec_types, mm.sparams)
end

function profile_method_signature!(interp::JETInterpreter,
                                   m::Method,
                                   @nospecialize(atype),
                                   sparams::SimpleVector,
                                   world::UInt = get_world_counter(interp)
                                   )
    mi = specialize_method(m, atype, sparams)

    result = InferenceResult(mi)

    frame = InferenceState(result, #= cached =# true, interp)

    return profile_frame!(interp, frame)
end

# miscellaneous, interactive
# ----------------------------

const __self_profiling__ = Ref(false)
switch_self_profiling(enable = true) = __self_profiling__[] = enable
@inline is_self_profiling() = return __self_profiling__[]

# profile from call expression
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

@nospecialize

function profile_call(f, argtypes::Type...; kwargs...)
    tt = to_tuple_type([typeof′(f), argtypes...])
    interp = JETInterpreter(; kwargs...)
    return profile_gf_by_type!(interp, tt)
end

profile_call(f, argtypes; kwargs...) = profile_call(f, argtypes...; kwargs...)

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

# exports
# -------

export
    profile_file,
    profile_and_watch_file,
    profile_text,
    @profile_call

end
