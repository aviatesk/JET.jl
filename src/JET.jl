@doc read(normpath(dirname(@__DIR__), "README.md"), String)
module JET

const CC = Core.Compiler

@static if !isdefined(CC, :AbstractInterpreter)
    throw(ErrorException("JET.jl only works with Julia versions 1.6 and higher"))
end

# imports
# =======

# `JETInterpreter`
import .CC:
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
    # tfuncs.jl
    builtin_tfunction,
    return_type_tfunc,
    # abstractinterpretation.jl
    abstract_call_gf_by_type,
    abstract_call_method_with_const_args,
    abstract_eval_special_value,
    abstract_eval_value,
    abstract_eval_statement,
    # typeinfer.jl
    typeinf,
    _typeinf,
    typeinf_edge

# `ConcreteInterpreter`
import JuliaInterpreter:
    # virtualprocess.jl
    step_expr!,
    evaluate_call_recurse!,
    handle_err

# usings
# ======
# TODO: really use `using` instead

import Core:
    Builtin,
    MethodMatch,
    LineInfoNode,
    SimpleVector,
    svec

import .CC:
    AbstractInterpreter,
    NativeInterpreter,
    InferenceState,
    InferenceResult,
    CodeInfo,
    InternalCodeCache,
    CodeInstance,
    WorldRange,
    WorldView,
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
    CallMeta,
    is_throw_call,
    tmerge,
    argtypes_to_type,
    _methods_by_ftype,
    specialize_method,
    add_backedge!,
    compute_basic_blocks

import Base:
    parse_input_line,
    unwrap_unionall,
    rewrap_unionall,
    Fix1,
    Fix2,
    IdSet

import Base.Meta:
    isexpr,
    _parse_string,
    lower

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

using InteractiveUtils

# common
# ======

# hooks
# -----

const INIT_HOOKS = Function[]
push_inithook!(f) = push!(INIT_HOOKS, f)
__init__() = foreach(f->f(), INIT_HOOKS)

# macros
# ------

# TODO: upstream these macros into Base

"""
    @invoke f(arg::T, ...; kwargs...)

Provides a convenient way to call [`invoke`](@ref);
`@invoke f(arg1::T1, arg2::T2; kwargs...)` will be expanded into `invoke(f, Tuple{T1,T2}, arg1, arg2; kwargs...)`.
When an argument's type annotation is omitted, it's specified as `Any` argument, e.g.
`@invoke f(arg1::T, arg2)` will be expanded into `invoke(f, Tuple{T,Any}, arg1, arg2)`.

This could be used to call down to `NativeInterpreter`'s abstract interpretation method of
  `f` while passing `JETInterpreter` so that subsequent calls of abstract interpretation
  functions overloaded against `JETInterpreter` can be called from the native method of `f`;
e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                 max_methods::Int)
```
"""
macro invoke(ex)
    f, args, kwargs = destructure_callex(ex)
    arg2typs = map(args) do x
        isexpr(x, :(::)) ? (x.args...,) : (x, GlobalRef(Core, :Any))
    end
    args, argtypes = first.(arg2typs), last.(arg2typs)
    return if isempty(kwargs)
        :($(GlobalRef(Core, :invoke))($(f), Tuple{$(argtypes...)}, $(args...))) # might not be necessary
    else
        :($(GlobalRef(Core, :invoke))($(f), Tuple{$(argtypes...)}, $(args...); $(kwargs...)))
    end |> esc
end

function destructure_callex(ex)
    @assert isexpr(ex, :call) "call expression f(args...; kwargs...) should be given"

    f = first(ex.args)
    args = []
    kwargs = []
    for x in ex.args[2:end]
        if isexpr(x, :parameters)
            append!(kwargs, x.args)
        elseif isexpr(x, :kw)
            push!(kwargs, x)
        else
            push!(args, x)
        end
    end

    return f, args, kwargs
end

"""
    @invokelatest f(args...; kwargs...)

Provides a convenient way to call [`Base.invokelatest`](@ref).
`@invokelatest f(args...; kwargs...)` will simply be expanded into
`Base.invokelatest(f, args...; kwargs...)`.
"""
macro invokelatest(ex)
    f, args, kwargs = destructure_callex(ex)
    return if isempty(kwargs) # eliminates dispatch to kwarg methods, might unnecessary to be special cased
        :($(GlobalRef(Base, :invokelatest))($(f), $(args...)))
    else
        :($(GlobalRef(Base, :invokelatest))($(f), $(args...); $(kwargs...)))
    end |> esc
end

# inference frame
# ---------------

is_constant_propagated(frame::InferenceState) = CC.any(frame.result.overridden_by_const)

# # XXX: should sync with the `haveconst` check within `abstract_call_method_with_const_args` ?
# is_constant_propagated(frame::InferenceState) = is_constant_propagated(frame.result)
# function is_constant_propagated(result::InferenceResult)
#     for a in result.argtypes
#         if CC.has_nontrivial_const_info(a) && CC.const_prop_profitable(a)
#             return true
#         end
#     end
#     return false
# end

istoplevel(linfo::MethodInstance) = linfo.def == __toplevel__
istoplevel(sv::InferenceState)    = istoplevel(sv.linfo)

prewalk_inf_frame(@nospecialize(f), ::Nothing) = return
function prewalk_inf_frame(@nospecialize(f), frame::InferenceState)
    ret = f(frame)
    prewalk_inf_frame(f, frame.parent)
    return ret
end

postwalk_inf_frame(@nospecialize(f), ::Nothing) = return
function postwalk_inf_frame(@nospecialize(f), frame::InferenceState)
    postwalk_inf_frame(f, frame.parent)
    return f(frame)
end

# NOTE: these methods assume `frame` is not inlined
get_cur_pc(frame::InferenceState) = return frame.currpc
get_cur_stmt(frame::InferenceState) = frame.src.code[get_cur_pc(frame)]
get_cur_loc(frame::InferenceState) = frame.src.codelocs[get_cur_pc(frame)]
get_cur_linfo(frame::InferenceState) = frame.src.linetable[get_cur_loc(frame)]
get_cur_varstates(frame::InferenceState) = frame.stmt_types[get_cur_pc(frame)]
get_result(frame::InferenceState) = frame.result.result

# includes
# ========

include("reports.jl")
include("abstractinterpreterinterface.jl")
include("jetcache.jl")
include("tfuncs.jl")
include("abstractinterpretation.jl")
include("typeinfer.jl")
include("print.jl")
include("virtualprocess.jl")
include("watch.jl")

# entry
# =====

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
                      kwargs...)
    included_files, reports, postprocess = collect_reports(profiling_logger,
                                                           mod,
                                                           text,
                                                           filename;
                                                           kwargs...,
                                                           )
    return included_files, print_reports(io, reports, postprocess; kwargs...)
end
profile_text(args...; kwargs...) = profile_text(stdout::IO, args...; kwargs...)

collect_reports(::Nothing, args...; kwargs...) = collect_reports(args...; kwargs...)
function collect_reports(logger::IO, args...; kwargs...)
    print(logger, "profiling from ", #= filename =# last(args), " ...")
    s = time()

    ret = collect_reports(args...; kwargs...)

    sec = round(time() - s; digits = 3)

    print(logger, '\b'^3)
    println(logger, "(finished in $(sec) sec)")

    return ret
end

function collect_reports(actualmod, text, filename; kwargs...)
    virtualmod = gen_virtual_module(actualmod)

    interp = JETInterpreter(; # dummy
                              inf_params      = gen_inf_params(; kwargs...),
                              opt_params      = gen_opt_params(; kwargs...),
                              analysis_params = AnalysisParams(; kwargs...),
                              kwargs...)
    ret, interp = virtual_process!(text,
                                   filename,
                                   virtualmod,
                                   Symbol(actualmod),
                                   interp,
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
module __toplevel__ end

function profile_toplevel!(interp::JETInterpreter, mod::Module, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.uninferred = src
    mi.specTypes = Tuple{}
    mi.def = mod

    result = InferenceResult(mi);
    frame = InferenceState(result, src, #= cached =# true, interp);

    mi.def = __toplevel__ # set to the dummy module

    return profile_frame!(interp, frame)
end

# TODO `profile_call_builtin!` ?
function profile_gf_by_type!(interp::JETInterpreter,
                             @nospecialize(tt::Type{<:Tuple}),
                             world::UInt = get_world_counter(interp),
                             )
    mms = _methods_by_ftype(tt, InferenceParams(interp).MAX_METHODS, world)
    @assert mms !== false "unable to find matching method for $(tt)"

    filter!(mm::MethodMatch->mm.spec_types===tt, mms)
    @assert length(mms) == 1 "unable to find single target method for $(tt)"

    mm = first(mms)::MethodMatch

    return profile_method_signature!(interp, mm.method, mm.spec_types, mm.sparams)
end

function profile_method!(interp::JETInterpreter,
                         m::Method,
                         world::UInt = get_world_counter(interp),
                         )
    return profile_method_signature!(interp, m, m.sig, sparams_from_method_signature(m), world)
end

function sparams_from_method_signature(m)
    s = TypeVar[]
    sig = m.sig
    while isa(sig, UnionAll)
        push!(s, sig.var)
        sig = sig.body
    end
    return svec(s...)
end

function profile_method_signature!(interp::JETInterpreter,
                                   m::Method,
                                   @nospecialize(atype),
                                   sparams::SimpleVector,
                                   world::UInt = get_world_counter(interp),
                                   )
    mi = specialize_method(m, atype, sparams)

    result = InferenceResult(mi)

    frame = InferenceState(result, #= cached =# true, interp)

    return profile_frame!(interp, frame)
end

function profile_frame!(interp::JETInterpreter, frame::InferenceState)
    typeinf(interp, frame)

    # report `throw` calls "appropriately";
    # if the final return type here is `Bottom`-annotated, it _may_ mean the control flow
    # didn't catch some of the `ExceptionReport`s stashed within `interp.exception_reports`,
    if get_result(frame) === Bottom
        if !isempty(interp.exception_reports)
            append!(interp.reports, interp.exception_reports)
        end
    end

    return interp, frame
end

# test, interactive
# =================

# profiles from call expression
macro profile_call(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :profile_call, ex0)
end

function profile_call(@nospecialize(f), @nospecialize(types = Tuple{}); kwargs...)
    ft = Core.Typeof(f)
    if isa(types, Type)
        u = unwrap_unionall(types)
        tt = rewrap_unionall(Tuple{ft, u.parameters...}, types)
    else
        tt = Tuple{ft, types...}
    end

    interp = JETInterpreter(; inf_params      = gen_inf_params(; kwargs...),
                              opt_params      = gen_opt_params(; kwargs...),
                              analysis_params = AnalysisParams(; kwargs...),
                              kwargs...)
    return profile_gf_by_type!(interp, tt)
end

# collects and prints reports from call expression
macro report_call(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_call, ex0)
end

function report_call(@nospecialize(f), @nospecialize(types = Tuple{}); kwargs...)
    interp, frame = profile_call(f, types; kwargs...)
    print_reports(interp.reports; kwargs...)
    return get_result(frame)
end

print_reports(args...; kwargs...) = print_reports(stdout::IO, args...; kwargs...)

# for benchmarking JET analysis performance from call expression
macro benchmark_call(ex0...)
    call_ex = last(ex0)
    profile_ex = InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :profile_call, ex0)
    print_header_ex = quote
        printstyled("[ Benchmark for "; color = :blue)
        printstyled($(QuoteNode(call_ex)); color = TYPE_ANNOTATION_COLOR)
        print(": ")
    end
    return quote let
        $(print_header_ex); println("start analysis ...")
        $(print_header_ex); @time interp, frame = $(profile_ex)
        $(print_header_ex); println("$(length(interp.reports)) errors reported")
        interp, frame
    end end
end

# for inspection
macro lwr(ex) QuoteNode(lower(__module__, ex)) end
macro src(ex) QuoteNode(first(lower(__module__, ex).args)) end

# exports
# =======

export
    profile_file,
    profile_and_watch_file,
    profile_text,
    @profile_call,
    profile_call,
    @report_call,
    report_call

end
