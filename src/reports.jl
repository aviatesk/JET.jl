# toplevel
# --------

abstract type ToplevelErrorReport end

# `ToplevelErrorReport` interface
function Base.getproperty(er::ToplevelErrorReport, sym::Symbol)
    return if sym === :file
        getfield(er, sym)::String
    elseif sym === :line
        getfield(er, sym)::Int
    else
        getfield(er, sym) # fallback
    end
end

struct SyntaxErrorReport <: ToplevelErrorReport
    err::ErrorException
    file::String
    line::Int
    SyntaxErrorReport(msg::AbstractString, file, line) = new(ErrorException(msg), file, line)
end

struct RecursiveIncludeErrorReport <: ToplevelErrorReport
    duplicated_file::String
    files::Set{String}
    file::String
    line::Int
end

# wraps general errors from actual execution
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    st::Vector{Base.StackTraces.StackFrame}
    file::String
    line::Int

    # default constructor
    ActualErrorWrapped(err, st, file, line) = new(err, st, file, line)

    # bypass syntax error
    function ActualErrorWrapped(err::ErrorException, st, file, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, file, line)
        else
            new(err, st, file, line)
        end
    end
end

# inference
# ---------

# TODO: maybe we want to use https://github.com/aviatesk/Mixin.jl
abstract type InferenceErrorReport end

const VirtualFrame = @NamedTuple begin
    file::Symbol
    line::Int
    sig::Vector{Any}
end
const VirtualStackTrace = Vector{VirtualFrame}
const ViewedVirtualStackTrace = typeof(view(VirtualStackTrace(), :))

"""
    const VirtualFrame = @NamedTuple begin
        file::Symbol
        line::Int
        sig::Vector{Any}
    end
    const VirtualStackTrace = Vector{VirtualFrame}
    const ViewedVirtualStackTrace = typeof(view(VirtualStackTrace(), :))

- `VirtualStackTrace` represents virtual back trace of profiled errors (supposed to be
  ordered from call site to error point
- `ViewedVirtualStackTrace` is view of `VirtualStackTrace` and will be kept in [`TPCACHE`](@ref)
"""
VirtualFrame, VirtualStackTrace, ViewedVirtualStackTrace

# `InferenceErrorReport` interface
function Base.getproperty(er::InferenceErrorReport, sym::Symbol)
    return if sym === :st
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :msg
        getfield(er, sym)::String
    elseif sym === :sig
        getfield(er, sym)::Vector{Any}
    else
        getfield(er, sym) # fallback
    end
end

macro reportdef(ex, kwargs...)
    T = first(ex.args)
    args = map(ex.args) do x
        # unwrap @nospecialize
        if isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize")
            x = esc(last(x.args)) # `esc` is needed because `@nospecialize` escapes its argument anyway
        end
        return x
    end
    spec_args = args[4:end] # keep those additional, specific fields

    local track_from_frame::Bool = false
    local dont_cache::Bool = false
    for ex in kwargs
        @assert isexpr(ex, :(=))
        kw, val = ex.args
        if kw === :track_from_frame
            track_from_frame = val
        elseif kw === :dont_cache
            dont_cache = val
        end
    end

    sv = args[3]
    # `from_statement` should be used when report is constructed _during_ the inference
    from_statement = :(track_abstract_call_stack!(cache_report!, $(sv)))
    # `from_frame` should be used when report is constructed _after_ the inference on
    # `sv::InferenceState` has been done
    from_frame = :(let
        local sig = get_sig($(sv).linfo)
        file, line = get_file_line($(sv).linfo) # just use this frame's location
        frame = (; file, line, sig)
        st = VirtualFrame[frame]
        cache_report!($(sv), st)

        isroot($(sv)) ? st : track_abstract_call_stack!(cache_report!, $(sv).parent, st) # postwalk
    end)

    function strip_type_annotations(x)
        isexpr(x, :escape) && return Expr(:escape, strip_type_annotations(first(x.args))) # keep escape
        return isexpr(x, :(::)) ? first(x.args) : x
    end
    args′ = strip_type_annotations.(args)
    spec_args′ = strip_type_annotations.(spec_args)
    constructor_body = quote
        msg = get_msg(#= T, interp, sv, ... =# $(args′...))
        sig = get_sig(#= T, interp, sv, ... =# $(args′...))

        cache_report! = if $(dont_cache)
            dummy_cacher
        else
            gen_report_cacher(msg, sig, #= T, interp, sv, ... =# $(args′...))
        end
        st = $(track_from_frame ? from_frame : from_statement)

        return new(reverse(st), msg, sig, $(spec_args′...))
    end
    constructor_ex = Expr(:function, ex, constructor_body)

    return quote
        struct $(T) <: InferenceErrorReport
            st::VirtualStackTrace
            msg::String
            sig::Vector{Any}
            $(spec_args...)

            # inner constructor (from abstract interpretation)
            $(constructor_ex)

            # inner constructor (from cache)
            function $(T)(st::VirtualStackTrace, msg::AbstractString, sig::AbstractVector, @nospecialize(spec_args::NTuple{N, Any} where N))
                return new(reverse(st), msg, sig, $(esc(:(spec_args...)))) # `esc` is needed because `@nospecialize` escapes its argument anyway
            end
        end
    end
end

@reportdef NoMethodErrorReport(interp, sv, unionsplit::Bool, @nospecialize(atype::Type), linfo::MethodInstance)

@reportdef NoMethodErrorReportConst(interp, sv, unionsplit::Bool, @nospecialize(atype::Type), linfo::MethodInstance) dont_cache = true

@reportdef InvalidBuiltinCallErrorReport(interp, sv)

@reportdef GlobalUndefVarErrorReport(interp, sv, mod::Module, name::Symbol)

@reportdef LocalUndefVarErrorReport(interp, sv, name::Symbol) track_from_frame = true

@reportdef NonBooleanCondErrorReport(interp, sv, @nospecialize(t::Type))

"""
    ExceptionReport <: InferenceErrorReport

Represents general `Exception`s traced during inference. They are reported only when there's
  "inevitable" [`throw`](@ref) calls found by inter-frame analysis.
"""
@reportdef ExceptionReport(interp, sv, throw_calls::Vector{Any}) track_from_frame = true

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other `InferenceErrorReport`s.
"""
@reportdef NativeRemark(interp, sv, s::String)

function gen_report_cacher(msg, sig, T, interp, #= sv =# _, args...)
    return function (sv, st)
        key = sv.linfo
        if haskey(TPCACHE, key)
            _, cached_reports = TPCACHE[key]
        else
            id = get_id(interp)
            cached_reports = InferenceReportCache[]
            TPCACHE[key] = id => cached_reports
        end

        push!(cached_reports, InferenceReportCache{T}(view(st, :), msg, sig, args))
    end
end

# traces the current abstract call stack
function track_abstract_call_stack!(@nospecialize(cacher), sv::InferenceState, st = VirtualFrame[])
    walker = let cacher = cacher, st = st
        function (frame)
            sig = get_sig(frame)
            file, line = get_file_line(frame)
            virtualframe = (; file, line, sig)
            push!(st, virtualframe)

            cacher(frame, st)
        end
    end
    prewalk_inf_frame(walker, sv)

    return st
end
track_abstract_call_stack!(sv::InferenceState, st = VirtualFrame[]) =
    track_abstract_call_stack!(dummy_cacher, sv, st)

function prewalk_inf_frame(@nospecialize(f), frame::InferenceState)
    ret = f(frame)
    isroot(frame) || prewalk_inf_frame(f, frame.parent)
    return ret
end
function postwalk_inf_frame(@nospecialize(f), frame::InferenceState)
    isroot(frame) || prewalk_inf_frame(f, frame.parent)
    return f(frame)
end

dummy_cacher(args...) = return

get_file_line(frame::InferenceState) = get_file_line(get_cur_linfo(frame))
get_file_line(linfo::LineInfoNode)   = linfo.file, linfo.line
get_file_line(linfo::MethodInstance) = linfo.def.file, linfo.def.line

# adapted from https://github.com/JuliaLang/julia/blob/519b04e4ada9b07c85427e303d3ce4c823a0310f/base/show.jl#L974-L987
function get_sig(l::MethodInstance)
    def = l.def
    ret = if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            # print(io, "MethodInstance generator for ")
            # show(io, def)
            sprint(show, def)
        else
            # print(io, "MethodInstance for ")
            # show_tuple_as_call(io, def.name, l.specTypes)
            sprint(Base.show_tuple_as_call, def.name, l.specTypes)
        end
    else
        # print(io, "Toplevel MethodInstance thunk")
        "toplevel MethodInstance thunk"
    end
    return Any[ret]
end

# entry
get_sig(::Type{<:InferenceErrorReport}, interp, sv, @nospecialize(args...)) = get_sig(sv)
get_sig(sv::InferenceState) = _get_sig(sv, get_cur_stmt(sv))

# special cased entries
get_sig(::Type{LocalUndefVarErrorReport}, interp, sv, name) = Any[""] # TODO
function get_sig(::Type{ExceptionReport}, interp, sv, throw_calls)
    sig = Any[]
    ncalls = length(throw_calls)
    for (i, call) in enumerate(throw_calls)
        call_sig = _get_sig(sv, call)
        append!(sig, call_sig)
        i ≠ ncalls && push!(sig, ", ")
    end
    return sig
end

_get_sig(args...) = first(_get_sig_type(args...))

function _get_sig_type(sv::InferenceState, expr::Expr)
    head = expr.head
    return if head === :call
        f = first(expr.args)
        args = expr.args[2:end]
        nargs = length(args)

        sig = _get_sig(sv, f)
        push!(sig, '(')
        for (i, arg) in enumerate(args)
            arg_sig = _get_sig(sv, arg)
            append!(sig, arg_sig)
            i ≠ nargs && push!(sig, ", ")
        end
        push!(sig, ')')

        sig, nothing
    elseif head === :(=)
        _get_sig_type(sv, last(expr.args))
    elseif head === :static_parameter
        typ = widenconst(sv.sptypes[first(expr.args)])
        Any['_', typ], typ
    else
        Any[string(expr)], nothing
    end
end
function _get_sig_type(sv::InferenceState, ssa::SSAValue)
    sig, sig_typ = _get_sig_type(sv, sv.src.code[ssa.id])
    typ = widenconst(sv.src.ssavaluetypes[ssa.id])
    sig_typ == typ || push!(sig, typ)
    return sig, typ
end
function _get_sig_type(sv::InferenceState, slot::SlotNumber)
    sig = string(sv.src.slotnames[slot.id])
    if isempty(sig)
        sig = string(slot) # fallback if no explicit slotname
    end
    typ = widenconst(get_cur_varstates(sv)[slot.id].typ)
    return Any[sig, typ], typ
end
_get_sig_type(::InferenceState, gr::GlobalRef) = Any[string(gr.mod, '.', gr.name)], nothing
function _get_sig_type(sv::InferenceState, gotoifnot::GotoIfNot)
    sig  = Any[string("goto %", gotoifnot.dest, " if not "), _get_sig(sv, gotoifnot.cond)...]
    return sig, nothing
end
function _get_sig_type(sv::InferenceState, rn::ReturnNode)
    sig = is_unreachable(rn) ? Any["unreachable"] : Any["return ", _get_sig(sv, rn.val)...]
    return sig, nothing
end
function _get_sig_type(::InferenceState, qn::QuoteNode)
    typ = typeof(qn.value)
    return Any[string(qn), typ], typ
end
_get_sig_type(::InferenceState, @nospecialize(x)) = Any[repr(x; context = :compact => true)], nothing

get_msg(::Type{<:Union{NoMethodErrorReport,NoMethodErrorReportConst}}, interp, sv, unionsplit, @nospecialize(atype), linfo) = unionsplit ?
    "for one of the union split cases, no matching method found for signature: " :
    "no matching method found for call signature: "
get_msg(::Type{InvalidBuiltinCallErrorReport}, interp, sv) =
    "invalid builtin function call: "
get_msg(::Type{GlobalUndefVarErrorReport}, interp, sv, mod, name) =
    "variable $(mod).$(name) is not defined: "
get_msg(::Type{LocalUndefVarErrorReport}, interp, sv, name) =
    "variable $(name) may raise UndefVarError"
get_msg(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t)) =
    "non-boolean ($(t)) used in boolean context: "
function get_msg(::Type{ExceptionReport}, interp, sv, throw_blocks)
    n = length(throw_blocks)
    return if isone(n)
        "will throw: "
    else
        "will throw either of: "
    end
end
get_msg(::Type{NativeRemark}, interp, sv, s) = s

# utils
# -----

# NOTE: these methods assume `frame` is not inlined

get_cur_pc(frame::InferenceState) = return frame.currpc
get_cur_stmt(frame::InferenceState) = frame.src.code[get_cur_pc(frame)]
get_cur_loc(frame::InferenceState) = frame.src.codelocs[get_cur_pc(frame)]
get_cur_linfo(frame::InferenceState) = frame.src.linetable[get_cur_loc(frame)]
get_cur_varstates(frame::InferenceState) = frame.stmt_types[get_cur_pc(frame)]
get_result(frame::InferenceState) = frame.result.result
isroot(frame::InferenceState) = isnothing(frame.parent)
