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

macro reportdef(ex)
    T = first(ex.args)
    args = map(ex.args) do x
        # unwrap @nospecialize
        isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize") && (x = last(x.args))
        # handle default arguments
        isexpr(x, :(=)) && return first(x.args)
        return x
    end

    constructor_body = quote
        msg = get_msg(#= T, interp, sv, ... =# $(args...))
        sig = get_sig(#= T, interp, sv, ... =# $(args...))

        cache_report! = generate_report_cacher($(T), msg, sig, #= interp =# $(args[2]))
        st = track_abstract_call_stack!(cache_report!, #= sv =# $(args[3]))

        return new(reverse(st), msg, sig)
    end
    constructor_ex = Expr(:function, ex, constructor_body)

    return quote
        struct $(T) <: InferenceErrorReport
            st::VirtualStackTrace
            msg::String
            sig::Vector{Any}

            # inner constructor (from abstract interpretation)
            $(constructor_ex)

            # inner constructor (from cache)
            function $(T)(st::VirtualStackTrace, msg::AbstractString, sig::AbstractVector)
                return new(reverse(st), msg, sig)
            end
        end
    end
end

@reportdef NoMethodErrorReport(interp, sv, unionsplit)

@reportdef InvalidBuiltinCallErrorReport(interp, sv)

@reportdef UndefVarErrorReport(interp, sv, mod, name)

@reportdef NonBooleanCondErrorReport(interp, sv, @nospecialize(t))

"""
    ExceptionReport <: InferenceErrorReport

Represents general `Exception`s traced during inference. They are reported only when there's
  "inevitable" [`throw`](@ref) calls found by inter-frame analysis.
"""
struct ExceptionReport <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::Vector{Any}

    # the constructors below are needed to be special cased, since `ExceptionReport` is
    # reported _after_ the inference on `sv::InferenceState` has been done rather than
    # during abstract interpretation

    # inner constructor (from abstract interpretation)
    function ExceptionReport(sv::InferenceState, interp, throw_calls)
        msg = get_msg(ExceptionReport, interp, sv, throw_calls)
        sig = get_sig(ExceptionReport, interp, sv, throw_calls)

        cache_report! = generate_report_cacher(ExceptionReport, msg, sig, interp)

        st = let
            local sig = Any["unreachable"]
            file, line = get_file_line(sv.linfo) # just use this frame's location
            frame = (; file, line, sig)
            st = VirtualFrame[frame]
            cache_report!(sv, st)

            isroot(sv) ? st : track_abstract_call_stack!(cache_report!, sv.parent, st) # postwalk
        end

        return new(reverse(st), msg, sig)
    end

    # inner constructor (from cache)
    function ExceptionReport(st::VirtualStackTrace, msg::AbstractString, sig::AbstractVector)
        return new(reverse(st), msg, sig)
    end
end

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other `InferenceErrorReport`s.
"""
@reportdef NativeRemark(interp, sv, s)

function generate_report_cacher(T, msg, sig, interp)
    return function (sv, st)
        key = hash(sv.linfo)
        if haskey(TPCACHE, key)
            _, cached_reports = TPCACHE[key]
        else
            id = get_id(interp)
            cached_reports = InferenceReportCache[]
            TPCACHE[key] = id => cached_reports
        end

        push!(cached_reports, InferenceReportCache{T}(view(st, :), msg, sig))
    end
end

# traces the current abstract call stack
function track_abstract_call_stack!(f::Function, sv::InferenceState, st = VirtualFrame[])
    sig = get_sig(sv)
    file, line = get_file_line(sv)
    frame = (; file, line, sig)

    push!(st, frame)
    f(sv, st)

    isroot(sv) || track_abstract_call_stack!(f, sv.parent, st) # postwalk

    return st
end
track_abstract_call_stack!(sv::InferenceState, st = VirtualFrame[]) =
    track_abstract_call_stack!(dummy_cacher, sv, st)

dummy_cacher(args...) = return

get_file_line(frame::InferenceState) = get_file_line(get_cur_linfo(frame))
get_file_line(linfo::LineInfoNode)   = linfo.file, linfo.line
get_file_line(linfo::MethodInstance) = linfo.def.file, linfo.def.line

# # adapted from https://github.com/JuliaLang/julia/blob/58febaaf2fe38d90d41c170bc2f416a76eac46f5/base/show.jl#L945-L958
# function get_sig(linfo::MethodInstance)
#     io = IOBuffer()
#     def = linfo.def
#     if isa(def, Method)
#         if isdefined(def, :generator) && linfo === def.generator
#             show(io, def)
#         else
#             Base.show_tuple_as_call(io, def.name, linfo.specTypes)
#         end
#     else
#         print(io, "Toplevel MethodInstance thunk")
#     end
#     return String(take!(io))
# end

# entry
get_sig(::Type{<:InferenceErrorReport}, interp, sv, @nospecialize(args...)) = get_sig(sv)
get_sig(sv::InferenceState) = _get_sig(sv, get_cur_stmt(sv))

# special case `ExceptionReport`: there might be multiple throw calls in frame
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
function _get_sig_type(sv::InferenceState, ret::ReturnNode)
    sig = Any["return ", _get_sig(sv, ret.val)...]
    return sig, nothing
end
function _get_sig_type(::InferenceState, qn::QuoteNode)
    typ = typeof(qn.value)
    return Any[string(qn), typ], typ
end
_get_sig_type(::InferenceState, @nospecialize(x)) = Any[repr(x; context = :compact => true)], nothing

get_msg(::Type{NoMethodErrorReport}, interp, sv, unionsplit) = unionsplit ?
    "for one of the union split cases, no matching method found for signature" :
    "no matching method found for call signature"
get_msg(::Type{InvalidBuiltinCallErrorReport}, interp, sv) =
    "invalid builtin function call"
get_msg(::Type{UndefVarErrorReport}, interp, sv, mod, name) = isnothing(mod) ?
    "variable $(name) is not defined" :
    "variable $(mod).$(name) is not defined"
get_msg(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t)) =
    "non-boolean ($(t)) used in boolean context"
function get_msg(::Type{ExceptionReport}, interp, sv, throw_blocks)
    n = length(throw_blocks)
    return if isone(n)
        "will throw"
    else
        "will throw either of"
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
