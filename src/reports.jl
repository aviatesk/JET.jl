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

# to help inference
function Base.getproperty(er::InferenceErrorReport, sym::Symbol)
    return if sym === :st
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :msg
        getfield(er, sym)::String
    elseif sym === :sig
        getfield(er, sym)::Vector{Any}
    elseif sym === :lineage
        getfield(er, sym)::Lineage
    else
        getfield(er, sym) # fallback
    end
end

function Base.show(io::IO, report::T) where {T<:InferenceErrorReport}
    print(io, T.name.name, '(')
    for a in report.sig
        _print_signature(io, a; annotate_types = true, bold = true)
    end
    print(io, ')')
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", report::T) where {T<:InferenceErrorReport} =
    return report

struct VirtualFrame
    file::Symbol
    line::Int
    sig::Vector{Any}
    linfo::MethodInstance
end
function Base.hash(vf::VirtualFrame, h::UInt)
    h = @static UInt === UInt64 ? 0x140ec41731a857a7 : 0x6e6397bc
    h = hash(vf.file, h)
    h = hash(vf.line, h)
    h = hash(vf.sig, h)
    h = hash(vf.linfo, h)
    return h
end
function Base.:(==)(vf1::VirtualFrame, vf2::VirtualFrame)
    return vf1.file === vf2.file &&
           vf1.line == vf2.line &&
           vf1.sig == vf2.sig &&
           vf1.linfo == vf2.linfo
end

# `InferenceErrorReport` is supposed to keep its virtual stack trace in order of
# "from entry call site to error point"
const VirtualStackTrace = Vector{VirtualFrame}

struct LineageKey
    file::Symbol
    line::Int
    linfo::MethodInstance
end
const Lineage = Set{LineageKey}
function Base.hash(lk::LineageKey, h::UInt)
    h = @static UInt === UInt64 ? 0x895379e76333a606 : 0x3879844d
    h = hash(lk.file, h)
    h = hash(lk.line, h)
    h = hash(lk.linfo, h)
    return h
end
function Base.:(==)(lk1::LineageKey, lk2::LineageKey)
    return lk1.file === lk2.file &&
           lk1.line == lk2.line &&
           lk1.linfo == lk2.linfo
end

get_lineage_key(frame::InferenceState) = LineageKey(get_file_line(frame)..., frame.linfo)
get_lineage_key(vf::VirtualFrame)      = LineageKey(vf.file, vf.line, vf.linfo)

function is_lineage(lineage::Lineage, parent::InferenceState, linfo::MethodInstance)
    # check if current `linfo` is in `lineage`
    # NOTE: we can't use `get_lineage_key` for this `linfo`, just because we don't analyze
    # on cached frames and thus no appropriate lineage key (i.e. program counter) exists
    for lk in lineage
        lk.linfo === linfo && return is_lineage(lineage, parent)
    end
    return false
end
function is_lineage(lineage::Lineage, frame::InferenceState)
    get_lineage_key(frame) in lineage || return false
    return is_lineage(lineage, frame.parent)
end
is_lineage(::Lineage, ::Nothing) = true

# `ViewedVirtualStackTrace` is for `InferenceErrorReportCache` and only keeps a part of the
# stack trace of the original `InferenceErrorReport`, in the order of "from cached frame to error point"
const ViewedVirtualStackTrace = typeof(view(VirtualStackTrace(), 1:0))

struct InferenceErrorReportCache
    T::Type{<:InferenceErrorReport}
    st::ViewedVirtualStackTrace
    msg::String
    sig::Vector{Any}
    spec_args::NTuple{N,Any} where N
end

function cache_report!(report::T, linfo, cache) where {T<:InferenceErrorReport}
    st = report.st
    i = findfirst(vf->vf.linfo==linfo, st)
    # sometimes `linfo` can't be found within the `report.st` chain; e.g. frames for inner
    # constructor methods doesn't seem to be tracked in the `(frame::InferenceState).parent`
    # chain so that there is no `MethodInstance` within `report.st` for such a frame;
    # XXX: reports from these frames might need to be cached as well rather than just giving up
    i === nothing && return
    st = view(st, i:length(st))
    new = InferenceErrorReportCache(T, st, report.msg, report.sig, spec_args(report))
    push!(cache, new)
end

function restore_cached_report!(cache::InferenceErrorReportCache,
                                interp#=::JETInterpreter=#,
                                caller::InferenceState,
                                )
    report = restore_cached_report(cache, caller)
    push!(isa(report, ExceptionReport) ? interp.exception_reports : interp.reports, report)
    return
end

function restore_cached_report(cache::InferenceErrorReportCache,
                               caller::InferenceState,
                               )
    T = cache.T
    msg = cache.msg
    sig = cache.sig
    st = collect(cache.st)
    spec_args = cache.spec_args
    lineage = Lineage(get_lineage_key(vf) for vf in st)

    prewalk_inf_frame(caller) do frame::InferenceState
        linfo = frame.linfo
        vf = get_virtual_frame(frame)
        pushfirst!(st, vf)
        push!(lineage, get_lineage_key(vf))
    end

    return T(st, msg, sig, lineage, spec_args)
end

macro reportdef(ex, kwargs...)
    T = esc(first(ex.args))
    args = map(ex.args) do x
        # unwrap @nospecialize
        if isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize")
            x = esc(last(x.args)) # `esc` is needed because `@nospecialize` escapes its argument anyway
        end
        return x
    end
    spec_args = args[4:end] # keep those additional, specific fields

    local track_from_frame::Bool = false
    for ex in kwargs
        @assert isexpr(ex, :(=))
        kw, val = ex.args
        if kw === :track_from_frame
            track_from_frame = val
        end
    end

    args′ = strip_type_decls.(args)
    spec_args′ = strip_type_decls.(spec_args)
    interp_constructor = Expr(:function, ex, Expr(:block, __source__, quote
        interp = $(args′[2])
        sv = $(args′[3])

        msg = get_msg(#= T, interp, sv, ... =# $(args′...))
        sig = get_sig(#= T, interp, sv, ... =# $(args′...))
        st = VirtualFrame[]
        lineage = Lineage()

        @static $(track_from_frame) && let
            # when report is constructed _after_ the inference on `sv::InferenceState` has been done,
            # collect location information from `sv.linfo` and start traversal from `sv.parent`
            linfo = sv.linfo
            vf = get_virtual_frame(linfo)
            push!(st, vf)
            push!(lineage, get_lineage_key(vf))
            sv = sv.parent
        end

        prewalk_inf_frame(sv) do frame::InferenceState
            vf = get_virtual_frame(frame)
            pushfirst!(st, vf)
            push!(lineage, get_lineage_key(vf))
        end

        return new(st, msg, sig, lineage, $(spec_args′...))
    end))

    spec_args_unescaped = strip_escape.(spec_args′)
    spec_args_types = extract_type_decls.(spec_args)

    return Expr(:block, __source__, quote
        struct $(T) <: InferenceErrorReport
            st::VirtualStackTrace
            msg::String
            sig::Vector{Any}
            lineage::Lineage
            $(spec_args...)

            # constructor from abstract interpretation process by `JETInterpreter`
            $(interp_constructor)

            # constructor from cache
            function $(T)(st::VirtualStackTrace,
                          msg::AbstractString,
                          sig::AbstractVector,
                          lineage::Lineage,
                          @nospecialize(spec_args::NTuple{N,Any} where N),
                          )
                return new(st, msg, sig, lineage, $(esc(:(spec_args...)))) # `esc` is needed because `@nospecialize` escapes its argument anyway
            end
        end

        function $(esc(:spec_args))(report::$(T))
            gn = (getproperty(report, spec_arg) for spec_arg in ($(QuoteNode.(spec_args_unescaped)...),))
            return (gn...,)::Tuple{$(spec_args_types...)}
        end
    end)
end

function strip_type_decls(x)
    isexpr(x, :escape) && return Expr(:escape, strip_type_decls(first(x.args))) # keep escape
    return isexpr(x, :(::)) ? first(x.args) : x
end

strip_escape(x) = isexpr(x, :escape) ? first(x.args) : x
extract_type_decls(x) = isexpr(x, :(::)) ? last(x.args) : Any

@reportdef NoMethodErrorReport(interp, sv, unionsplit::Bool, @nospecialize(atype::Type))

@reportdef InvalidBuiltinCallErrorReport(interp, sv, argtypes::Vector{Any})

@reportdef NoFieldErrorReport(interp, sv, @nospecialize(typ::Type), name::Symbol)

@reportdef GlobalUndefVarErrorReport(interp, sv, mod::Module, name::Symbol)

@reportdef LocalUndefVarErrorReport(interp, sv, name::Symbol) track_from_frame = true

@reportdef NonBooleanCondErrorReport(interp, sv, @nospecialize(t::Type))

@reportdef InvalidConstantRedefinition(interp, sv, mod::Module, name::Symbol, @nospecialize(t′), @nospecialize(t))

"""
    ExceptionReport <: InferenceErrorReport

Represents general `Exception`s traced during inference. They are reported only when there's
  "inevitable" [`throw`](@ref) calls found by inter-frame analysis.
"""
:(ExceptionReport)
@reportdef ExceptionReport(interp, sv, throw_calls::Vector{Any}) track_from_frame = true

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.

!!! note
    Currently JET.jl doesn't make any use of `NativeRemark`.
"""
:(NativeRemark)
@reportdef NativeRemark(interp, sv, s::String)

function get_virtual_frame(loc::Union{InferenceState,MethodInstance})::VirtualFrame
    sig = get_sig(loc)
    file, line = get_file_line(loc)
    linfo = isa(loc, InferenceState) ? loc.linfo : loc
    return VirtualFrame(file, line, sig, linfo)
end

get_file_line(frame::InferenceState) = get_file_line(get_cur_linfo(frame))
get_file_line(linfo::LineInfoNode)   = linfo.file, linfo.line
# this location is not exact, but this is whay we know at best
function get_file_line(linfo::MethodInstance)::Tuple{Symbol,Int}
    def = linfo.def

    isa(def, Method) && return def.file, def.line

    # toplevel
    src = linfo.uninferred::CodeInfo
    file = first(unique(map(lin->lin.file, src.linetable)))::Symbol
    line = minimum(lin->lin.line, src.linetable)::Int
    return file, line
end

# adapted from https://github.com/JuliaLang/julia/blob/0f11a7bb07d2d0d8413da05dadd47441705bf0dd/base/show.jl#L989-L1011
function get_sig(l::MethodInstance)
    def = l.def
    ret = if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            # print(io, "MethodInstance generator for ")
            # show(io, def)
            sprint(show, def)
        else
            # print(io, "MethodInstance for ")
            # show_tuple_as_call(io, def.name, l.specTypes, false, nothing, nothing, true)
            sprint(Base.show_tuple_as_call, def.name, l.specTypes, false, nothing, nothing, true)
        end
    else
        # print(io, "Toplevel MethodInstance thunk")
        # # `thunk` is not very much information to go on. If this
        # # MethodInstance is part of a stacktrace, it gets location info
        # # added by other means.  But if it isn't, then we should try
        # # to print a little more identifying information.
        # if !from_stackframe
        #     linetable = l.uninferred.linetable
        #     line = isempty(linetable) ? "unknown" : (lt = linetable[1]; string(lt.file) * ':' * string(lt.line))
        #     print(io, " from ", def, " starting at ", line)
        # end
        "toplevel"
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

        # special case splat call signature
        if isa(f, GlobalRef) && f.name === :_apply_iterate && begin
                itf = first(args)
                isa(itf, GlobalRef) && itf.name === :iterate
            end
            f = args[2]
            args = args[3:end]

            sig = _get_sig(sv, f)
            push!(sig, '(')

            nargs = length(args)
            for (i, arg) in enumerate(args)
                arg_sig = _get_sig(sv, arg)
                append!(sig, arg_sig)
                i ≠ nargs ? push!(sig, ", ") : push!(sig, "...)")
            end
        else
            sig = _get_sig(sv, f)
            push!(sig, '(')

            nargs = length(args)
            for (i, arg) in enumerate(args)
                arg_sig = _get_sig(sv, arg)
                append!(sig, arg_sig)
                i ≠ nargs && push!(sig, ", ")
            end
            push!(sig, ')')
        end

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
function _get_sig_type(sv::InferenceState, s::Symbol)
    # for toplevel frame, we need to resolve symbols to global references by ourselves
    istoplevel(sv) && return _get_sig_type(sv, GlobalRef(sv.mod, s))
    return Any[repr(s; context = :compact => true)], nothing
end
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

get_msg(::Type{NoMethodErrorReport}, interp, sv, unionsplit, @nospecialize(args...)) = unionsplit ?
    "for one of the union split cases, no matching method found for signature" :
    "no matching method found for call signature"
get_msg(::Type{InvalidBuiltinCallErrorReport}, interp, sv, @nospecialize(args...)) =
    "invalid builtin function call"
get_msg(::Type{NoFieldErrorReport}, interp, sv, @nospecialize(typ), name) =
    "type $(typ) has no field $(name)"
get_msg(::Type{GlobalUndefVarErrorReport}, interp, sv, mod, name) =
    "variable $(mod).$(name) is not defined"
get_msg(::Type{LocalUndefVarErrorReport}, interp, sv, name) =
    "local variable $(name) is not defined"
get_msg(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t)) =
    "non-boolean ($(t)) used in boolean context"
get_msg(::Type{InvalidConstantRedefinition}, interp, sv, mod, name, @nospecialize(t′), @nospecialize(t)) =
    "invalid redefinition of constant $(mod).$(name) (from $(t′) to $(t))"
get_msg(::Type{ExceptionReport}, interp, sv, throw_blocks) = isone(length(throw_blocks)) ?
    "may throw" :
    "may throw either of"
get_msg(::Type{NativeRemark}, interp, sv, s) = s
