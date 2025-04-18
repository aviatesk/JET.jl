# InferenceErrorReport
# ====================

# abstract stack trace
# --------------------

"""
    Signature

Represents an expression signature.
`print_signature` implements a frontend functionality to show this type.
"""
struct Signature
    _sig::Vector{Any}
    tt::Union{Type,Nothing}
end

# define equality functions that avoid dynamic dispatches
function Base.:(==)(sig1::Signature, sig2::Signature)
    sig1 = sig1._sig
    sig2 = sig2._sig
    length(sig1) == length(sig2) || return false
    for (a1, a2) in zip(sig1, sig2)
        a1 === a2 || return false
    end
    return true
end
function Base.hash(sig::Signature, h::UInt)
    for a in sig._sig
        h = @invoke hash(a::Any, h::UInt)
    end
    return h
end
Base.iterate(sig::Signature, state...) = iterate(sig._sig, state...)
Base.getindex(sig::Signature, args...) = getindex(sig._sig, args...)
Base.setindex!(sig::Signature, args...) = setindex!(sig._sig, args...)
Base.lastindex(sig::Signature, args...) = lastindex(sig._sig, args...)

"""
    VirtualFrame

Stack information representing virtual execution context:
- `file::Symbol`: the path to the file containing the virtual execution context
- `line::Int`: the line number in the file containing the virtual execution context
- [`sig::Signature`](@ref Signature): a signature of this frame
- `linfo::MethodInstance`: The `MethodInstance` containing the execution context

This type is very similar to `Base.StackTraces.StackFrame`, but its execution context is
collected during abstract interpration, not collected from actual execution.
"""
@withmixedhash struct VirtualFrame
    file::Symbol
    line::Int
    linfo::MethodInstance
end

"""
    VirtualStackTrace

Represents a virtual stack trace in the form of a vector of `VirtualFrame`.
The vector holds `VirtualFrame`s in order of "from entry call site to error point", i.e.
the first element is the `VirtualFrame` of the entry call site, and the last element is that
contains the error.
"""
const VirtualStackTrace = Vector{VirtualFrame}

const INFERENCE_ERROR_REPORT_FIELD_DECLS = [
    :(vst::VirtualStackTrace),
    :(sig::Signature),
]

# get location information at the given program counter (or a current counter if not specified)
function get_virtual_frame(state::StateAtPC)
    file, line = get_file_line(state)
    linfo = isa(state, MethodInstance) ? state : first(state).linfo
    return VirtualFrame(file, line, linfo)
end
get_virtual_frame(sv::InferenceState) = get_virtual_frame((sv, get_currpc(sv)))
get_virtual_frame(caller::InferenceResult) = get_virtual_frame(get_linfo(caller))
function get_virtual_frame(linfo::MethodInstance)
    file, line = get_file_line(linfo)
    return VirtualFrame(file, line, linfo)
end

get_file_line(s::StateAtPC) = get_file_line(get_lin(s)::LineInfoNode)
get_file_line(lin::LineInfoNode) = lin.file, lin.line
function get_file_line(linfo::MethodInstance)
    def = linfo.def
    isa(def, Method) && return def.file, Int(def.line)
    # top-level
    # XXX this is very hacky, remove me
    src = linfo.cache.inferred::CodeInfo
    lin = _get_lin(linfo, src, 1)
    return lin.file, Int(lin.line)
end

# signature
# ---------

@inline get_sig(s::StateAtPC, @nospecialize(x=get_stmt(s))) = Signature(get_sig_nowrap(s, x)...)
get_sig(sv::InferenceState) = get_sig((sv, get_currpc(sv)))

get_sig(mi::MethodInstance) = Signature(Any[mi], mi.specTypes::Type)
get_sig(caller::InferenceResult) = get_sig(get_linfo(caller))

const HandleSigRT = Tuple{Vector{Any}, Union{Type,Nothing}}   # the return type of `handle_sig!`
get_sig_nowrap(s::StateAtPC, @nospecialize(stmt)) = handle_sig!([], s, stmt)::HandleSigRT

function handle_sig!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    head = expr.head
    sig, tt = if head === :call
        handle_sig_call!(sig, s, expr)
    elseif head === :invoke
        handle_sig_invoke!(sig, s, expr)
    elseif head === :(=)
        handle_sig_assignment!(sig, s, expr)
    elseif head === :static_parameter
        handle_sig_static_parameter!(sig, s, expr)
    else
        push!(sig, expr)
        sig, nothing
    end
    return sig, tt
end

function handle_sig_call!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    function splitlast!(list::Vector{Any})
        last = pop!(list)
        return list, last
    end

    f_ssa = f = first(expr.args)
    if f isa SSAValue
        f_ssa = get_stmt((s[1], f.id))
    end
    args = expr.args[2:end]
    splat = false
    if isa(f_ssa, GlobalRef)
        handle_sig_binop!(sig, s, f_ssa, args) && return splitlast!(sig)
        handle_sig_getproperty!(sig, s, f_ssa, args) && return splitlast!(sig)
        handle_sig_setproperty!!(sig, s, f_ssa, args) && return splitlast!(sig)
        handle_sig_getindex!(sig, s, f_ssa, args) && return splitlast!(sig)
        handle_sig_setindex!!(sig, s, f_ssa, args) && return splitlast!(sig)
        handle_sig_const_apply_type!(sig, s, f_ssa, args) && return splitlast!(sig)
        if issplat(f_ssa, args)
            f = args[2]
            args = args[3:end]
            splat = true
        end
    end
    sig, tt = handle_sig_call!(sig, s, f, args, #=splat=#splat)
    return sig, tt
end

# create a type-annotated signature for `([sig of ex]::T)`
macro annotate_if_active(sig, ex)
    sig, ex = esc(sig), esc(ex)
    return quote
        push!($sig, IgnoreMarker())
        i = length($sig)
        $ex
        if last($sig) !== Union{}
            $sig[i] = AnnotationMaker(true)
            push!($sig, AnnotationMaker(false))
        else
            push!($sig, IgnoreMarker())
        end
        $sig
    end
end

function handle_sig_binop!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    (Base.isbinaryoperator(f.name) && length(args) == 2) || return false
    @annotate_if_active sig begin
        handle_sig!(sig, s, args[1])
        t1 = typeof_arg(s, args[1])
        push!(sig, ' ')
        handle_sig!(sig, s, f)
        push!(sig, ' ')
        handle_sig!(sig, s, args[2])
        t2 = typeof_arg(s, args[2])
    end
    push!(sig, safewidenconst(get_ssavaluetype(s)))
    push!(sig, Tuple{typeof_arg(s, f), t1, t2})
    return true
end

function handle_sig_getproperty!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :getproperty || return false
    length(args) == 2 || return false
    sym = args[2]
    isa(sym, QuoteNode) || return false
    val = sym.value
    isa(val, Symbol) || return false
    @annotate_if_active sig handle_sig!(sig, s, args[1])
    push!(sig, '.')
    push!(sig, String(val))
    push!(sig, safewidenconst(get_ssavaluetype(s)))
    push!(sig, Tuple{typeof(getglobal(f.mod, f.name)), typeof_arg(s, args[1]), Symbol})
    return true
end

function handle_sig_setproperty!!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :setproperty! || return false
    length(args) == 3 || return false
    sym = args[2]
    isa(sym, QuoteNode) || return false
    val = sym.value
    isa(val, Symbol) || return false
    @annotate_if_active sig begin
        @annotate_if_active sig handle_sig!(sig, s, args[1])
        push!(sig, '.')
        push!(sig, String(val))
        push!(sig, " = ")
        handle_sig!(sig, s, args[3])
        push!(sig, safewidenconst(get_ssavaluetype(s)))
    end
    push!(sig, Tuple{typeof(getglobal(f.mod, f.name)), typeof_arg(s, args[1]), Symbol, typeof_arg(s, args[3])})
    return true
end

function handle_sig_getindex!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :getindex || return false
    length(args) ≥ 1 || return false
    @annotate_if_active sig handle_sig!(sig, s, args[1])
    push!(sig, '[')
    na = length(args)
    for i = 2:na
        handle_sig!(sig, s, args[i])
        i == na || push!(sig, ", ")
    end
    push!(sig, ']')
    push!(sig, safewidenconst(get_ssavaluetype(s)))
    push!(sig, Tuple{typeof(getglobal(f.mod, f.name)), [typeof_arg(s, arg) for arg in args]...})
    return true
end

function handle_sig_setindex!!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :setindex! || return false
    length(args) ≥ 2 || return false
    @annotate_if_active sig begin
        @annotate_if_active sig handle_sig!(sig, s, args[1])
        push!(sig, '[')
        na = length(args)
        for i = 3:na
            handle_sig!(sig, s, args[i])
            i == na || push!(sig, ", ")
        end
        push!(sig, ']')
        push!(sig, " = ")
        handle_sig!(sig, s, args[2])
        push!(sig, safewidenconst(get_ssavaluetype(s)))
    end
    push!(sig, Tuple{typeof(getglobal(f.mod, f.name)), [typeof_arg(s, arg) for arg in args]...})
    return true
end

function handle_sig_const_apply_type!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :apply_type || return false
    typ = get_ssavaluetype(s)
    isa(typ, Const) || return false
    push!(sig, ApplyTypeResult(typ.val))
    push!(sig, Tuple{typeof(getglobal(f.mod, f.name)), [typeof_arg(s, arg) for arg in args]...})
    return true
end

function issplat(f::GlobalRef, args::Vector{Any})
    f.name === :_apply_iterate || return false
    itf = first(args)
    isa(itf, GlobalRef) || return false
    return itf.name === :iterate
end

function handle_sig_call!(sig::Vector{Any}, s::StateAtPC, @nospecialize(f), args::Vector{Any},
    splat::Bool = false)
    handle_sig!(sig, s, f)
    push!(sig, '(')
    typs = Any[typeof_arg(s, f; callable=true)]
    nargs = length(args)
    for (i, arg) in enumerate(args)
        push!(typs, typeof_arg(s, arg))
        handle_sig!(sig, s, arg)
        if i ≠ nargs
            push!(sig, ", ")
        else
            splat && push!(sig, "...")
        end

    end
    push!(sig, ')')
    push!(sig, safewidenconst(get_ssavaluetype(s)))
    return sig, Tuple{typs...}
end

function handle_sig_invoke!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    f = expr.args[2]
    args = expr.args[3:end]
    sig, tt = handle_sig_call!(sig, s, f, args)
    return sig, tt
end

function handle_sig_assignment!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    sv = first(s)
    if isa(sv, InferenceState)
        lhs = first(expr.args)
        if isa(lhs, SlotNumber)
            name = get_slotname(sv, lhs)
            if name === Symbol("")
                pushfirst!(sig, "@_", lhs.id, " = ")
            else
                pushfirst!(sig, String(name), " = ")
            end
        end
    end
    return handle_sig!(sig, s, last(expr.args))::HandleSigRT
end

function handle_sig_static_parameter!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    i = first(expr.args)::Int
    sv = first(s)
    name = sparam_name((sv.linfo.def::Method).sig::UnionAll, i)
    typ = widenconst(sv.sptypes[i].typ)
    push!(sig, String(name), typ)
    return sig, nothing
end

function sparam_name(u::UnionAll, i::Int)
    while true
        i == 1 && break
        u = u.body::UnionAll
        i -= 1
    end
    return u.var.name
end

function handle_sig!(sig::Vector{Any}, (sv, _)::StateAtPC, ssa::SSAValue)
    newstate = (sv, ssa.id)
    if isa(sv, OptimizationState)
        # when working on `OptimizationState`, the SSA traverse could be really long because
        # of inlining, so just give up for such a case
        typ = safewidenconst(get_ssavaluetype(newstate))
        push!(sig, ssa, typ)
    else
        # XXX the same problem may happen for `InferenceState` too ?
        handle_sig!(sig, newstate, get_stmt(newstate))
    end
    return sig, nothing
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, slot::SlotNumber)
    sv = first(s)
    name = get_slotname(sv, slot)
    if istoplevelframe(sv)
        # this is a abstract global variable, form the global reference
        handle_sig!(sig, s, GlobalRef(sv.linfo.def::Module, name))
        return sig, nothing
    end
    if name === Symbol("")
        repr = slot # fallback if no explicit slotname
    else
        repr = String(name)
    end
    # we can use per-program counter type after inference
    typ = safewidenconst((sv isa InferenceState && CC.is_inferred(sv)) ?
        get_slottype(sv, slot) : get_slottype(s, slot))
    push!(sig, repr, typ)
    return sig, nothing
end

# NOTE `Argument` is introduced by optimization, and so we don't need to handle abstract global variable here
function handle_sig!(sig::Vector{Any}, (sv, _)::StateAtPC, arg::Argument)
    name = get_slotname(sv, arg.n)
    if name === Symbol("")
        repr = SlotNumber(arg.n) # fallback if no explicit slotname
    else
        repr = String(name)
    end
    typ = safewidenconst(get_slottype(sv, arg)) # after optimization we shouldn't use `get_slottype(::StateAtPC, ::Any)`
    push!(sig, repr, typ)
    return sig, nothing
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, gotoifnot::GotoIfNot)
    push!(sig, "goto ", SSAValue(gotoifnot.dest), " if not ")
    handle_sig!(sig, s, gotoifnot.cond)
    return sig, nothing
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, rn::ReturnNode)
    if is_unreachable(rn)
        push!(sig, "unreachable")
    else
        push!(sig, "return ")
        handle_sig!(sig, s, rn.val)
    end
    return sig, nothing
end
is_unreachable(@nospecialize(x)) = isa(x, ReturnNode) && !isdefined(x, :val)

function handle_sig!(sig::Vector{Any}, ::StateAtPC, qn::QuoteNode)
    v = qn.value
    if isa(v, Symbol)
        push!(sig, Repr(v))
        return sig, nothing
    end
    typ = typeof(v)
    push!(sig, qn, typ)
    return sig, nothing
end

# reprs
handle_sig!(sig::Vector{Any}, ::StateAtPC, x::Symbol) = (push!(sig, Repr(x)); return sig, nothing)
handle_sig!(sig::Vector{Any}, ::StateAtPC, x::String) = (push!(sig, Repr(x)); return sig, nothing)

# fallback: GlobalRef, literals...
handle_sig!(sig::Vector{Any}, ::StateAtPC, @nospecialize(x)) = (push!(sig, x); return sig, nothing)

function typeof_arg(s::State, @nospecialize(f); callable::Bool=false)
    isa(f, GlobalRef) && return isdefined(f.mod, f.name) ? Core.Typeof(getglobal(f.mod, f.name)) : Any
    isa(f, SSAValue) && return safewidenconst(get_ssavaluetype((s, f.id)))
    isa(f, Function) && return Core.Typeof(f)
    isa(f, Type) && return Type{f}
    isa(f, QuoteNode) && return Core.Typeof(f.value)
    isexpr(f, :static_parameter) && return Core.Typeof(s.sptypes[first(f.args)::Int])
    callable && error("f ", string(f)::String, " with type ", string(typeof(f)), " not supported")   # FIXME self check runtime dispatch
    return typeof(f)
end
function typeof_arg(s::StateAtPC, @nospecialize(f); kwargs...)
    if isa(f, SlotNumber)
        ret = safewidenconst(get_slottype(s, f))
        ret === Union{} || return ret
        # "broken" calls end up here, e.g., one where an argument (not necessarily this one) is undefined and inference doesn't bother assigning types
        # for the other args
        return TypeUnassigned  # One can't create Tuple{typeof(f), Union{}} so we use a placeholder
    end
    isa(f, Core.Argument) && return safewidenconst(get_slottype(s, f))
    return typeof_arg(first(s), f; kwargs...)
end

# new report
# ----------

"""
    abstract type InferenceErrorReport end

An interface type of error reports collected by JET's abstract interpretation based analysis.
All `InferenceErrorReport`s have the following fields,
which explains _where_ and _how_ this error is reported:
- [`vst::VirtualStackTrace`](@ref VirtualStackTrace): a virtual stack trace of the error
- [`sig::Signature`](@ref Signature): a signature of the error point

Note that some `InferenceErrorReport` may have additional fields other than `vst` and `sig`
to explain _why_ they are reported.
"""
abstract type InferenceErrorReport end

"""
    InferenceErrorReport

In order for `Report <: InferenceErrorReport` to implement the interface,
it should satisfy the following requirements:

- **Required fields** \\
  `Report` should have the following fields,
  which explains _where_ and _how_ this error is reported:
  * `vst::VirtualStackTrace`: a virtual stack trace of the error
  * [`sig::Signature`](@ref Signature): a signature of the error point

  Note that `Report` can have additional fields other than `vst` and `sig` to explain
  _why_ this error is reported (mostly used for [`print_report_message`](@ref)).

- **Required overloads** \\

  * [`JETInterface.copy_report(report::Report) -> new::Report`](@ref copy_report)
  * [`JETInterface.print_report_message(io::IO, report::Report)`](@ref print_report_message)

- **Optional overloads** \\

  * [`JETInterface.print_signature(::Report) -> Bool`](@ref print_signature)
  * [`JETInterface.report_color(::Report) -> Symbol`](@ref report_color)

`Report <: InferenceErrorReport` is supposed to be constructed using the following constructor

    Report(::AbstractAnalyzer, state, spec_args...) -> Report

where `state` can be either of:
- `state::$StateAtPC`: a state with the current program counter specified
- `state::InferenceState`: a state with the current program counter set to `state.currpc`
- `state::InferenceResult`: a state with the current program counter unknown
- `state::MethodInstance`: a state with the current program counter unknown

See also: [`@jetreport`](@ref), [`VirtualStackTrace`](@ref), [`VirtualFrame`](@ref)
"""
function InferenceErrorReport() end

# interfaces
# ----------

"""
    JETInterface.copy_report(orig::Report) where Report<:InferenceErrorReport -> new::Report

Returns new `new::Report`, that should be identical to the original `orig::Report`, except
that `new.vst` is copied from `orig.vst` so that the further modification on `orig.vst`
that may happen in later abstract interpretation doesn't affect `new.vst`.
"""
@noinline copy_report(report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`copy_report(::$(typeof(report)))` is not implemented"))

"""
    JETInterface.print_report_message(io::IO, report::Report) where Report<:InferenceErrorReport

Prints to `io` and describes _why_ `report` is reported.
"""
@noinline print_report_message(io::IO, report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`print_report_message(::IO, ::$(typeof(report)))` is not implemented"))

"""
    JETInterface.print_signature(::Report) where Report<:InferenceErrorReport -> Bool

Configures whether or not to print the report signature when printing `Report` (defaults to `true`).
"""
print_signature(::InferenceErrorReport) = true

"""
    JETInterface.report_color(::Report) where Report<:InferenceErrorReport -> Symbol

Configures the color for `Report` (defaults to `:red`).
"""
report_color(::InferenceErrorReport) = ERROR_COLOR

# common
# ------

# to help inference
@inline function Base.getproperty(@nospecialize(er::InferenceErrorReport), sym::Symbol)
    return if sym === :vst
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :sig
        getfield(er, sym)::Signature
    else
        getfield(er, sym) # fallback
    end
end

# type stable version (assuming all reports satisfy the interface requirements)
function copy_report_stable(@nospecialize report::InferenceErrorReport)
    @static if JET_DEV_MODE
        new = copy_report(report)
        Report = typeof(report)
        if !(new isa Report)
            error(lazy"""
            bad `$InferenceErrorReport` interface:
            `$copy_report(::$Report)` should return new `$Report`.
            See the documentation of `$InferenceErrorReport` and `$copy_report`
            """)
        end
        new = new::InferenceErrorReport
        if report.vst === new.vst
            error(lazy"""
            bad `$InferenceErrorReport` interface:
            `$copy_report(report::$Report).vst` should be a copy of `report.vst`.
            See the documentation of `$InferenceErrorReport` and `$copy_report`
            """)
        end
        return new
    else
        return copy_report(report)::InferenceErrorReport
    end
end

function Base.show(io::IO, report::InferenceErrorReport)
    print(io, typeof(report).name.name, '(')
    print_report(io, report)
    print(io, ')')
end

# the default constructor to create a report from abstract interpretation
function (Report::Type{<:InferenceErrorReport})(state, @nospecialize(spec_args...))
    vf = get_virtual_frame(state)
    vst = VirtualFrame[vf]
    sig = get_sig(state)
    return Report(vst, sig, spec_args...)
end

# utility
# -------

"""
    reportkey(report::InferenceErrorReport)

Returns an identifier for the runtime-dispatched call site of `report`.

If you have a long list of reports to analyze, `urpts = unique(reportkey, rpts)` may remove "duplicates"
that arrive at the same runtime dispatch from different entry points.
"""
reportkey(report::InferenceErrorReport) = (report.sig.tt, report.vst[end].linfo)

# TODO parametric definition?

"""
    @jetreport struct NewReport <: InferenceErrorReport
        ...
    end

A utility macro to define [`InferenceErrorReport`](@ref).
It can be very tedious to manually satisfy the `InferenceErrorReport` interfaces.
JET internally uses this `@jetreport` utility macro, which takes a `struct` definition of
`InferenceErrorReport` without the required fields specified, and automatically defines
the `struct` as well as constructor definitions.
If the report `NewReport <: InferenceErrorReport` is defined using `@jetreport`,
then `NewReport` just needs to implement the `print_report_message` interface.

For example, [`JETAnalyzer`](@ref)'s `MethodErrorReport` is defined as follows:
```julia
@jetreport struct MethodErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
end
function print_report_message(io::IO, (; t, union_split)::MethodErrorReport)
    print(io, "no matching method found for ")
    if union_split == 0
        print_callsig(io, t)
    else
        ts = t::Vector{Any}
        nts = length(ts)
        for i = 1:nts
            print_callsig(io, ts[i])
            i == nts || print(io, ", ")
        end
        print(io, " (", nts, '/', union_split, " union split)")
    end
end
```
and constructed as like `MethodErrorReport(sv::InferenceState, atype::Any, 0)`.
"""
macro jetreport(ex)
    @assert @capture(ex, struct NewReport_ <: Super_; spec_sigs__; end)
    @assert Core.eval(__module__, Super) <: InferenceErrorReport

    spec_decls = Any[]
    for i in 1:length(spec_sigs)
        x = spec_sigs[i]
        if isexpr(x, :macrocall) && x.args[1] === Symbol("@nospecialize")
            push!(spec_decls, x.args[3])
        elseif isexpr(x, :(=))
            push!(spec_decls, x.args[1])
            spec_sigs[i] = Expr(:kw, x.args[1], x.args[2])
        else
            push!(spec_decls, x)
        end
    end
    spec_names = extract_decl_name.(spec_decls)
    spec_types = esc.(extract_decl_type.(spec_decls))

    NewReport, Super = esc(NewReport), esc(Super)

    # copy_report
    copy_report = let
        sig = :($(GlobalRef(JETInterface, :copy_report))(report::$NewReport))
        call = :($NewReport(copy(report.vst), report.sig))
        for name in spec_names
            push!(call.args, :(getproperty(report, $(QuoteNode(name)))))
        end
        Expr(:function, sig, Expr(:block, __source__, call))
    end

    return quote
        Base.@__doc__ struct $NewReport <: $Super
            $(INFERENCE_ERROR_REPORT_FIELD_DECLS...)
            $(map(esc, spec_decls)...)
            # esc is needed here since signanture might be `@nospecialize`d
            function $NewReport($(INFERENCE_ERROR_REPORT_FIELD_DECLS...), $(map(esc, spec_sigs)...))
                new($(extract_decl_name.(INFERENCE_ERROR_REPORT_FIELD_DECLS)...), $(map(esc, spec_names)...))
            end
        end

        $copy_report
    end
end

extract_decl_name(@nospecialize(x)) = (isexpr(x, :(::)) ? first(x.args) : x)::Symbol
extract_decl_type(@nospecialize(x)) = isexpr(x, :(::)) ? last(x.args) : GlobalRef(Core, :Any)
