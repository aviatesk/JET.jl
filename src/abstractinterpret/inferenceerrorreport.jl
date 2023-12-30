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
    src = linfo.uninferred::CodeInfo
    return get_file_line(first(src.linetable::LineTable)::LineInfoNode)
end

# signature
# ---------

@inline get_sig(s::StateAtPC, @nospecialize(x=get_stmt(s))) = Signature(get_sig_nowrap(s, x))
get_sig(sv::InferenceState) = get_sig((sv, get_currpc(sv)))

get_sig(mi::MethodInstance) = Signature(Any[mi])
get_sig(caller::InferenceResult) = get_sig(get_linfo(caller))

function get_sig_nowrap(@nospecialize args...)
    sig = Any[]
    handle_sig!(sig, args...)
    return sig
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    head = expr.head
    if head === :call
        handle_sig_call!(sig, s, expr)
    elseif head === :invoke
        handle_sig_invoke!(sig, s, expr)
    elseif head === :(=)
        handle_sig_assignment!(sig, s, expr)
    elseif head === :static_parameter
        handle_sig_static_parameter!(sig, s, expr)
    else
        push!(sig, expr)
    end
    return sig
end

function handle_sig_call!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    f = first(expr.args)
    args = expr.args[2:end]
    splat = false
    if isa(f, GlobalRef)
        handle_sig_binop!(sig, s, f, args) && return sig
        handle_sig_getproperty!(sig, s, f, args) && return sig
        handle_sig_setproperty!!(sig, s, f, args) && return sig
        handle_sig_getindex!(sig, s, f, args) && return sig
        handle_sig_setindex!!(sig, s, f, args) && return sig
        handle_sig_const_apply_type!(sig, s, f, args) && return sig
        if issplat(f, args)
            f = args[2]
            args = args[3:end]
            splat = true
        end
    end
    handle_sig_call!(sig, s, f, args, #=splat=#splat)
    return sig
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
        push!(sig, ' ')
        handle_sig!(sig, s, f)
        push!(sig, ' ')
        handle_sig!(sig, s, args[2])
    end
    push!(sig, safewidenconst(get_ssavaluetype(s)))
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
    return true
end

function handle_sig_const_apply_type!(sig::Vector{Any}, s::StateAtPC, f::GlobalRef, args::Vector{Any})
    f.name === :apply_type || return false
    typ = get_ssavaluetype(s)
    isa(typ, Const) || return false
    push!(sig, ApplyTypeResult(typ.val))
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
    nargs = length(args)
    for (i, arg) in enumerate(args)
        handle_sig!(sig, s, arg)
        if i ≠ nargs
            push!(sig, ", ")
        else
            splat && push!(sig, "...")
        end
    end
    push!(sig, ')')
    push!(sig, safewidenconst(get_ssavaluetype(s)))
    return sig
end

function handle_sig_invoke!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    f = expr.args[2]
    args = expr.args[3:end]
    handle_sig_call!(sig, s, f, args)
    return sig
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
    handle_sig!(sig, s, last(expr.args))
    return sig
end

function handle_sig_static_parameter!(sig::Vector{Any}, s::StateAtPC, expr::Expr)
    i = first(expr.args)::Int
    sv = first(s)
    name = sparam_name((sv.linfo.def::Method).sig::UnionAll, i)
    typ = widenconst(sv.sptypes[i].typ)
    push!(sig, String(name), typ)
    return sig
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
    return sig
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, slot::SlotNumber)
    sv = first(s)
    name = get_slotname(sv, slot)
    if istoplevel(sv)
        # this is a abstract global variable, form the global reference
        handle_sig!(sig, s, GlobalRef(sv.linfo.def::Module, name))
        return sig
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
    return sig
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
    return sig
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, gotoifnot::GotoIfNot)
    push!(sig, "goto ", SSAValue(gotoifnot.dest), " if not ")
    handle_sig!(sig, s, gotoifnot.cond)
    return sig
end

function handle_sig!(sig::Vector{Any}, s::StateAtPC, rn::ReturnNode)
    if is_unreachable(rn)
        push!(sig, "unreachable")
    else
        push!(sig, "return ")
        handle_sig!(sig, s, rn.val)
    end
    return sig
end
is_unreachable(@nospecialize(x)) = isa(x, ReturnNode) && !isdefined(x, :val)

function handle_sig!(sig::Vector{Any}, ::StateAtPC, qn::QuoteNode)
    v = qn.value
    if isa(v, Symbol)
        push!(sig, Repr(v))
        return sig
    end
    typ = typeof(v)
    push!(sig, qn, typ)
    return sig
end

# reprs
handle_sig!(sig::Vector{Any}, ::StateAtPC, x::Symbol) = (push!(sig, Repr(x)); return sig)
handle_sig!(sig::Vector{Any}, ::StateAtPC, x::String) = (push!(sig, Repr(x)); return sig)

# fallback: GlobalRef, literals...
handle_sig!(sig::Vector{Any}, ::StateAtPC, @nospecialize(x)) = (push!(sig, x); return sig)

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

  * [`copy_report(report::Report) -> new::Report`](@ref copy_report)
  * [`print_report_message(io::IO, report::Report)`](@ref print_report_message)

- **Optional overloads** \\

  * [`print_signature(::Report) -> Bool`](@ref print_signature)
  * [`report_color(::Report) -> Symbol`](@ref report_color)

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
    copy_report(report::Report) where Report<:InferenceErrorReport -> new::Report

Returns new `new::Report`, that should be identical to the original `report::Report`, except
that `new.vst` is copied from `report.vst` so that the further modification on `report.vst`
that may happen in later abstract interpretation doesn't affect `new.vst`.
"""
@noinline copy_report(report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`copy_report(::$(typeof(report)))` is not implemented"))

"""
    print_report_message(io::IO, report::Report) where Report<:InferenceErrorReport

Prints to `io` and describes _why_ `report` is reported.
"""
@noinline print_report_message(io::IO, report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`print_report_message(::IO, ::$(typeof(report)))` is not implemented"))

"""
    print_signature(::Report) where Report<:InferenceErrorReport -> Bool

Configures whether or not to print the report signature when printing `Report` (defaults to `true`).
"""
print_signature(::InferenceErrorReport) = true

"""
    report_color(::Report) where Report<:InferenceErrorReport -> Symbol

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

# type stable version (assuming it satisfies the interface correctly)
function copy_report′(@nospecialize report::InferenceErrorReport)
    @static if JET_DEV_MODE
        new = copy_report(report)
        Report = typeof(report)
        if !isa(new, Report)
            error(lazy"""
            bad `$InferenceErrorReport` interface:
            `$copy_report(::$Report)` should return new `$Report`.
            See the documentation of `$InferenceErrorReport` and `$copy_report`
            """)
        end
        new = new::InferenceErrorReport
        if report.vst === new.vst && (@static JET_DEV_MODE ? report.vst == new.vst : true)
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
        sig = :($(GlobalRef(JET, :copy_report))(report::$NewReport))
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
