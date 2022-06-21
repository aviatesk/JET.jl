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
@inline Base.iterate(sig::Signature, state...) = iterate(sig._sig, state...)

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
    sig::Signature
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
    sig = get_sig(state)
    file, line = get_file_line(state)
    linfo = isa(state, MethodInstance) ? state : first(state).linfo
    return VirtualFrame(file, line, sig, linfo)
end
get_virtual_frame(sv::InferenceState) = get_virtual_frame((sv, get_currpc(sv)))
get_virtual_frame(caller::InferenceResult) = get_virtual_frame(get_linfo(caller))
function get_virtual_frame(linfo::MethodInstance)
    sig = get_sig(linfo)
    file, line = get_file_line(linfo)
    return VirtualFrame(file, line, sig, linfo)
end

get_file_line(s::StateAtPC) = get_file_line(get_lin(s))
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
            sprint(show_tuple_as_call, def.name, l.specTypes)
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
    return Signature(Any[ret])
end

@inline function show_tuple_as_call(io::IO, name::Symbol, @nospecialize(sig::Type))
    @static if hasmethod(Base.show_tuple_as_call, (IO, Symbol, Type), (:demangle, :kwargs, :argnames, :qualified))
        Base.show_tuple_as_call(io, name, sig; qualified = true)
    else
        Base.show_tuple_as_call(io, name, sig, false, nothing, nothing, true)
    end
end

@inline get_sig(s::StateAtPC) = get_sig(s, get_stmt(s))
@inline get_sig(s::StateAtPC, @nospecialize(x)) = Signature(_get_sig(s, x))
@inline _get_sig(@nospecialize args...) = first(_get_sig_type(args...))

struct SignatureType
    sig::Vector{Any}
    typ
    SignatureType(sig::Vector{Any}, @nospecialize typ) = new(sig, typ)
end
Base.iterate(sigtype::SignatureType, state=1) =
    return state > 2 ? nothing : (getfield(sigtype, state), state+1)

# to help inference
_get_sig_type(@nospecialize args...) = __get_sig_type(args...)::SignatureType

function _get_callsig(s::StateAtPC, @nospecialize(f), args::Vector{Any};
                      splat::Bool = false)
    sig = _get_sig(s, f)
    push!(sig, '(')

    nargs = length(args)
    for (i, arg) in enumerate(args)
        arg_sig = _get_sig(s, arg)
        append!(sig, arg_sig)
        if i ≠ nargs
            push!(sig, ", ")
        else
            splat && push!(sig, "...")
        end
    end
    push!(sig, ')')

    return sig
end

function __get_sig_type(s::StateAtPC, expr::Expr)
    head = expr.head
    if head === :call
        f = first(expr.args)
        args = expr.args[2:end]

        # special case splat call signature
        if isa(f, GlobalRef) && f.name === :_apply_iterate && begin
                itf = first(args)
                isa(itf, GlobalRef) && itf.name === :iterate
            end
            f = args[2]
            args = args[3:end]
            return SignatureType(_get_callsig(s, f, args; splat = true), nothing)
        else
            return SignatureType(_get_callsig(s, f, args), nothing)
        end
    elseif head === :invoke
        f = expr.args[2]
        args = expr.args[3:end]
        return SignatureType(_get_callsig(s, f, args), nothing)
    elseif head === :(=)
        sigtyp = _get_sig_type(s, last(expr.args))
        sv = first(s)
        if isa(sv, InferenceState)
            lhs = first(expr.args)
            if isa(lhs, SlotNumber)
                name = sv.src.slotnames[slot_id(lhs)]
                pushfirst!(first(sigtyp), string(name), " = ")
                return sigtyp
            end
        end
        return sigtyp
    elseif head === :static_parameter
        typ = widenconst(first(s).sptypes[first(expr.args)::Int])
        return SignatureType(Any['_', typ], typ)
    else
        return SignatureType(Any[string(expr)], nothing)
    end
end
function __get_sig_type((sv, _)::StateAtPC, ssa::SSAValue)
    news = (sv, ssa.id)
    if isa(sv, OptimizationState)
        # when working on `OptimizationState`, the SSA traverse could be really long because
        # of inlining, so just give up for such a case
        typ = widenconst(ignorelimited(ignorenotfound(get_ssavaluetype(news))))
        sig = Any["%$(ssa.id)", typ]
    else
        # XXX the same problem may happen for `InferenceState` too ?
        sig, sig_typ = _get_sig_type(news, get_stmt(news))
        typ = widenconst(ignorelimited(ignorenotfound(get_ssavaluetype(news))))
        # NOTE this additional `typ` would print e.g. return type of an inner call, e.g. `f(getproperty(x, :y)::typ)`
        sig_typ == typ || push!(sig, typ)
    end
    return SignatureType(sig, typ)
end
function __get_sig_type(s::StateAtPC, slot::SlotNumber)
    sv = first(s)
    name = get_slotname(sv, slot)
    sig = string(name)
    if isempty(sig)
        sig = string(slot) # fallback if no explicit slotname
    end
    if istoplevel(sv)
        # this is a abstract global variable, form the global reference
        return _get_sig_type(s, GlobalRef(sv.linfo.def::Module, name))
    else
        # we can use per-program counter type after inference
        t = (isa(sv, InferenceState) && sv.inferred) ? get_slottype(sv, slot) : get_slottype(s, slot)
        typ = widenconst(ignorelimited(t))
        return SignatureType(Any[sig, typ], typ)
    end
end
# NOTE `Argument` is introduced by optimization, and so we don't need to handle abstract global variable here, etc.
function __get_sig_type((sv, _)::StateAtPC, arg::Argument)
    name = get_slotname(sv, arg.n)
    sig = string(name)
    typ = widenconst(ignorelimited(get_slottype(sv, arg))) # after optimization we shouldn't use `get_slottype(::StateAtPC, ::Any)`
    return SignatureType(Any[sig, typ], typ)
end
__get_sig_type(_::StateAtPC, gr::GlobalRef) = SignatureType(Any[string(gr.mod, '.', gr.name)], nothing)
__get_sig_type(_::StateAtPC, name::Symbol) = SignatureType(Any[repr(name; context = :compact => true)], nothing)
function __get_sig_type(s::StateAtPC, gotoifnot::GotoIfNot)
    sig  = Any[string("goto %", gotoifnot.dest, " if not "), _get_sig(s, gotoifnot.cond)...]
    return SignatureType(sig, nothing)
end
function __get_sig_type(s::StateAtPC, rn::ReturnNode)
    sig = is_unreachable(rn) ? Any["unreachable"] : Any["return ", _get_sig(s, rn.val)...]
    return SignatureType(sig, nothing)
end
function __get_sig_type(::StateAtPC, qn::QuoteNode)
    typ = typeof(qn.value)
    return SignatureType(Any[string(qn), typ], typ)
end
# TODO don't use `repr` here -- only stringify safe primitives
function __get_sig_type(::StateAtPC, @nospecialize(x))
    return SignatureType(Any[repr(x; context = :compact => true)], nothing)
end

# new report
# ----------

"""
    InferenceErrorReport

An interface type of error reports that JET collects by abstract interpration.
If `T` implements this interface, the following requirements should be satisfied:

---
- **Required fields** \\
  `T` should have the following fields, which explains _where_ and _how_ this error is reported:
  * `vst::VirtualStackTrace`: a virtual stack trace of the error
  * [`sig::Signature`](@ref Signature): a signature of the error point

  Note that `T` can still have arbitrary additional fields other than `vst` and `sig`
  to explain _why_ this error is reported (mostly used for `print_report`).
---
- **A constructor to create `T` from abstraction interpretation** \\
  `T<:InferenceErrorReport` has the default constructor

      T(::AbstractAnalyzer, state, spec_args...)

  which can take following `state`s:
  where `state` can be either of:
  - `state::$StateAtPC`: a state with the current program counter specified
  - `state::InferenceState`: a state with the current program counter set to `state.currpc`
  - `state::InferenceResult`: a state with the current program counter unknown
  - `state::MethodInstance`: a state with the current program counter unknown
---
- **A contructor to create `T` from the global report cache** \\
  In order to be cached and restored from [`JET_CACHE`](@ref), `T` _**must**_ implement
  the following interfaces:
  * `JET.get_spec_args(::T) -> Vector{Any}`: returns additional fields that are specific to `T`
  * `_restore_cached_report(::Type{T}, vst::VirtualStackTrace, sig::Signature spec_args::Vector{Any}) -> T`:
    constructor to create `T` from the cache, which _should_ copy `vst` and expand `spec_args` into each specific field
---
- **A error message printer** \\
  `T` should also implement `print_report_message(io::IO, report::T)`,
  which prints to `io` and describes _why_ it is reported.
  `T` can optionally overload:
  - `print_signature(::T) -> Bool` to configure whether or not to print the report signature
     (defaults to `true`)
  - `report_color(::T) -> Symbol` to configure the printed color for `T` (defaults to `:red`)
---

See also: [@reportdef](@ref), [`VirtualStackTrace`](@ref), [`VirtualFrame`](@ref)
"""
abstract type InferenceErrorReport end

print_report_message(io::IO, report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`print_report_message(::IO, ::$(typeof(report)))` is not implemented"))
print_signature(::InferenceErrorReport) = true
report_color(::InferenceErrorReport) = ERROR_COLOR

# to help inference
function Base.getproperty(er::InferenceErrorReport, sym::Symbol)
    return if sym === :vst
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :sig
        getfield(er, sym)::Signature
    else
        getfield(er, sym) # fallback
    end
end

function Base.show(io::IO, report::InferenceErrorReport)
    print(io, typeof(report).name.name, '(')
    print_report(io, report)
    print(io, ')')
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", report::InferenceErrorReport) =
    return report

# the default constructor to create a report from abstract interpretation
function (T::Type{<:InferenceErrorReport})(state, @nospecialize(spec_args...))
    vf = get_virtual_frame(state)
    return T([vf], vf.sig, spec_args...)
end

get_spec_args(report::InferenceErrorReport) = (@nospecialize;
    error(lazy"`get_spec_args(::$(typeof(report)))` is not implemented"))

# cache
# -----

@eval struct InferenceErrorReportCache
    T::DataType
    $(INFERENCE_ERROR_REPORT_FIELD_DECLS...)
    spec_args::Vector{Any}
end

function cache_report!(cache, @nospecialize(report#=::InferenceErrorReport=#))
    vst = copy(report.vst)
    new = InferenceErrorReportCache(typeof(report)::DataType, vst, report.sig, get_spec_args(report))
    return push!(cache, new)
end

restore_cached_report(cache::InferenceErrorReportCache) = _restore_cached_report(
    cache.T, copy(cache.vst), cache.sig, cache.spec_args)::InferenceErrorReport
function _restore_cached_report end

# utility
# -------

# TODO parametric definition?

"""
    @reportdef struct T <: InferenceErrorReport
        ...
    end

An utilitiy macro to define [`InferenceErrorReport`](@ref).
It can be very tedious to manually satisfy the `InferenceErrorReport` interfaces.
JET internally uses this `@reportdef` utility macro, which takes a `struct` definition of
`InferenceErrorReport` without the required fields specified, and automatically defines
the `struct` as well as constructor definitions.
If the report `T <: InferenceErrorReport` is defined using `@reportdef`,
then `T` just needs to implement the `print_report_message` interface.

For example, [`JETAnalyzer`](@ref)'s `NoMethodErrorReport` is defined as follows:
```julia
@reportdef struct NoMethodErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
    union_split::Int
end
function print_report_message(io::IO, (; t, union_split)::NoMethodErrorReport)
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
and constructed as like `NoMethodErrorReport(sv::InferenceState, atype::Any, 0)`.
"""
macro reportdef(ex)
    @assert @capture(ex, struct T_ <: S_; spec_sigs__; end)
    @assert Core.eval(__module__, S) <: InferenceErrorReport

    spec_decls = map(spec_sigs) do x
        if isexpr(x, :macrocall) && x.args[1] === Symbol("@nospecialize")
            return x.args[3]
        end
        return x
    end
    spec_names = extract_decl_name.(spec_decls)
    spec_types = esc.(extract_decl_type.(spec_decls))

    T, S = esc(T), esc(S)

    # cache constructor
    cache_constructor_sig = :($(GlobalRef(JET, :_restore_cached_report))(
        ::Type{$T}, vst::VirtualStackTrace, sig::Signature, spec_args::Vector{Any}))
    cache_constructor_call = :($T(vst, sig))
    for (i, spec_type) in enumerate(spec_types)
        push!(cache_constructor_call.args, :(spec_args[$i]::$spec_type))
    end
    cache_constructor = Expr(:function, cache_constructor_sig, Expr(:block, __source__,
        :(return @inbounds $cache_constructor_call),
    ))

    # cache helper
    spec_getter_sig = :($(GlobalRef(JET, :get_spec_args))(report::$T))
    spec_getter_body = Expr(:block, __source__)
    nspecs = length(spec_names)
    push!(spec_getter_body.args, :(spec_args = Vector{Any}(undef, $nspecs)))
    for i in 1:nspecs
        spec_name = spec_names[i]
        push!(spec_getter_body.args, :(spec_args[$i] = getproperty(report, $(QuoteNode(spec_name)))))
    end
    push!(spec_getter_body.args, :(return spec_args))
    spec_getter = Expr(:function, spec_getter_sig, spec_getter_body)

    return quote
        Base.@__doc__ struct $T <: $S
            $(INFERENCE_ERROR_REPORT_FIELD_DECLS...)
            $(map(esc, spec_decls)...)
            # esc is needed here since signanture might be `@nospecialize`d
            function $T($(INFERENCE_ERROR_REPORT_FIELD_DECLS...), $(map(esc, spec_sigs)...))
                new($(extract_decl_name.(INFERENCE_ERROR_REPORT_FIELD_DECLS)...), $(map(esc, spec_names)...))
            end
        end

        $cache_constructor

        $spec_getter
    end
end

extract_decl_name(@nospecialize(x)) = (isexpr(x, :(::)) ? first(x.args) : x)::Symbol
extract_decl_type(@nospecialize(x)) = isexpr(x, :(::)) ? last(x.args) : GlobalRef(Core, :Any)
