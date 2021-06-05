# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

When `T` implements this interface (i.e. `T <: AbstractAnalyzer`), `T` is expected to implement:
- `T(; jetconfigs...) -> T`:
  constructs new analyzer given [JET configurations](@ref);
  `AnalyzerState` for this analyzer should be constructed using these configurations
- `AnalyzerState(analyzer::T) -> AnalyzerState`:
  returns `AnalyzerState` instance, usually it's kept within `T` itself
- `AbstractAnalyzer(analyzer::T, state::AnalyzerState) -> T`:
  constructs new analyzer given the previous analyzer and analysis state
- `ReportPass(analyzer::T) -> ReportPass`:
  returns the [report pass](@ref ReportPass) of `T`

For example, JET.jl defines `JETAnalyzer <: AbstractAnalyzer` as the following (modified a bit for the sake of simplicity):
```julia
# the default abstract interpreter for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API requirements

function JETAnalyzer(;
    report_pass::Union{Nothing,T} = nothing,
    mode::Symbol                  = :basic,
    jetconfigs...) where {T<:ReportPass}
    if isnothing(report_pass)
        # if `report_pass` isn't passed explicitly, here we configure it according to `mode`
        report_pass = mode === :basic ? BasicPass() :
                      mode === :sound ? SoundPass() :
                      throw(ArgumentError("`mode` configuration should be either of `:basic` or `:sound`"))
    end
    return JETAnalyzer(report_pass,
                       AnalyzerState(; jetconfigs...),
                       )
end
AnalyzerState(analyzer::JETAnalyzer)                          = analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) = JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer)                             = analyzer.report_pass
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

# ReportPass
# ==========

"""
    ReportPass

An interface type for report passes of JET's analysis.
"""
abstract type ReportPass end

# pre-defined passes
# ------------------

"""
    SoundPass <: ReportPass

`ReportPass` for the sound JET analysis.
"""
struct SoundPass <: ReportPass end

"""
    BasicPass <: ReportPass

`ReportPass` for the basic (default) JET analysis.
"""
struct BasicPass <: ReportPass end

# `SoundPass` is still WIP, we will use it to implement both passes at once for the meantime
const SoundBasicPass = Union{SoundPass,BasicPass}

# ToplevelErrorReport
# ===================

"""
    ToplevelErrorReport

An interface type of error reports that JET collects while top-level concrete interpration.
All `ToplevelErrorReport` should have the following fields:
- `file::String`: the path to the file containing the interpretation context
- `line::Int`: the line number in the file containing the interpretation context

See also: [`virtual_process`](@ref), [`ConcreteInterpreter`](@ref)
"""
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

# InferenceErrorReport
# ====================

# fields
# ------

"""
    VirtualFrame

Stack information representing virtual execution context:
- `file::Symbol`: the path to the file containing the virtual execution context
- `line::Int`: the line number in the file containing the virtual execution context
- `sig::Vector{Any}`: a signature of this frame
- `linfo::MethodInstance`: The `MethodInstance` containing the execution context

This type is very similar to `Base.StackTraces.StackFrame`, but its execution context is
collected during abstract interpration, not collected from actual execution.
"""
@withmixedhash struct VirtualFrame
    file::Symbol
    line::Int
    sig::Vector{Any}
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
    :(msg::String),
    :(sig::Vector{Any}),
]

"""
    InferenceErrorReport

An interface type of error reports that JET collects by abstract interpration.
If `T` implements this interface, the following requirements should be satisfied:

---
- **Required fields** \\
  `T` should have the following fields, which explains _where_ and _why_ this error is reported:
  * `vst::VirtualStackTrace`: a virtual stack trace of the error
  * `msg::String`: explains why this error is reported
  * `sig::Vector{Any}`: a signature of the error point

  Note that `T` can still have additional, specific fields.
---
- **A constructor interface to create `T` from abstraction interpretation** \\
  `T<:InferenceErrorReport` has the default constructor

      T(::AbstractAnalyzer, sv::InferenceState, spec_args...)

  which works when `T` is reported when `sv`'s program counter (`sv.currpc`) points to that
  of statement where the error may happen. If so `T` just needs to overload

      JET.get_msg(::Type{T}, ::AbstractAnalyzer, ::InferenceState, spec_args...) -> msg::String

  to provide the message that describes why this error is reported (otherwise the senseless
  default message will be used).

  ---

  If `T` is reported when `sv`'s program counter (`sv.currpc`) may not point to the error
  location or even `sv::InferenceState` isn't available, `T` can implement its own constructor method.
---
- **A contructor interface to create `T` from the global report cache** \\
  In order to be cached and restored from [`JET_REPORT_CACHE`](@ref), `T` _**must**_ implement
  the following interfaces:
  * `JET.get_spec_args(::T) -> Tuple{...}`:
    returns fields that are specific to `T`, which is internally used by the caching logic
  * `T(vst::VirtualStackTrace, msg::String, sig::Vector{Any} spec_args::Tuple{...}) -> T`:
    constructor to create `T` from the cache, which should expand `spec_args` into each specific field
---

To satisfy these requirements manually will be very tedious.
JET internally uses `@reportdef` utility macro, which takes the `struct` definition of
`InferenceErrorReport` and automatically defines the `struct` itself and the cache interfaces.

See also: [`VirtualStackTrace`](@ref), [`VirtualFrame`](@ref)
"""
abstract type InferenceErrorReport end

# to help inference
function Base.getproperty(er::InferenceErrorReport, sym::Symbol)
    return if sym === :vst
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :msg
        getfield(er, sym)::String
    elseif sym === :sig
        getfield(er, sym)::Vector{Any}
    elseif sym === :lin # only needed for SeriousExceptionReport
        getfield(er, sym)::LineInfoNode
    else
        getfield(er, sym) # fallback
    end
end

function Base.show(io::IO, report::T) where {T<:InferenceErrorReport}
    print(io, T.name.name, '(')
    for a in report.sig
        _print_signature(io, a, (; annotate_types = true); bold = true)
    end
    print(io, ')')
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", report::T) where {T<:InferenceErrorReport} =
    return report

get_msg(T::Type{<:InferenceErrorReport}, @nospecialize(_...)) = throw("`get_msg` is not implemented for $T")
get_spec_args(T::Type{<:InferenceErrorReport}) =                throw("`get_spec_args` is not implemented for $T")

# default constructor to create a report from abstract interpretation routine
function (T::Type{<:InferenceErrorReport})(analyzer::AbstractAnalyzer, state, @nospecialize(spec_args...))
    vf = get_virtual_frame(analyzer, state)
    msg = get_msg(T, analyzer, state, spec_args...)
    return T([vf], msg, vf.sig, spec_args...)
end

# virtual frame

function get_virtual_frame(analyzer::AbstractAnalyzer, loc::Union{InferenceState,MethodInstance})
    sig = get_sig(analyzer, loc)
    file, line = get_file_line(loc)
    linfo = isa(loc, InferenceState) ? loc.linfo : loc
    return VirtualFrame(file, line, sig, linfo)
end

# get location information at the given program counter
function get_virtual_frame(analyzer::AbstractAnalyzer, (sv, pc)::Tuple{InferenceState,Int})
    sig = get_sig(analyzer, sv, get_stmt(sv, pc))
    file, line = get_file_line(sv, pc)
    linfo = sv.linfo
    return VirtualFrame(file, line, sig, linfo)
end

get_file_line(frame::InferenceState, pc = get_currpc(frame)) = get_file_line(get_lin(frame, pc))

# this location is not exact, but this is whay we know at best
function get_file_line(linfo::MethodInstance)
    def = linfo.def

    isa(def, Method) && return def.file, Int(def.line)

    # top-level
    src = linfo.uninferred::CodeInfo
    return get_file_line(first(src.linetable::Vector)::LineInfoNode)
end

get_file_line(lin::LineInfoNode) = lin.file, lin.line

# signature

# adapted from https://github.com/JuliaLang/julia/blob/0f11a7bb07d2d0d8413da05dadd47441705bf0dd/base/show.jl#L989-L1011
function get_sig(analyzer::AbstractAnalyzer, l::MethodInstance)
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
    return Any[ret]
end

@inline function show_tuple_as_call(io::IO, name::Symbol, @nospecialize(sig::Type))
    @static if hasmethod(Base.show_tuple_as_call, (IO, Symbol, Type), (:demangle, :kwargs, :argnames, :qualified))
        Base.show_tuple_as_call(io, name, sig; qualified = true)
    else
        Base.show_tuple_as_call(io, name, sig, false, nothing, nothing, true)
    end
end

get_sig(analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(x = get_stmt(sv))) = _get_sig(analyzer, sv, x)

@inline _get_sig(@nospecialize(args...)) = first(_get_sig_type(args...))::Vector{Any}

function _get_callsig(analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(f), args::Vector{Any};
                      splat::Bool = false)
    sig = _get_sig(analyzer, sv, f)
    push!(sig, '(')

    nargs = length(args)
    for (i, arg) in enumerate(args)
        arg_sig = _get_sig(analyzer, sv, arg)
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

function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, expr::Expr)
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
            return _get_callsig(analyzer, sv, f, args; splat = true), nothing
        else
            return _get_callsig(analyzer, sv, f, args), nothing
        end
    elseif head === :invoke
        f = expr.args[2]
        args = expr.args[3:end]
        return _get_callsig(analyzer, sv, f, args), nothing
    elseif head === :(=)
        return _get_sig_type(analyzer, sv, last(expr.args))
    elseif head === :static_parameter
        typ = widenconst(sv.sptypes[first(expr.args)])
        return Any['_', typ], typ
    else
        return Any[string(expr)], nothing
    end
end
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, ssa::SSAValue)
    sig, sig_typ = _get_sig_type(analyzer, sv, sv.src.code[ssa.id])
    typ = widenconst(ignorelimited(ignorenotfound(sv.src.ssavaluetypes[ssa.id])))
    sig_typ == typ || push!(sig, typ)
    return sig, typ
end
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, slot::SlotNumber)
    name = get_slotname(sv, slot)
    sig = string(name)
    if isempty(sig)
        sig = string(slot) # fallback if no explicit slotname
    end
    if istoplevel(analyzer, sv)
        # this is a abstract global variable, form the global reference
        return _get_sig_type(analyzer, sv, GlobalRef(get_toplevelmod(analyzer), name))
    else
        typ = widenconst(ignorelimited(get_slottype(sv, slot)))
        return Any[sig, typ], typ
    end
end
# NOTE `Argument` only appears after optimization
# and so we don't need to handle abstract global variable here, etc.
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, arg::Argument)
    name = get_slotname(sv, arg.n)
    sig = string(name)
    typ = widenconst(ignorelimited(sv.slottypes[arg.n])) # NOTE after optimization, and so we can't use `get_slottype` here
    return Any[sig, typ], typ
end
_get_sig_type(analyzer::AbstractAnalyzer, ::InferenceState, gr::GlobalRef) = Any[string(gr.mod, '.', gr.name)], nothing
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, s::Symbol)
    if istoplevel(analyzer, sv)
        # this is concrete global variable, form the global reference
        return _get_sig_type(analyzer, sv, GlobalRef(get_toplevelmod(analyzer), s))
    else
        return Any[repr(s; context = :compact => true)], nothing
    end
end
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, gotoifnot::GotoIfNot)
    sig  = Any[string("goto %", gotoifnot.dest, " if not "), _get_sig(analyzer, sv, gotoifnot.cond)...]
    return sig, nothing
end
function _get_sig_type(analyzer::AbstractAnalyzer, sv::InferenceState, rn::ReturnNode)
    sig = is_unreachable(rn) ? Any["unreachable"] : Any["return ", _get_sig(analyzer, sv, rn.val)...]
    return sig, nothing
end
function _get_sig_type(analyzer::AbstractAnalyzer, ::InferenceState, qn::QuoteNode)
    typ = typeof(qn.value)
    return Any[string(qn), typ], typ
end
_get_sig_type(analyzer::AbstractAnalyzer, ::InferenceState, @nospecialize(x)) = Any[repr(x; context = :compact => true)], nothing

# cache
# -----

@eval struct InferenceErrorReportCache
    T::Type{<:InferenceErrorReport}
    $(INFERENCE_ERROR_REPORT_FIELD_DECLS...)
    spec_args::NTuple{N,Any} where N
end

function cache_report!(cache, report::T) where {T<:InferenceErrorReport}
    vst = copy(report.vst)
    new = InferenceErrorReportCache(T, vst, report.msg, report.sig, get_spec_args(report))
    return push!(cache, new)
end

restore_cached_report(cache::InferenceErrorReportCache) =
    cache.T(copy(cache.vst), cache.msg, cache.sig, cache.spec_args)::InferenceErrorReport

# utility
# -------

# a simple utility macro to define `InferenceErrorReport` w/o code duplications
macro reportdef(ex)
    @assert @capture(ex, struct T_ <: S_; spec_sigs__; end)
    @assert Core.eval(__module__, S) <: InferenceErrorReport

    spec_decls = map(spec_sigs) do x
        if @isexpr(x, :macrocall) && x.args[1] === Symbol("@nospecialize")
            return x.args[3]
        end
        return x
    end
    spec_names = extract_decl_name.(spec_decls)
    spec_types = esc.(extract_decl_type.(spec_decls))

    T = esc(T)

    # cache constructor
    cache_constructor_sig = :($T(vst::VirtualStackTrace,
                                 msg::String,
                                 sig::Vector{Any},
                                 @nospecialize(spec_args::Tuple),
                                 ))
    cache_constructor_call = :($T(vst, msg, sig))
    for (i, spec_type) in enumerate(spec_types)
        push!(cache_constructor_call.args, :($(esc(:spec_args))[$i]::$spec_type)) # needs escape since `@nospecialize`d
    end
    cache_constructor = Expr(:function, cache_constructor_sig, Expr(:block, __source__,
        :(return @inbounds $cache_constructor_call),
    ))

    # cache helper
    spec_getter_sig = :($(GlobalRef(JET, :get_spec_args))(report::$T))
    spec_getter_tuple = Expr(:tuple)
    for spec_name in spec_names
        getter = Expr(:call, GlobalRef(Base, :getproperty), :report, QuoteNode(spec_name))
        push!(spec_getter_tuple.args, getter)
    end
    spec_getter = Expr(:function, spec_getter_sig, Expr(:block, __source__,
        :(return $spec_getter_tuple::Tuple{$(spec_types...)}),
    ))

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

extract_decl_name(@nospecialize(x)) = (@isexpr(x, :(::)) ? first(x.args) : x)::Symbol
extract_decl_type(@nospecialize(x)) = @isexpr(x, :(::)) ? last(x.args) : GlobalRef(Core, :Any)
