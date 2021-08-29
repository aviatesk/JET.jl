# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type of analyzers that are built on top of [JET's analyzer framework](@ref JET-Analyzer-Framework).

When a new type `NewAnalyzer` implements the `AbstractAnalyzer` interface, it should be declared
as subtype of `AbstractAnalyzer`, and is expected to the following interfaces:

---
- `NewAnalyzer(; jetconfigs...) -> NewAnalyzer`: \\
  Constructs new analyzer given [JET configurations](@ref) passed as `jetconfigs`.
---
- `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`: \\
  Returns the [`AnalyzerState`](@ref) for `analyzer::NewAnalyzer`.
---
- `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`: \\
  Constructs an new `NewAnalyzer` instance in the middle of JET's [top-level analysis](@ref top-level-analysis)
  or [abstract interpretation based analysis](@ref abstractinterpret-analysis), given the
  previous `analyzer::NewAnalyzer` and [`state::AnalyzerState`](@ref AnalyzerState).
---
- `ReportPass(analyzer::NewAnalyzer) -> ReportPass`: \\
  Returns [`ReportPass`](@ref) used for `analyzer::NewAnalyzer`.
---

See also [`AnalyzerState`](@ref) and [`ReportPass`](@ref).

# Example

JET.jl defines its default error analyzer `JETAnalyzer <: AbstractAnalyzer` as the following
(modified a bit for the sake of simplicity):
```julia
# the default error analyzer for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
end

# AbstractAnalyzer API requirements

function JETAnalyzer(;
    report_pass::ReportPass = BasicPass(),
    jetconfigs...)
    return JETAnalyzer(report_pass,
                       AnalyzerState(; jetconfigs...),
                       )
end
AnalyzerState(analyzer::JETAnalyzer) =
    return analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) =
    return JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) =
    return analyzer.report_pass
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

"""
    abstract type ReportPass end

An interface type that represents `AbstractAnalyzer`'s report pass.
`analyzer::AbstractAnalyzer` injects report passes using the `(::ReportPass)(::Type{InferenceErrorReport}, ::AbstractAnalyzer, state, ...)`
interface, which provides a flexible and efficient layer to configure the analysis done by `AbstractAnalyzer`.

---

    ReportPass(analyzer::AbstractAnalyzer) -> ReportPass

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `ReportPass(analyzer::NewAnalyzer) -> ReportPass` interface.

`ReportPass` allows `NewAnalyzer` to provide a very flexible configuration layer for `NewAnalyzer`'s analysis;
an user can define their own `ReportPass` to control how `NewAnalyzer` collects report errors
while still using the analysis routine implemented by `NewAnalyzer`.

# Example

For example, [`JETAnalyzer`](@ref) accepts a custom `ReportPass` passed as [JET configurations](@ref)
(see the example documentation of [`AbstractAnalyzer`](@ref) for details).
And we can setup a custom report pass `IgnoreAllExceptGlobalUndefVar`, that ignores all the
reports that are otherwise collected by `JETAnalyzer` except `GlobalUndefVarErrorReport`:
```julia
# custom report pass that ignores all the reports except `GlobalUndefVarErrorReport`
struct IgnoreAllExceptGlobalUndefVar <: ReportPass end

# ignores all the reports analyzed by `JETAnalyzer`
(::IgnoreAllExceptGlobalUndefVar)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return

# bypass to `BasicPass` to collect `GlobalUndefVarErrorReport`
function (::IgnoreAllExceptGlobalUndefVar)(::Type{GlobalUndefVarErrorReport}, @nospecialize(args...))
    BasicPass()(GlobalUndefVarErrorReport, args...)
end

no_method_error()    = 1 + "1"
undef_global_error() = undefvar
report_call(; report_pass=IgnoreAllExceptGlobalUndefVar()) do
    if rand(Bool)
        return no_method_error()    # "no matching method found" error report won't be reported here
    else
        return undef_global_error() # "variable undefvar is not defined" error report will be reported
    end
end
```
"""
abstract type ReportPass end

function (::Type{Analyzer})(; jetconfigs...) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$Analyzer(; jetconfigs...) -> $Analyzer` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

function ReportPass(::Analyzer) where Analyzer<:AbstractAnalyzer
    error("""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$ReportPass(analyzer::$Analyzer) -> $ReportPass` interface.
    See the documentation of `$AbstractAnalyzer` and `$ReportPass`.
    """)
end

# pre-defined report passes
# TODO document the definitions of errors, elaborate the difference of these two passes

"""
    SoundPass <: ReportPass

`ReportPass` for the sound `JETAnalyzer`'s error analysis.
"""
struct SoundPass <: ReportPass end

"""
    BasicPass <: ReportPass

`ReportPass` for the basic (default) `JETAnalyzer`'s error analysis.
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
  In order to be cached and restored from [`JET_CACHE`](@ref), `T` _**must**_ implement
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
    else
        getfield(er, sym) # fallback
    end
end

function Base.show(io::IO, report::InferenceErrorReport)
    print(io, typeof(report).name.name, '(')
    for a in report.sig
        _print_signature(io, a, (; annotate_types = true); bold = true)
    end
    print(io, ')')
end
Base.show(io::IO, ::MIME"application/prs.juno.inline", report::InferenceErrorReport) =
    return report

get_msg(T::Type{<:InferenceErrorReport}, @nospecialize(_...)) = error("`get_msg` is not implemented for $T")
get_spec_args(T::Type{<:InferenceErrorReport}) =                error("`get_spec_args` is not implemented for $T")

# default constructor to create a report from abstract interpretation routine
function (T::Type{<:InferenceErrorReport})(analyzer::AbstractAnalyzer, state, @nospecialize(spec_args...))
    vf = get_virtual_frame(state)
    msg = get_msg(T, analyzer, state, spec_args...)
    return T([vf], msg, vf.sig, spec_args...)
end

"""
    aggregation_policy(analyzer::AbstractAnalyzer)

Defines how `analyzer` aggregates [`InferenceErrorReport`](@ref)s.
Defaults to `default_aggregation_policy`.

---

    default_aggregation_policy(report::InferenceErrorReport) -> DefaultReportIdentity

Returns the default identity of `report::InferenceErrorReport`, where `DefaultReportIdentity`
aggregates reports based on "error location" of each `report`.
`DefaultReportIdentity` aggregates `InferenceErrorReport`s aggressively in a sense that it
ignores the identity of error point's `MethodInstance`, under the assumption that errors are
identical as far as they're collected at the same file and line.
"""
aggregation_policy(::AbstractAnalyzer) = default_aggregation_policy
function default_aggregation_policy(@nospecialize(report::InferenceErrorReport))
    return DefaultReportIdentity(
        typeof(report),
        report.sig,
        # VirtualFrameNoLinfo(first(report.vst)),
        VirtualFrameNoLinfo(last(report.vst)),
        )
end
@withmixedhash struct VirtualFrameNoLinfo
    file::Symbol
    line::Int
    sig::Vector{Any}
    # linfo::MethodInstance # ignore the idenity of `MethodInstace`
    VirtualFrameNoLinfo(vf::VirtualFrame) = new(vf.file, vf.line, vf.sig)
end
@withmixedhash struct DefaultReportIdentity
    T::Type{<:InferenceErrorReport}
    sig::Vector{Any}
    # entry_frame::VirtualFrameNoLinfo
    error_frame::VirtualFrameNoLinfo
end

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

# TODO parametric definition

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

    T, S = esc(T), esc(S)

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
