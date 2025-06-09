# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type of analyzers that are built on top of [JET's analyzer framework](@ref AbstractAnalyzer-Framework).

When a new type `NewAnalyzer` implements the `AbstractAnalyzer` interface, it should be declared
as subtype of `AbstractAnalyzer`, and is expected to implement the following interfaces:

## Required interfaces

1. **`JETInterface.AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`**:
   Returns the [`AnalyzerState`](@ref) for `analyzer::NewAnalyzer`.

2. **`JETInterface.AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`**:
   Constructs a new `NewAnalyzer` instance in the middle of JET's [top-level analysis](@ref toplevel)
   or [abstract interpretation](@ref abstractinterpret), given the previous
   `analyzer::NewAnalyzer` and [`state::AnalyzerState`](@ref AnalyzerState).

3. **`JETInterface.ReportPass(analyzer::NewAnalyzer) -> ReportPass`**:
   Returns [`ReportPass`](@ref) used for `analyzer::NewAnalyzer`.

4. **`JETInterface.AnalysisToken(analyzer::NewAnalyzer) -> AnalysisToken`**:
   Returns a unique `AnalysisToken` object used for `analyzer::NewAnalyzer`.

See also [`AnalyzerState`](@ref), [`ReportPass`](@ref) and [`AnalysisToken`](@ref).

# Example

JET.jl defines its default error analyzer `JETAnalyzer <: AbstractAnalyzer` as the following
(modified a bit for the sake of simplicity):
```julia
# the default error analyzer for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    state::AnalyzerState
    report_pass::RP
end

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::JETAnalyzer) = analyzer.state
JETInterface.AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) = JETAnalyzer(ReportPass(analyzer), state)
JETInterface.ReportPass(analyzer::JETAnalyzer) = analyzer.report_pass
let global_analysis_token = AnalysisToken()
    JETInterface.AnalysisToken(analyzer::JETAnalyzer) = global_analysis_token
end
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

# interface 1
# -----------
# 1. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`

"""
    AnalysisResult

Container for error reports collected during analysis of a specific `InferenceResult`.

[`AbstractAnalyzer`](@ref) manages [`InferenceErrorReport`](@ref) instances by
associating them with their corresponding `InferenceResult`. Reports found
during the analysis of `result::InferenceResult` can be accessed via
`get_reports(analyzer, result)`.
"""
struct AnalysisResult
    reports::Vector{InferenceErrorReport}
end

"""
    CachedAnalysisResult

Cached version of [`AnalysisResult`](@ref) stored in the global analyzer cache.

When an [`AnalysisResult`](@ref) is cached into the global cache maintained by
`AbstractAnalyzer`, it's transformed into this type. That is, when
`codeinf::CodeInstance = $CC.code_cache(analyzer::AbstractAnalyzer)[mi::MethodInstance]`,
the `codeinf.inferred` field will contain a `CachedAnalysisResult` instance.
"""
struct CachedAnalysisResult
    reports::Vector{InferenceErrorReport}
end

struct AbstractBindingState
    isconst::Bool
    maybeundef::Bool
    typ
    AbstractBindingState(isconst::Bool, maybeundef::Bool, @nospecialize typ) = new(isconst, maybeundef, typ)
    AbstractBindingState(isconst::Bool, maybeundef::Bool) = new(isconst, maybeundef)
end
const AbstractAbstractBindings = IdDict{Core.BindingPartition,AbstractBindingState}

"""
    mutable struct AnalyzerState
        ...
    end

The mutable object that holds various states that are consumed by all [`AbstractAnalyzer`](@ref)s.

---

    JETInterface.AnalyzerState(analyzer::AbstractAnalyzer) -> AnalyzerState

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState` interface.

A new `AnalyzerState` is supposed to be constructed using the [general configurations](@ref) passed
as keyword arguments `jetconfigs` of the [`NewAnalyzer(; jetconfigs...)`](@ref AbstractAnalyzer)
constructor, and the constructed `AnalyzerState` is usually kept within `NewAnalyzer` itself:
```julia
function NewAnalyzer(world::UInt=Base.get_world_counter(); jetconfigs...)
    ...
    state = AnalyzerState(world; jetconfigs...)
    return NewAnalyzer(..., state)
end
JETInterface.AnalyzerState(analyzer::NewAnalyzer) = analyzer.state
```
"""
mutable struct AnalyzerState
    ## AbstractInterpreter ##

    world::UInt
    inf_cache::Vector{InferenceResult}
    inf_params::InferenceParams
    opt_params::OptimizationParams

    ## AbstractAnalyzer ##

    results::IdDict{InferenceResult,AnalysisResult}

    # the temporal stash to keep reports that are collected within the currently-analyzed frame:
    # they will be appended to the caller when returning back to the caller inference/optimization
    report_stash::Vector{InferenceErrorReport}

    # the temporal stash to keep track of the context of caller inference/optimization and
    # the caller itself, to which reconstructed cached reports will be appended
    cache_target::Union{Nothing,Pair{Symbol,InferenceState}}

    ## abstract toplevel execution ##

    # will be used in toplevel analysis (skip inference on actually interpreted statements)
    concretized::BitVector

    binding_states::AbstractAbstractBindings # TODO Make this globally maintained?

    # some `AbstractAnalyzer` may want to use this
    entry::Union{Nothing,MethodInstance}
end

# define shortcut getter/setter methods for `AbstractAnalyzer`s
for fld in fieldnames(AnalyzerState)
    getter = Symbol("get_", fld)
    setter = Symbol("set_", fld, '!')
    @eval (@__MODULE__) @inline $getter(analyzer::AbstractAnalyzer)    = getfield(AnalyzerState(analyzer), $(QuoteNode(fld)))
    @eval (@__MODULE__) @inline $setter(analyzer::AbstractAnalyzer, v) = setfield!(AnalyzerState(analyzer), $(QuoteNode(fld)), v)
end

function AnalyzerState(world::UInt  = get_world_counter();
    results::IdDict{InferenceResult,AnalysisResult} = IdDict{InferenceResult,AnalysisResult}(),
    inf_params::Union{Nothing,InferenceParams} = nothing,
    opt_params::Union{Nothing,OptimizationParams} = nothing,
    concretized::BitVector = _CONCRETIZED,
    binding_states::AbstractAbstractBindings = AbstractAbstractBindings(),
    jetconfigs...)
    isnothing(inf_params) && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params) && (opt_params = JETOptimizationParams(; jetconfigs...))
    inf_cache = InferenceResult[]
    report_stash = InferenceErrorReport[]
    return AnalyzerState(#=world::UInt=# world,
                         #=inf_cache::Vector{InferenceResult}=# inf_cache,
                         #=inf_params::InferenceParams=# inf_params,
                         #=opt_params::OptimizationParams=# opt_params,
                         #=results::IdDict{InferenceResult,AnalysisResult}=# results,
                         #=report_stash::Vector{InferenceErrorReport}=# report_stash,
                         #=cache_target::Union{Nothing,Pair{Symbol,InferenceState}}=# nothing,
                         #=concretized::BitVector=# concretized,
                         #=binding_states::AbstractAbstractBindings=# binding_states,
                         #=entry::Union{Nothing,MethodInstance}=# nothing)
end

# dummies for non-toplevel analysis
const _CONCRETIZED  = BitVector()

"""
Configurations for abstract interpretation performed by JET.
These configurations will be active for all the entries.

You can configure any of the keyword parameters that [`$CC.InferenceParams`](@ref)
or [`$CC.OptimizationParams`](@ref) can take, e.g. `max_methods`:
```julia
julia> methods(==, (Any,Nothing))
# 3 methods for generic function "==" from Base:
 [1] ==(::Missing, ::Any)
     @ missing.jl:75
 [2] ==(w::WeakRef, v)
     @ gcutils.jl:4
 [3] ==(x, y)
     @ Base.jl:127

julia> report_call((Any,)) do x
           # when we account for all the possible matching method candidates,
           # `(::Missing == ::Nothing)::Missing` leads to an `NonBooleanCondErrorReport`
           x == nothing ? :nothing : :some
       end
═════ 1 possible error found ═════
┌ @ none:4 goto %4 if not x == nothing
│ non-boolean `Missing` found in boolean context (1/2 union split): goto %4 if not (x::Any == nothing)::Union{Missing, Bool}
└──────────

julia> report_call((Any,); max_methods=1) do x
           # since we limit `max_methods=1`, JET gives up analysis on `(x::Any == nothing)`
           # and thus we won't get any error report
           x == nothing ? :nothing : :some
       end
No errors detected
```

See also [`$CC.InferenceParams`](@ref) and [`$CC.OptimizationParams`](@ref).

Listed below are selections of those parameters that can have a potent influence on JET analysis.

---
- `ipo_constant_propagation::Bool = true` \\
  Enables constant propagation in abstract interpretation.
  It is _**highly**_ recommended that you keep this configuration `true` to get reasonable analysis result,
  because constant propagation can cut off lots of false positive errorenous code paths and
  thus produce more accurate and useful analysis results.
---
- `aggressive_constant_propagation::Bool = true` \\
  If `true`, JET will try to do constant propagation more "aggressively".
  It can lead to more accurate analysis as explained above, but also it may incur a performance cost.
  JET by default enables this configuration to get more accurate analysis result.
---
"""
function JETInferenceParams end
function JETOptimizationParams end

# define wrappers of `InferenceParams(...)` and `OptimizationParams(...)` that can accept JET configurations
for (Params, Func) = ((InferenceParams, JETInferenceParams),
                      (OptimizationParams, JETOptimizationParams))
    params = Params()
    param = Expr(:kw, Expr(:(::), :params, Params), CC.quoted(params))
    kwargs = Expr[]
    parameters = Symbol[]
    for i = 1:nfields(params)
        fname, ftype = fieldname(Params, i), fieldtype(Params, i)
        push!(parameters, fname)
        arg = Expr(:(::), fname, ftype)
        default = Expr(:., :params, QuoteNode(fname))
        push!(kwargs, Expr(:kw, arg, default))
    end
    sig = Expr(:call, nameof(Func), Expr(:parameters, kwargs..., :(__jetconfigs...)), param)
    call = Expr(:call, nameof(Params), parameters...)
    body = Expr(:block, LineNumberNode(@__LINE__, @__FILE__), call)
    def = Expr(:(=), sig, body)
    Core.eval(@__MODULE__, def)
end

# assert that the wrappers create same objects as the original constructors
for (Params, Func) = ((InferenceParams, JETInferenceParams),
                      (OptimizationParams, JETOptimizationParams))
    @assert Params() == Func()
end
@assert JETInferenceParams(InferenceParams(); max_methods=1).max_methods == 1
@assert !JETOptimizationParams(OptimizationParams(); inlining=false).inlining

Base.show(io::IO, state::AnalyzerState) = print(io, typeof(state), "(...)")

@noinline function AnalyzerState(analyzer::AbstractAnalyzer)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `$AnalyzerState(analyzer::$AnalyzerType) -> $AnalyzerState` interface.
    See the documentation of `$AbstractAnalyzer` and `$AnalyzerState`.
    """)
end

# interface 2
# -----------
# 2. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`

@noinline function AbstractAnalyzer(analyzer::AbstractAnalyzer, state::AnalyzerState)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `$AbstractAnalyzer(analyzer::$AnalyzerType, state::$AnalyzerState) -> $AnalyzerType` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# constructor for additional JET analysis in the middle of parent (non top-level) abstractinterpret
function AbstractAnalyzer(analyzer::T) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(CC.get_inference_world(analyzer);
                             results    = get_results(analyzer),
                             inf_params = InferenceParams(analyzer),
                             opt_params = OptimizationParams(analyzer))
    return AbstractAnalyzer(analyzer, newstate)
end

# interface 3
# -----------
# 3. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`

"""
    abstract type ReportPass end

An interface type that represents `AbstractAnalyzer`'s report pass.
`analyzer::AbstractAnalyzer` injects report passes using the `(::ReportPass)(::Type{InferenceErrorReport}, ::AbstractAnalyzer, state, ...)`
interface, which provides a flexible and efficient layer to configure the analysis done by `AbstractAnalyzer`.

---

    JETInterface.ReportPass(analyzer::AbstractAnalyzer) -> ReportPass

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `ReportPass(analyzer::NewAnalyzer) -> ReportPass` interface.

`ReportPass` allows `NewAnalyzer` to provide a very flexible configuration layer for `NewAnalyzer`'s analysis;
an user can define their own `ReportPass` to control how `NewAnalyzer` collects report errors
while still using the analysis routine implemented by `NewAnalyzer`.

# Example

For example, [`JETAnalyzer`](@ref) accepts a custom `ReportPass` passed as part of the
[general configurations](@ref) (see the documentation of [`AbstractAnalyzer`](@ref) for
an example implementation).
And we can setup a custom report pass `IgnoreAllExceptGlobalUndefVar`, that ignores all the
reports that are otherwise collected by `JETAnalyzer` except `UndefVarErrorReport`:
```julia
# custom report pass that ignores all the reports except `UndefVarErrorReport`
struct IgnoreAllExceptGlobalUndefVar <: ReportPass end

# ignores all the reports analyzed by `JETAnalyzer`
(::IgnoreAllExceptGlobalUndefVar)(::Type{<:InferenceErrorReport}, @nospecialize(_...)) = return

# forward to `BasicPass` to collect `UndefVarErrorReport`
function (::IgnoreAllExceptGlobalUndefVar)(::Type{UndefVarErrorReport}, @nospecialize(args...))
    BasicPass()(UndefVarErrorReport, args...)
end

no_method_error()    = 1 + "1"
undef_global_error() = undefvar
report_call(; report_pass=IgnoreAllExceptGlobalUndefVar()) do
    if rand(Bool)
        return no_method_error()    # "no matching method found" error report won't be reported here
    else
        return undef_global_error() # "`undefvar` is not defined" error report will be reported
    end
end
```
"""
abstract type ReportPass end

@noinline function ReportPass(analyzer::AbstractAnalyzer)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `$ReportPass(analyzer::$AnalyzerType) -> $ReportPass` interface.
    See the documentation of `$AbstractAnalyzer` and `$ReportPass`.
    """)
end

# interface 4
# -----------
# 4. `JETInterface.AnalysisToken(analyzer::NewAnalyzer) -> AnalysisToken`

"""
    mutable struct AnalysisToken
        AnalysisToken() = new()
    end

A unique token object used to identify and separate caches of analysis results.

Each `AbstractAnalyzer` implementation should use a consistent token to enable
proper caching behavior. The identity of the token determines whether cached analysis
results can be reused between analyzer instances.
"""
mutable struct AnalysisToken
    AnalysisToken() = new()
end

"""
    JETInterface.AnalysisToken(analyzer::AbstractAnalyzer) -> AnalysisToken

Returns [`AnalysisToken`](@ref) for the given `analyzer::AbstractAnalyzer`.
`AbstractAnalyzer` instances can share the same cache if they perform the same analysis,
otherwise their cache should be separated.

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, it must implement this
function to return a consistent token for instances that should share the same cache.
"""
@noinline function AnalysisToken(analyzer::AbstractAnalyzer)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `JETInterface.AnalysisToken(analyzer::$AnalyzerType) -> AnalysisToken` interface.
    See the documentation of `$AbstractAnalyzer` and `JETInterface.AnalysisToken`.
    """)
end

# optional API
# ------------

"""
    JETInterface.valid_configurations(analyzer::AbstractAnalyzer) -> names or nothing

Returns a set of names that are valid as a configuration for `analyzer`.
`names` should be an iterator of `Symbol`.
No validations are performed if `nothing` is returned.
"""
valid_configurations(analyzer::AbstractAnalyzer) = nothing

function validate_configs(analyzer::AbstractAnalyzer, jetconfigs)
    valid_keys = valid_configurations(analyzer)
    isnothing(valid_keys) && return nothing
    for (key, val) in jetconfigs
        if key ∉ valid_keys
            valrepr = LazyPrinter(io::IO->show(io,val))
            throw(JETConfigError(lazy"Given unexpected configuration: `$key = $valrepr`", key, val))
        end
    end
    return nothing
end

"""
    JETInterface.aggregation_policy(analyzer::AbstractAnalyzer)

Defines how `analyzer` aggregates [`InferenceErrorReport`](@ref)s.
This policy determines how duplicate or similar reports are identified and grouped.
Defaults to `default_aggregation_policy`.

---

    default_aggregation_policy(report::InferenceErrorReport) -> DefaultReportIdentity

Returns the default identity of `report::InferenceErrorReport` using `DefaultReportIdentity`,
which aggregates reports based on their "error location".

`DefaultReportIdentity` aggregates `InferenceErrorReport`s by creating an identity based on:
1. The report type
2. The signature of the method where the error was found
3. The file and line number where the error occurred

This approach ignores the specific `MethodInstance` identity, allowing errors to be
aggregated if they occur at the same file and line, under the assumption that errors
at the same location are likely duplicates even if in different method specializations.
"""
aggregation_policy(::AbstractAnalyzer) = default_aggregation_policy
const default_aggregation_policy = function (report::InferenceErrorReport)
    @nospecialize report
    return DefaultReportIdentity(
        typeof(Base.inferencebarrier(report)),
        report.sig,
        # VirtualFrameNoLinfo(first(report.vst)),
        VirtualFrameNoLinfo(last(report.vst)))
end
@withmixedhash struct VirtualFrameNoLinfo
    file::Symbol
    line::Int
    # linfo::MethodInstance # ignore the identity of `MethodInstance`
    VirtualFrameNoLinfo(vf::VirtualFrame) = new(vf.file, vf.line)
end
@withmixedhash struct DefaultReportIdentity
    T::DataType
    sig::Signature
    # entry_frame::VirtualFrameNoLinfo
    error_frame::VirtualFrameNoLinfo
end

# InferenceResult
# ===============
# define how AbstractAnalyzer manages `InferenceResult`

Base.getindex(analyzer::AbstractAnalyzer, result::InferenceResult) = get_results(analyzer)[result]
Base.setindex!(analyzer::AbstractAnalyzer, analysis_result::AnalysisResult, result::InferenceResult) = get_results(analyzer)[result] = analysis_result

function init_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    analyzer[result] = AnalysisResult(InferenceErrorReport[])
    return nothing
end

get_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::AnalysisResult).reports

"""
    add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, report::InferenceErrorReport)

Adds a new error report to the analyzer's collection for a specific inference result.

This function associates an [`InferenceErrorReport`](@ref) with its corresponding
`result::InferenceResult` in the analyzer's internal storage. The report becomes
part of the analysis results that can be retrieved later using `get_reports(analyzer, result)`.

Reports are stored in the order they are added, which can be important for maintaining
the logical sequence of errors discovered during analysis.
"""
function add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, @nospecialize(report::InferenceErrorReport))
    push!(get_reports(analyzer, result), report)
    return report
end

function add_cached_report!(analyzer::AbstractAnalyzer, caller::InferenceResult, @nospecialize(cached::InferenceErrorReport))
    cached = copy_report_stable(cached)
    push!(get_reports(analyzer, caller), cached)
    return cached
end

stash_report!(analyzer::AbstractAnalyzer, @nospecialize(report::InferenceErrorReport)) = push!(get_report_stash(analyzer), report)
stash_reports!(analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport}) = append!(get_report_stash(analyzer), reports)

# AbstractInterpreter
# ===================
# provide default implementations for the API requirements

CC.InferenceParams(analyzer::AbstractAnalyzer) = get_inf_params(analyzer)
CC.OptimizationParams(analyzer::AbstractAnalyzer) = get_opt_params(analyzer)
CC.get_inference_world(analyzer::AbstractAnalyzer) = get_world(analyzer)

# allow compression during precompilation only
CC.may_compress(::AbstractAnalyzer) = generating_output()

# this overload is necessary to avoid caching with the const ABI
CC.may_discard_trees(::AbstractAnalyzer) = false

# ToplevelAbstractAnalyzer
# ========================

"""
    abstract type ToplevelAbstractAnalyzer <: AbstractAnalyzer end

A specialized interface type for analyzers that perform top-level analysis of Julia code.

`ToplevelAbstractAnalyzer` extends [`AbstractAnalyzer`](@ref) to provide clear separation
between analyzers that support top-level analysis and those that don't, offering several
key architectural benefits:
- _Type Safety_: Only analyzers that explicitly extend `ToplevelAbstractAnalyzer` can be
  used with JET's `virtual_process` system, making it clear at compile time which analyzers
  support top-level analysis capabilities.
- _Responsibility Separation_: Analyzers like `OptAnalyzer` that don't perform top-level
  analysis are no longer required to handle top-level specific code paths, reducing
  complexity and improving performance.

## Top-Level Analysis Capabilities

`ToplevelAbstractAnalyzer` provides specialized functionality for analyzing top-level
Julia constructs such as:
- Global variable assignments
- Constant declarations (`const` statements)
- Module definitions and imports
- Method definitions at the top level
- Package-level code execution

This analyzer type is used in [`virtual_process`](@ref) system to analyze Julia code
as it would be executed at the top level, handling both concrete interpretation of some
statements and abstract interpretation of others.

## Usage

`ToplevelAbstractAnalyzer` is typically used through JET's virtual process system:

```julia
# Create a concrete interpreter with a toplevel analyzer
interp = JETConcreteInterpreter(JETAnalyzer(...))
analyzer = ToplevelAbstractAnalyzer(interp)

# Analyze top-level code
result = analyze_and_report_text!(interp, "x = 1; y = x + 1")
```

## Implementation Requirements

Concrete subtypes of `ToplevelAbstractAnalyzer` must implement all the interfaces required
by [`AbstractAnalyzer`](@ref), and will automatically inherit the specialized top-level
analysis behaviors provided by this type.

## See Also

- [`AbstractAnalyzer`](@ref): The base analyzer interface
- [`JETAnalyzer`](@ref): JET's default error analyzer that implements this interface
- [`virtual_process`](@ref): The main function that uses this analyzer type
- [`ConcreteInterpreter`](@ref): The concrete interpreter that works with this analyzer
"""
abstract type ToplevelAbstractAnalyzer <: AbstractAnalyzer end

# constructor for sequential toplevel JET analysis
function ToplevelAbstractAnalyzer(analyzer::T, concretized::BitVector;
    # update world age to take in newly added methods defined by `ConcreteInterpreter`
    world::UInt = get_world_counter()
    ) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(world;
                             inf_params = InferenceParams(analyzer),
                             opt_params = OptimizationParams(analyzer),
                             concretized,
                             binding_states = get_binding_states(analyzer))
    return AbstractAnalyzer(analyzer, newstate)
end
