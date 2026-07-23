# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type for analyzers built on
[JET's `AbstractAnalyzer` framework](@ref AbstractAnalyzer-framework).

To implement the `AbstractAnalyzer` interface, declare `NewAnalyzer` as a
subtype of `AbstractAnalyzer` and implement the following methods:

## Required interfaces

1. **`JETInterface.AnalyzerState(::NewAnalyzer) -> AnalyzerState`**
  Return the [`AnalyzerState`](@ref) associated with the analyzer.

2. **`JETInterface.AbstractAnalyzer(::NewAnalyzer, ::AnalyzerState) -> NewAnalyzer`**
  Construct a new `NewAnalyzer` from an existing analyzer and a replacement
  state. JET calls this method when it recreates an analyzer during
  [top-level analysis](@ref toplevel) or
  [abstract interpretation](@ref abstractinterpret).

3. **`JETInterface.AnalysisToken(::NewAnalyzer) -> AnalysisToken`**
  Return the [`AnalysisToken`](@ref) that identifies analyzer instances whose
  cached analysis results may be shared.

See also [`AnalyzerState`](@ref) and [`AnalysisToken`](@ref).

# Example

JET.jl's default error analyzer, `BasicJETAnalyzer <: AbstractAnalyzer`, can be
represented by the following simplified definition:
```julia
# the default error analyzer for JET.jl
struct BasicJETAnalyzer <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    # ... other fields
end

# AbstractAnalyzer API requirements
JETInterface.AnalyzerState(analyzer::BasicJETAnalyzer) = analyzer.state
JETInterface.AbstractAnalyzer(
    analyzer::BasicJETAnalyzer, state::AnalyzerState) =
        BasicJETAnalyzer(state, analyzer.analysis_token, ...)
JETInterface.AnalysisToken(analyzer::BasicJETAnalyzer) = analyzer.analysis_token
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

A cached collection of reports associated with an [`AnalysisResult`](@ref).

JET copies reports into this container and stacks it directly on the
corresponding `InferenceResult` with `CC.stack_analysis_result!`. When that
inference result is cached as a `CodeInstance`, the metadata is retained.
Global cache lookup recovers the reports by traversing the `CodeInstance` with
`CC.traverse_analysis_results`; local cache lookup can traverse the
`InferenceResult` directly.
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

# `report_package` analyzes signatures concurrently while all task analyzers share a single
# `binding_states::AbstractBindings` (see `AnalyzerState`). Guard the underlying `IdDict` with
# a lock so concurrent `setindex!`/rehash cannot corrupt it; the accesses are infrequent
# (only on global binding assignments and binding-partition lookups), so the uncontended lock
# cost is negligible.
struct AbstractBindings
    bindings::IdDict{Core.BindingPartition,AbstractBindingState}
    lock::ReentrantLock
    AbstractBindings() = new(IdDict{Core.BindingPartition,AbstractBindingState}(), ReentrantLock())
end
@inline Base.getindex(b::AbstractBindings, partition::Core.BindingPartition) =
    @lock b.lock b.bindings[partition]
@inline Base.setindex!(b::AbstractBindings, binding_state::AbstractBindingState, partition::Core.BindingPartition) =
    (@lock b.lock b.bindings[partition] = binding_state; return b)
@inline Base.haskey(b::AbstractBindings, partition::Core.BindingPartition) =
    @lock b.lock haskey(b.bindings, partition)
@inline Base.get(b::AbstractBindings, partition::Core.BindingPartition, @nospecialize(default)) =
    @lock b.lock get(b.bindings, partition, default)

"""
    mutable struct AnalyzerState
        ...
    end

Mutable storage for the state used by an [`AbstractAnalyzer`](@ref).

---

    JETInterface.AnalyzerState(analyzer::AbstractAnalyzer) -> AnalyzerState

Every `NewAnalyzer` subtype must implement this method to return its
`AnalyzerState`.

A new `AnalyzerState` is normally constructed by the
[`NewAnalyzer(; jetconfigs...)`](@ref AbstractAnalyzer) constructor and stored
in the analyzer itself:
```julia
function NewAnalyzer(world::UInt = Base.get_world_counter(); jetconfigs...)
    ...
    state = AnalyzerState(world; jetconfigs...)
    return NewAnalyzer(..., state)
end
JETInterface.AnalyzerState(analyzer::NewAnalyzer) = analyzer.state
```
"""
mutable struct AnalyzerState
    ## AbstractInterpreter ##

    const world::UInt
    const inf_cache::Vector{InferenceResult}
    const inf_params::InferenceParams
    const opt_params::OptimizationParams

    ## AbstractAnalyzer ##

    const analysis_results::IdDict{InferenceResult,AnalysisResult}

    # the temporal stash to keep reports that are collected within the currently-analyzed frame:
    # they will be appended to the caller when returning back to the caller inference/optimization
    const report_stash::Vector{InferenceErrorReport}

    # the temporal stash to keep track of the context of caller inference/optimization and
    # the caller itself, to which reconstructed cached reports will be appended
    cache_target::Union{Nothing,Pair{Symbol,InferenceState}}

    ## abstract toplevel execution ##

    # will be used in toplevel analysis (skip inference on actually interpreted statements)
    const concretized::BitVector

    const binding_states::AbstractBindings # TODO Make this globally maintained?

    # some `AbstractAnalyzer` may want to use this
    entry::Union{Nothing,MethodInstance}
end

# define shortcut getter/setter methods for `AbstractAnalyzer`s
for fld in fieldnames(AnalyzerState)
    getter = Symbol("get_", fld)
    @eval (@__MODULE__) @inline $getter(analyzer::AbstractAnalyzer) = getfield(AnalyzerState(analyzer), $(QuoteNode(fld)))
end
set_cache_target!(analyzer::AbstractAnalyzer, target::Union{Nothing,Pair{Symbol,InferenceState}}) = setfield!(AnalyzerState(analyzer), :cache_target, target)
set_entry!(analyzer::AbstractAnalyzer, entry::Union{Nothing,MethodInstance}) = setfield!(AnalyzerState(analyzer), :entry, entry)

# The main constructor used at analysis entries
function AnalyzerState(world::UInt = get_world_counter();
                       inf_params::InferenceParams = InferenceParams(),
                       opt_params::OptimizationParams = OptimizationParams(),
                       jetconfigs...)
    reject_abstract_interpretation_configs(jetconfigs)
    return AnalyzerState(#=world::UInt=# world,
                         #=inf_cache::Vector{InferenceResult}=# InferenceResult[],
                         #=inf_params::InferenceParams=# inf_params,
                         #=opt_params::OptimizationParams=# opt_params,
                         #=analysis_results::IdDict{InferenceResult,AnalysisResult}=# IdDict{InferenceResult,AnalysisResult}(),
                         #=report_stash::Vector{InferenceErrorReport}=# InferenceErrorReport[],
                         #=cache_target::Union{Nothing,Pair{Symbol,InferenceState}}=# nothing,
                         #=concretized::BitVector=# non_toplevel_concretized,
                         #=binding_states::AbstractBindings=# AbstractBindings(),
                         #=entry::Union{Nothing,MethodInstance}=# nothing)
end

# The constructor that inherits from existing `state::AnalyzerState`
function AnalyzerState(state::AnalyzerState, refresh_local_cache::Bool=true;
                       world::UInt = state.world,
                       inf_params::InferenceParams = state.inf_params,
                       opt_params::OptimizationParams = state.opt_params,
                       concretized::BitVector = state.concretized,
                       binding_states::AbstractBindings = state.binding_states,
                       entry::Union{Nothing,MethodInstance} = state.entry)
    if refresh_local_cache
        inf_cache = InferenceResult[]
        analysis_results = IdDict{InferenceResult,AnalysisResult}()
    else
        (; inf_cache, analysis_results) = state
    end
    return AnalyzerState(world,
                         inf_cache,
                         inf_params,
                         opt_params,
                         analysis_results,
                         #=report_stash=# InferenceErrorReport[],
                         #=cache_target=# nothing,
                         concretized,
                         binding_states,
                         entry)
end

# dummies for non-toplevel analysis
const non_toplevel_concretized  = BitVector()

const ABSTRACT_INTERPRETATION_CONFIGURATIONS = Set{Symbol}((
    fieldnames(InferenceParams)...,
    fieldnames(OptimizationParams)...))

function reject_abstract_interpretation_configs(jetconfigs)
    for (key, val) in jetconfigs
        if key in ABSTRACT_INTERPRETATION_CONFIGURATIONS
            valrepr = LazyPrinter(io::IO->show(io, val))
            throw(JETConfigError(
                lazy"Given unexpected configuration: `$key = $valrepr`", key, val))
        end
    end
    return nothing
end

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

@noinline function AbstractAnalyzer(analyzer::AbstractAnalyzer, ::AnalyzerState)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `$AbstractAnalyzer(analyzer::$AnalyzerType, state::$AnalyzerState) -> $AnalyzerType` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# constructor for additional JET analysis in the middle of parent (non top-level) abstractinterpret
function AbstractAnalyzer(analyzer::T) where {T<:AbstractAnalyzer}
    state = AnalyzerState(analyzer)
    newstate = AnalyzerState(state, #=refresh_local_cache=#false)
    return AbstractAnalyzer(analyzer, newstate)
end

# interface 3
# -----------
# 3. `JETInterface.AnalysisToken(analyzer::NewAnalyzer) -> AnalysisToken`

"""
    mutable struct AnalysisToken
        AnalysisToken() = new()
    end

An identity token used as the compiler cache owner for an [`AbstractAnalyzer`](@ref).

The token's object identity determines which analyzer instances can reuse
cached inference and report results. Reuse the same token object only for
analyzers with cache-compatible behavior; use distinct objects otherwise.
"""
mutable struct AnalysisToken
    AnalysisToken() = new()
end

"""
    JETInterface.AnalysisToken(analyzer::AbstractAnalyzer) -> AnalysisToken

Return the [`AnalysisToken`](@ref) used as the cache owner for `analyzer`.

Every `NewAnalyzer` subtype must implement this method. Instances that can
safely reuse cached inference and report results must return the same token
object. Instances whose analysis behavior or configuration can produce
incompatible results must return distinct token objects.
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
    typeinf_world(analyzer::AbstractAnalyzer) -> world::Union{UInt,Nothing}

Return the world age to use for type inference performed by the given `analyzer`,
or `nothing` to use the current world.

When a specific world age is returned, the analyzer will invoke type inference
within that fixed world using `Base.invoke_in_world`. This makes the analysis
implementation more robust against potential invalidations that may be caused
by loading external packages.

The default implementation returns `nothing`, meaning type inference runs in
the latest world. Specific analyzer implementations may override this to return
a fixed world age for stability.
"""
typeinf_world(::AbstractAnalyzer) = nothing

"""
    JETInterface.valid_configurations(analyzer::AbstractAnalyzer) -> names or nothing

Return an iterable of `Symbol`s naming the configurations accepted by
`analyzer`. Return `nothing` to skip configuration-name validation.
"""
valid_configurations(::AbstractAnalyzer) = nothing

function validate_configs(analyzer::AbstractAnalyzer, jetconfigs)
    return validate_configs(valid_configurations(analyzer), jetconfigs)
end

function validate_configs(valid_keys, jetconfigs)
    isnothing(valid_keys) && return nothing
    for (key, val) in jetconfigs
        if key ∉ valid_keys
            valrepr = LazyPrinter(io::IO->show(io,val))
            throw(JETConfigError(
                lazy"Given unexpected configuration: `$key = $valrepr`", key, val))
        end
    end
    return nothing
end

"""
    JETInterface.aggregation_policy(analyzer::AbstractAnalyzer) -> key_function

Return a callable that maps each [`InferenceErrorReport`](@ref) to the key used
to deduplicate reports. The default is `default_aggregation_policy`.

---

    default_aggregation_policy(report::InferenceErrorReport) -> DefaultReportIdentity

Return the default deduplication key for a report. Two reports have the same
key when they have:

1. The same concrete report type
2. Equal expression [`Signature`](@ref)s
3. The same file and line in their final [`VirtualFrame`](@ref)s

`Signature` equality compares only the `_sig` elements, using `===`, and ignores
`tt`. The stack-trace component uses only the final frame's file and line; it
omits that frame's `MethodInstance` and all preceding frames.
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

Base.getindex(analyzer::AbstractAnalyzer, result::InferenceResult) = get_analysis_results(analyzer)[result]
Base.setindex!(analyzer::AbstractAnalyzer, analysis_result::AnalysisResult, result::InferenceResult) = get_analysis_results(analyzer)[result] = analysis_result

function init_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    analyzer[result] = AnalysisResult(InferenceErrorReport[])
    return nothing
end

get_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::AnalysisResult).reports

"""
    add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, report::InferenceErrorReport)

Append `report` to the reports associated with `result` in `analyzer`, then
return `report`. Retrieve the collection with `get_reports(analyzer, result)`.
Reports remain in insertion order.
"""
function add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, @nospecialize(report::InferenceErrorReport))
    push!(get_reports(analyzer, result), report)
    return report
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

An interface type for analyzers that support top-level analysis of Julia code.

`ToplevelAbstractAnalyzer` is a subtype of [`AbstractAnalyzer`](@ref).
Methods specialized for this type provide top-level-specific inference behavior
and distinguish these analyzers from analyzers such as `OptAnalyzer`, which do
not support top-level analysis.

## Top-level analysis capabilities

Together with a [`ConcreteInterpreter`](@ref), a `ToplevelAbstractAnalyzer`
supports analysis of top-level constructs such as:

- Global variable assignments
- Constant declarations (`const` statements)
- Module definitions and imports
- Method definitions at the top level
- Package-level code execution

The [`virtual_process`](@ref) system uses the concrete interpreter and analyzer
to process Julia code as it would execute at the top level. Some statements are
interpreted concretely, while others are analyzed abstractly.

## Usage

`ToplevelAbstractAnalyzer` is typically used through JET's virtual process system:

```julia
# Create a concrete interpreter with a top-level analyzer
interp = JETConcreteInterpreter(JETAnalyzer())
analyzer = ToplevelAbstractAnalyzer(interp)

# Analyze top-level code
result = analyze_and_report_text!(interp, "x = 1; y = x + 1")
```

## Implementation requirements

Concrete subtypes of `ToplevelAbstractAnalyzer` must implement all interfaces
required by [`AbstractAnalyzer`](@ref). Methods specialized for this interface
then provide the top-level-specific abstract interpretation behavior.

## See also

- [`AbstractAnalyzer`](@ref): the base analyzer interface
- [`JETAnalyzer`](@ref): JET's default error analyzer for this interface
- [`virtual_process`](@ref): the top-level processing entry point
- [`ConcreteInterpreter`](@ref): the corresponding concrete interpreter
"""
abstract type ToplevelAbstractAnalyzer <: AbstractAnalyzer end

# constructor for sequential toplevel JET analysis
function ToplevelAbstractAnalyzer(interp::ConcreteInterpreter, concretized::BitVector)
    # use the latest world age to take in newly added methods defined by `ConcreteInterpreter`
    world = get_world_counter()
    analyzer = ToplevelAbstractAnalyzer(interp)
    state = AnalyzerState(analyzer)
    newstate = AnalyzerState(state; world, concretized)
    return AbstractAnalyzer(analyzer, newstate)
end
