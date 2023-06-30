# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type of analyzers that are built on top of [JET's analyzer framework](@ref AbstractAnalyzer-Framework).

When a new type `NewAnalyzer` implements the `AbstractAnalyzer` interface, it should be declared
as subtype of `AbstractAnalyzer`, and is expected to the following interfaces:

---
1. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`: \\
   Returns the [`AnalyzerState`](@ref) for `analyzer::NewAnalyzer`.
---
2. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`: \\
   Constructs an new `NewAnalyzer` instance in the middle of JET's [top-level analysis](@ref toplevel)
   or [abstract interpretation](@ref abstractinterpret), given the previous
   `analyzer::NewAnalyzer` and [`state::AnalyzerState`](@ref AnalyzerState).
---
3. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`: \\
   Returns [`ReportPass`](@ref) used for `analyzer::NewAnalyzer`.
---
4. `AnalysisCache(analyzer::NewAnalyzer) -> analysis_cache::AnalysisCache`: \\
   Returns code cache used for `analyzer::NewAnalyzer`.
---

See also [`AnalyzerState`](@ref), [`ReportPass`](@ref) and [`AnalysisCache`](@ref).

# Example

JET.jl defines its default error analyzer `JETAnalyzer <: AbstractAnalyzer` as the following
(modified a bit for the sake of simplicity):
```julia
# the default error analyzer for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    state::AnalyzerState
    analysis_cache::AnalysisCache
    report_pass::RP
end

# AbstractAnalyzer API requirements
AnalyzerState(analyzer::JETAnalyzer) = analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) = JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) = analyzer.report_pass
AnalysisCache(analyzer::JETAnalyzer) = analyzer.analysis_cache
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

# interface 1
# -----------
# 1. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`

"""
    AnalysisResult

[`analyzer::AbstractAnalyzer`](@ref AbstractAnalyzer) manages [`InferenceErrorReport`](@ref)
by associating it with `InferenceResult`.
`InferenceErrorReport`s found within the currently-analyzed `result::InferenceResult` can be
accessed using `get_reports(analyzer, result)`.
"""
struct AnalysisResult
    reports::Vector{InferenceErrorReport}
end

"""
    CachedAnalysisResult

[`AnalysisResult`](@ref AnalysisResult) is transformed into `CachedAnalysisResult` when it is cached into
a global cache maintained by `AbstractAnalyzer`. That means,
`codeinf::CodeInstance = Core.Compiler.code_cache(analyzer::AbstractAnalyzer)[mi::MethodInstance])`
is expected to have its field `codeinf.inferred::CachedAnalysisResult`.

[`InferenceErrorReport`](@ref)s found within already-analyzed `result::InferenceResult`
can be accessed with `get_cached_reports(analyzer, result)`.
"""
struct CachedAnalysisResult
    src
    reports::Vector{InferenceErrorReport}
    CachedAnalysisResult(@nospecialize(src), reports::Vector{InferenceErrorReport}) = new(src, reports)
end

const AnyAnalysisResult = Union{AnalysisResult,CachedAnalysisResult}

"""
    mutable struct AnalyzerState
        ...
    end

The mutable object that holds various states that are consumed by all [`AbstractAnalyzer`](@ref)s.

---

    AnalyzerState(analyzer::AbstractAnalyzer) -> AnalyzerState

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
AnalyzerState(analyzer::NewAnalyzer) = analyzer.state
```
"""
mutable struct AnalyzerState
    ## AbstractInterpreter ##

    world::UInt
    inf_cache::Vector{InferenceResult}
    inf_params::InferenceParams
    opt_params::OptimizationParams

    ## AbstractAnalyzer ##

    results::IdDict{InferenceResult,AnyAnalysisResult}

    # the temporal stash to keep reports that are collected within the currently-analyzed frame:
    # they will be appended to the caller when returning back to the caller inference/optimization
    report_stash::Vector{InferenceErrorReport}

    # the temporal stash to keep track of the context of caller inference/optimization and
    # the caller itself, to which reconstructed cached reports will be appended
    cache_target::Union{Nothing,Pair{Symbol,InferenceResult}}

    ## abstract toplevel execution ##

    # will be used in toplevel analysis (skip inference on actually interpreted statements)
    concretized::BitVector

    # virtual toplevel module
    toplevelmod::Module

    # slots to represent toplevel global variables
    global_slots::Dict{Int,Symbol}

    # some `AbstractAnalyzer` may want to use this inforamion
    entry::Union{Nothing,MethodInstance}

    ## debug ##

    # records depth of call stack
    depth::Int
end

# define shortcut getter/setter methods for `AbstractAnalyzer`s
for fld in fieldnames(AnalyzerState)
    getter = Symbol("get_", fld)
    setter = Symbol("set_", fld, '!')
    @eval (@__MODULE__) @inline $getter(analyzer::AbstractAnalyzer)    = getfield(AnalyzerState(analyzer), $(QuoteNode(fld)))
    @eval (@__MODULE__) @inline $setter(analyzer::AbstractAnalyzer, v) = setfield!(AnalyzerState(analyzer), $(QuoteNode(fld)), v)
end

function AnalyzerState(world::UInt  = get_world_counter();
    results::IdDict{InferenceResult,AnyAnalysisResult} = IdDict{InferenceResult,AnyAnalysisResult}(),
    inf_params::Union{Nothing,InferenceParams} = nothing,
    opt_params::Union{Nothing,OptimizationParams} = nothing,
    concretized::BitVector = _CONCRETIZED,
    toplevelmod::Module = _TOPLEVELMOD,
    global_slots::Dict{Int,Symbol} = _GLOBAL_SLOTS,
    depth::Int = 0,
    jetconfigs...)
    isnothing(inf_params) && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params) && (opt_params = JETOptimizationParams(; jetconfigs...))
    inf_cache = InferenceResult[]
    report_stash = InferenceErrorReport[]
    return AnalyzerState(#=world::UInt=# world,
                         #=inf_cache::Vector{InferenceResult}=# inf_cache,
                         #=inf_params::InferenceParams=# inf_params,
                         #=opt_params::OptimizationParams=# opt_params,
                         #=results::IdDict{InferenceResult,AnyAnalysisResult}=# results,
                         #=report_stash::Vector{InferenceErrorReport}=# report_stash,
                         #=cache_target::Union{Nothing,InferenceResult}=# nothing,
                         #=concretized::BitVector=# concretized,
                         #=toplevelmod::Module=# toplevelmod,
                         #=global_slots::Dict{Int,Symbol}=# global_slots,
                         #=entry::Union{Nothing,MethodInstance}=# nothing,
                         #=depth::Int=# depth)
end

# dummies for non-toplevel analysis
module __toplevelmod__ end
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = __toplevelmod__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

"""
Configurations for abstract interpretation performed by JET.
These configurations will be active for all the entries.

You can configure any of the keyword parameters that [`Core.Compiler.InferenceParams`](@ref)
or [`Core.Compiler.OptimizationParams`](@ref) can take, e.g. `max_methods`:
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

See also [`Core.Compiler.InferenceParams`](@ref) and [`Core.Compiler.OptimizationParams`](@ref).

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
- `unoptimize_throw_blocks::Bool = false` \\
  Turn this on to skip analysis on code blocks that will eventually lead to a `throw` call.
  This configuration improves the analysis performance, but it's better to be turned off
  to get a "proper" analysis result, just because there may be other errors even in those "throw blocks".
---
"""
function JETInferenceParams end
function JETOptimizationParams end

# define wrappers of `InferenceParams(...)` and `OptimizationParams(...)` that can accept JET configurations
@static if hasfield(InferenceParams, :max_methods) # VERSION ≥ v"1.10.0-DEV.105"
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
else
function JETInferenceParams(
    params::InferenceParams = InferenceParams();
    ipo_constant_propagation::Bool = params.ipo_constant_propagation,
    aggressive_constant_propagation::Bool = params.aggressive_constant_propagation,
    unoptimize_throw_blocks::Bool = params.unoptimize_throw_blocks,
    max_methods::Int = params.MAX_METHODS,
    union_splitting::Int = params.MAX_UNION_SPLITTING,
    apply_union_enum::Int = params.MAX_APPLY_UNION_ENUM,
    tupletype_depth::Int = params.TUPLE_COMPLEXITY_LIMIT_DEPTH,
    tuple_splat::Int = params.MAX_TUPLE_SPLAT,
    __jetconfigs...)
    return InferenceParams(; ipo_constant_propagation,
                             aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             max_methods,
                             union_splitting,
                             apply_union_enum,
                             tupletype_depth,
                             tuple_splat)
end
function JETOptimizationParams(
    params::OptimizationParams=OptimizationParams();
    inlining::Bool = params.inlining,
    inline_cost_threshold::Int = params.inline_cost_threshold,
    inline_nonleaf_penalty::Int = params.inline_nonleaf_penalty,
    inline_tupleret_bonus::Int = params.inline_tupleret_bonus,
    inline_error_path_cost::Int = params.inline_error_path_cost,
    tuple_splat::Int = params.MAX_TUPLE_SPLAT,
    compilesig_invokes::Bool = params.compilesig_invokes,
    trust_inference::Bool = params.trust_inference,
    assume_fatal_throw::Bool = params.assume_fatal_throw,
    __jetconfigs...)
    return OptimizationParams(; inlining,
                               inline_cost_threshold,
                               inline_nonleaf_penalty,
                               inline_tupleret_bonus,
                               inline_error_path_cost,
                               tuple_splat,
                               compilesig_invokes,
                               trust_inference,
                               assume_fatal_throw)
end
end # hasfield(InferenceParams, :max_methods) # VERSION ≥ v"1.10.0-DEV.105"

# assert that the wrappers create same objects as the original constructors
for (Params, Func) = ((InferenceParams, JETInferenceParams),
                      (OptimizationParams, JETOptimizationParams))
    @assert Params() == Func()
end
@static if hasfield(InferenceParams, :max_methods) # VERSION ≥ v"1.10.0-DEV.105"
    # assert that `Effects(::Effects; ...)`-like constructors work as expected
    @assert JETInferenceParams(InferenceParams(); max_methods=1).max_methods == 1
    @assert !JETOptimizationParams(OptimizationParams(); inlining=false).inlining
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
    newstate = AnalyzerState(get_world_counter(analyzer);
                             results    = get_results(analyzer),
                             inf_params = InferenceParams(analyzer),
                             opt_params = OptimizationParams(analyzer),
                             depth      = get_depth(analyzer),
                             )
    return AbstractAnalyzer(analyzer, newstate)
end

# constructor for sequential toplevel JET analysis
function AbstractAnalyzer(analyzer::T, concretized::BitVector, toplevelmod::Module;
    # update world age to take in newly added methods defined by `ConcreteInterpreter`
    world::UInt = get_world_counter()
    ) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(world;
                             inf_params = InferenceParams(analyzer),
                             opt_params = OptimizationParams(analyzer),
                             concretized, # or construct partial `CodeInfo` from remaining abstract statements ?
                             toplevelmod,
                             )
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

    ReportPass(analyzer::AbstractAnalyzer) -> ReportPass

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
# some specific reports are necessary to be collected during `AbstractAnalyzer`'s core routine
# and ignore them by default (analyzer can opt-in to collect them by overloading this with
# their own report pass)
# otherwise, it means malformed report pass call, and we should inform users of it
function (rp::ReportPass)(T#=::Type{<:InferenceErrorReport}=#, @nospecialize(args...))
    if !(T === InvalidConstantRedefinition || T === InvalidConstantDeclaration)
        throw(MethodError(rp, (T, args...)))
    end
    return false
end

# interface 4
# -----------
# 4. `AnalysisCache(analyzer::NewAnalyzer) -> analysis_cache::AnalysisCache`

"""
    AnalysisCache

JET's internal representation of a global analysis cache.

---

    AnalysisCache(analyzer::AbstractAnalyzer) -> analysis_cache::AnalysisCache

Returns [`AnalysisCache`](@ref) for this `analyzer::AbstractAnalyzer`.
`AbstractAnalyzer` instances can share the same cache if they perform the same analysis,
otherwise their cache should be separated.
"""
struct AnalysisCache
    cache::IdDict{MethodInstance,CodeInstance}
end
AnalysisCache() = AnalysisCache(IdDict{MethodInstance,CodeInstance}())

Base.haskey(analysis_cache::AnalysisCache, mi::MethodInstance) = haskey(analysis_cache.cache, mi)
Base.get(analysis_cache::AnalysisCache, mi::MethodInstance, default) = get(analysis_cache.cache, mi, default)
Base.getindex(analysis_cache::AnalysisCache, mi::MethodInstance) = getindex(analysis_cache.cache, mi)
Base.setindex!(analysis_cache::AnalysisCache, ci::CodeInstance, mi::MethodInstance) = setindex!(analysis_cache.cache, ci, mi)
Base.delete!(analysis_cache::AnalysisCache, mi::MethodInstance) = delete!(analysis_cache.cache, mi)
Base.show(io::IO, analysis_cache::AnalysisCache) = print(io, typeof(analysis_cache), "(", length(analysis_cache.cache), " entries)")

@noinline function AnalysisCache(analyzer::AbstractAnalyzer)
    AnalyzerType = nameof(typeof(analyzer))
    error(lazy"""
    Missing `$AbstractAnalyzer` API:
    `$AnalyzerType` is required to implement the `$AnalysisCache(analyzer::$AnalyzerType) -> $AnalysisCache` interface.
    See the documentation of `$AbstractAnalyzer` and `$AnalysisCache`.
    """)
end

# optional API
# ------------

"""
    valid_configurations(analyzer::AbstractAnalyzer) -> names or nothing

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
Base.setindex!(analyzer::AbstractAnalyzer, analysis_result::AnyAnalysisResult, result::InferenceResult) =
    get_results(analyzer)[result] = analysis_result

function init_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    analyzer[result] = AnalysisResult(InferenceErrorReport[])
    return nothing
end
function set_cached_result!(analyzer::AbstractAnalyzer, result::InferenceResult, cache::Vector{InferenceErrorReport})
    analyzer[result] = CachedAnalysisResult(result.src, cache)
    return nothing
end

get_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::AnalysisResult).reports
get_cached_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::CachedAnalysisResult).reports
get_any_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::AnyAnalysisResult).reports

"""
    add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, report::InferenceErrorReport)

Adds new [`report::InferenceErrorReport`](@ref InferenceErrorReport) associated with `result::InferenceResult`.
"""
function add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, @nospecialize(report::InferenceErrorReport))
    push!(get_reports(analyzer, result), report)
    return report
end

function add_cached_report!(analyzer::AbstractAnalyzer, caller::InferenceResult, @nospecialize(cached::InferenceErrorReport))
    cached = copy_report′(cached)
    push!(get_reports(analyzer, caller), cached)
    return cached
end

stash_report!(analyzer::AbstractAnalyzer, @nospecialize(report::InferenceErrorReport)) = push!(get_report_stash(analyzer), report)
stash_report!(analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport}) = append!(get_report_stash(analyzer), reports)

# AbstractInterpreter
# ===================
# provide default implementations for the API requirements

CC.InferenceParams(analyzer::AbstractAnalyzer)    = get_inf_params(analyzer)
CC.OptimizationParams(analyzer::AbstractAnalyzer) = get_opt_params(analyzer)
CC.get_world_counter(analyzer::AbstractAnalyzer)  = get_world(analyzer)

CC.may_compress(analyzer::AbstractAnalyzer)      = false
CC.may_discard_trees(analyzer::AbstractAnalyzer) = false

let # overload `inlining_policy`
    @static if isdefined(CC, :InliningInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer, @nospecialize(src), iinfo::CC.InliningInfo)
        args_ex = :(analyzer::AbstractInterpreter, src::Any, iinfo::CC.InliningInfo)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(src), @nospecialize(info::CC.CallInfo), stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
        args_ex = :(analyzer::AbstractInterpreter,
            src::Any, info::CC.CallInfo, stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
    end
    @eval begin
        @doc """
            inlining_policy(analyzer::AbstractAnalyzer, @nospecialize(src), ...) -> source::Any

        Implements inlining policy for `AbstractAnalyzer`.
        Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
        [`AnalysisResult`](@ref) or [`CachedAnalysisResult`](@ref), this implementation needs to forward
        their wrapped source to `inlining_policy(::AbstractInterpreter, ::Any, ::UInt8)`.
        """
        function CC.inlining_policy($(sigs_ex.args...))
            if isa(src, CachedAnalysisResult)
                src = src.src
            end
            return @invoke CC.inlining_policy($(args_ex.args...))
        end
    end
end
