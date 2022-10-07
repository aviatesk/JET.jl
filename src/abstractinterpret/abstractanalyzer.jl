# AbstractAnalyzer
# ================

"""
    abstract type AbstractAnalyzer <: AbstractInterpreter end

An interface type of analyzers that are built on top of [JET's analyzer framework](@ref AbstractAnalyzer-Framework).

When a new type `NewAnalyzer` implements the `AbstractAnalyzer` interface, it should be declared
as subtype of `AbstractAnalyzer`, and is expected to the following interfaces:

---
1. `NewAnalyzer(; jetconfigs...) -> NewAnalyzer`: \\
   Constructs new analyzer given [JET configurations](@ref) passed as `jetconfigs`.
---
2. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`: \\
   Returns the [`AnalyzerState`](@ref) for `analyzer::NewAnalyzer`.
---
3. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`: \\
   Constructs an new `NewAnalyzer` instance in the middle of JET's [top-level analysis](@ref toplevel)
   or [abstract interpretation](@ref abstractinterpret), given the previous
   `analyzer::NewAnalyzer` and [`state::AnalyzerState`](@ref AnalyzerState).
---
4. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`: \\
   Returns [`ReportPass`](@ref) used for `analyzer::NewAnalyzer`.
---
5. `get_cache_key(analyzer::NewAnalyzer) -> cache_key::UInt`: \\
   Returns [`cache_key::UInt`](@ref get_cache_key) used for `analyzer::NewAnalyzer`.
---

See also [`AnalyzerState`](@ref), [`ReportPass`](@ref) and [`get_cache_key`](@ref).

# Example

JET.jl defines its default error analyzer `JETAnalyzer <: AbstractAnalyzer` as the following
(modified a bit for the sake of simplicity):
```julia
# the default error analyzer for JET.jl
struct JETAnalyzer{RP<:ReportPass} <: AbstractAnalyzer
    report_pass::RP
    state::AnalyzerState
    __cache_key::UInt
end

# AbstractAnalyzer API requirements

function JETAnalyzer(;
    report_pass::ReportPass = BasicPass(),
    jetconfigs...)
    state = AnalyzerState(; jetconfigs...)
    cache_key = [...]
    return JETAnalyzer(report_pass, state, cache_key)
end
AnalyzerState(analyzer::JETAnalyzer) = analyzer.state
AbstractAnalyzer(analyzer::JETAnalyzer, state::AnalyzerState) = JETAnalyzer(ReportPass(analyzer), state)
ReportPass(analyzer::JETAnalyzer) = analyzer.report_pass
get_cache_key(analyzer::JETAnalyzer) = analyzer.__cache_key
```
"""
abstract type AbstractAnalyzer <: AbstractInterpreter end

# interface 1
# -----------
# 1. `NewAnalyzer(; jetconfigs...) -> NewAnalyzer`

@noinline function (::Type{Analyzer})(; jetconfigs...) where Analyzer<:AbstractAnalyzer
    error(lazy"""
    missing `$AbstractAnalyzer` API:
    `$Analyzer` is required to implement the `$Analyzer(; jetconfigs...) -> $Analyzer` interface.
    See the documentation of `$AbstractAnalyzer`.
    """)
end

# interface 2
# -----------
# 2. `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState`

"""
    JETResult

[`analyzer::AbstractAnalyzer`](@ref AbstractAnalyzer) manages [`InferenceErrorReport`](@ref)
associating it with `InferenceResult`.
`InferenceErrorReport`s found within currently-analyzed `result::InferenceResult` can be
accessed with `get_reports(analyzer, result)`.
"""
struct JETResult
    reports::Vector{InferenceErrorReport}
end

"""
    JETCachedResult

[`JETResult`](@ref JETResult) is transformed into `JETCachedResult` and then cached into
`codeinf::CodeInstance`.
When working with [`AbstractAnalyzer`](@ref), we can expect `codeinf` to have its field
`codeinf.inferred::JETCachedResult` as far as it's managed by [`JET_CACHE`](@ref).

[`InferenceErrorReport`](@ref)s found within already-analyzed `result::InferenceResult`
can be accessed with `get_cached_reports(analyzer, result)`.
"""
struct JETCachedResult
    src
    reports::Vector{InferenceErrorReport}
    JETCachedResult(@nospecialize(src), reports::Vector{InferenceErrorReport}) = new(src, reports)
end

const AnyJETResult = Union{JETResult,JETCachedResult}

"""
    mutable struct AnalyzerState
        ...
    end

The mutable object that holds various states that are consumed by all [`AbstractAnalyzer`](@ref)s.

---

    AnalyzerState(analyzer::AbstractAnalyzer) -> AnalyzerState

If `NewAnalyzer` implements the `AbstractAnalyzer` interface, `NewAnalyzer` should implement
this `AnalyzerState(analyzer::NewAnalyzer) -> AnalyzerState` interface.

A new `AnalyzerState` is supposed to be constructed using [JET configurations](@ref) passed
as keyword arguments `jetconfigs` of the [`NewAnalyzer(; jetconfigs...)`](@ref AbstractAnalyzer)
constructor, and the constructed `AnalyzerState` is usually kept within `NewAnalyzer` itself:
```julia
function NewAnalyzer(; jetconfigs...)
    ...
    state = AnalyzerState(; jetconfigs...)
    return NewAnalyzer(..., state)
end
AnalyzerState(analyzer::NewAnalyzer) = analyzer.state
```
"""
mutable struct AnalyzerState
    ## native ##

    native::NativeInterpreter

    ## `AbstractAnalyzer` ##

    results::IdDict{InferenceResult,AnyJETResult}

    # identity hash key for this state
    param_key::UInt

    # temporal stash to keep reports that are collected within the currently-analyzed frame
    # and should be appended to the caller when returning back to the caller frame next time
    caller_cache::Vector{InferenceErrorReport}

    # temporal stash to keep track of inference caller, to which reconstructed cached reports will be appended
    cacher::Union{Nothing,Pair{Symbol,InferenceResult}}

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

# constructor for fresh analysis
@jetconfigurable function AnalyzerState(world::UInt  = get_world_counter();
                                        results      = IdDict{InferenceResult,AnyJETResult}(),
                                        inf_params   = nothing,
                                        opt_params   = nothing,
                                        concretized  = _CONCRETIZED,
                                        toplevelmod  = _TOPLEVELMOD,
                                        global_slots = _GLOBAL_SLOTS,
                                        depth        = 0,
                                        jetconfigs...)
    isnothing(inf_params) && (inf_params = JETInferenceParams(; jetconfigs...))
    isnothing(opt_params) && (opt_params = JETOptimizationParams(; jetconfigs...))

    native       = NativeInterpreter(world; inf_params, opt_params)
    param_key    = get_param_key(inf_params)
    caller_cache = InferenceErrorReport[]

    return AnalyzerState(#=native::NativeInterpreter=# native,
                         #=results::IdDict{InferenceResult,AnyJETResult}=# results,
                         #=param_key::UInt=# param_key,
                         #=caller_cache::Vector{InferenceErrorReport}=# caller_cache,
                         #=cacher::Union{Nothing,InferenceResult}=# nothing,
                         #=concretized::BitVector=# concretized,
                         #=toplevelmod::Module=# toplevelmod,
                         #=global_slots::Dict{Int,Symbol}=# global_slots,
                         #=entry::Union{Nothing,MethodInstance}=# nothing,
                         #=depth::Int=# depth,
                         )
end

# dummies for non-toplevel analysis
module __toplevelmod__ end
const _CONCRETIZED  = BitVector()
const _TOPLEVELMOD  = __toplevelmod__
const _GLOBAL_SLOTS = Dict{Int,Symbol}()

# wrappers of `InferenceParams` and `OptimizationParams` that can accept JET configrations

"""
Configurations for abstract interpretation performed by JET.
These configurations will be active for all the entries.

You can configure any of the keyword parameters that `$InferenceParams` or `$OptimizationParams`
can take, e.g. `max_methods::Int = 3`, `union_splitting::Int = 4`.
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
JETInferenceParams(; ipo_constant_propagation::Bool        = true,
                     aggressive_constant_propagation::Bool = false,
                     unoptimize_throw_blocks::Bool         = true,
                     max_methods::Int                      = 3,
                     union_splitting::Int                  = 4,
                     apply_union_enum::Int                 = 8,
                     tupletype_depth::Int                  = 3,
                     tuple_splat::Int                      = 32,
                     __jetconfigs...) =
    return InferenceParams(; ipo_constant_propagation,
                             aggressive_constant_propagation,
                             unoptimize_throw_blocks,
                             max_methods,
                             union_splitting,
                             apply_union_enum,
                             tupletype_depth,
                             tuple_splat,
                             )
let
    @static if hasfield(OptimizationParams, :assume_fatal_throw)
        kwargs = :(inlining::Bool              = inlining_enabled(),
                   inline_cost_threshold::Int  = 100,
                   inline_nonleaf_penalty::Int = 1000,
                   inline_tupleret_bonus::Int  = 250,
                   inline_error_path_cost::Int = 20,
                   tuple_splat::Int            = 32,
                   assume_fatal_throw::Bool    = false,
                   )
    elseif isdefined(CC, :mark_throw_blocks!)
        kwargs = :(inlining::Bool              = inlining_enabled(),
                   inline_cost_threshold::Int  = 100,
                   inline_nonleaf_penalty::Int = 1000,
                   inline_tupleret_bonus::Int  = 250,
                   inline_error_path_cost::Int = 20,
                   max_methods::Int            = 3,
                   tuple_splat::Int            = 32,
                   union_splitting::Int        = 4,
                   )
    else
        kwargs = :(inlining::Bool                = inlining_enabled(),
                   inline_cost_threshold::Int    = 100,
                   inline_nonleaf_penalty::Int   = 1000,
                   inline_tupleret_bonus::Int    = 250,
                   inline_error_path_cost::Int   = 20,
                   max_methods::Int              = 3,
                   tuple_splat::Int              = 32,
                   union_splitting::Int          = 4,
                   unoptimize_throw_blocks::Bool = true,
                   )
    end
    kwargs_exs = Expr[]
    names = Symbol[]
    for x::Expr in kwargs.args
        @assert isexpr(x, :(=))
        push!(kwargs_exs, Expr(:kw, x.args...))
        lhs = first(x.args)
        @assert isexpr(lhs, :(::))
        push!(names, first(lhs.args)::Symbol)
    end
    push!(kwargs_exs, :(__jetconfigs...))
    @eval global function JETOptimizationParams(; $(kwargs_exs...))
        return OptimizationParams(; $(names...))
    end
end
# # assert here that they create same objects as the original constructors
@assert JETInferenceParams() == InferenceParams()
@assert JETOptimizationParams() == OptimizationParams()

@noinline function AnalyzerState(analyzer::AbstractAnalyzer)
    error(lazy"""
    missing `$AbstractAnalyzer` API:
    `$(typeof(analyzer))` is required to implement the `$AnalyzerState(analyzer::$(typeof(analyzer))) -> $AnalyzerState` interface.
    See the documentation of `$AbstractAnalyzer` and `$AnalyzerState`.
    """)
end

# interface 3
# -----------
# 3. `AbstractAnalyzer(analyzer::NewAnalyzer, state::AnalyzerState) -> NewAnalyzer`

@noinline function AbstractAnalyzer(analyzer::AbstractAnalyzer, state::AnalyzerState)
    error(lazy"""
    missing `$AbstractAnalyzer` API:
    `$(typeof(analyzer))` is required to implement the `$AbstractAnalyzer(analyzer::$(typeof(analyzer)), state::$AnalyzerState) -> $(typeof(analyzer))` interface.
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
    newanalyzer = AbstractAnalyzer(analyzer, newstate)
    may_init_cache!(newanalyzer)
    return newanalyzer
end

# constructor for sequential toplevel JET analysis
function AbstractAnalyzer(analyzer::T, concretized, toplevelmod) where {T<:AbstractAnalyzer}
    newstate = AnalyzerState(# update world age to take in newly added methods defined
                             # in a previous toplevel interpretation performed by `ConcreteInterpreter`
                             get_world_counter();
                             inf_params  = InferenceParams(analyzer),
                             opt_params  = OptimizationParams(analyzer),
                             concretized = concretized, # or construct partial `CodeInfo` from remaining abstract statements ?
                             toplevelmod = toplevelmod,
                             )
    newanalyzer = AbstractAnalyzer(analyzer, newstate)
    may_init_cache!(newanalyzer)
    return newanalyzer
end

# interface 4
# -----------
# 4. `ReportPass(analyzer::NewAnalyzer) -> ReportPass`

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

# forward to `BasicPass` to collect `GlobalUndefVarErrorReport`
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

@noinline function ReportPass(analyzer::AbstractAnalyzer)
    error(lazy"""
    missing `$AbstractAnalyzer` API:
    `$(typeof(analyzer))` is required to implement the `$ReportPass(analyzer::$(typeof(analyzer))) -> $ReportPass` interface.
    See the documentation of `$AbstractAnalyzer` and `$ReportPass`.
    """)
end
# some specific reports are necessary to be collected during `AbstractAnalyzer`'s core routine
# and ignore them by default (analyzer can opt-in to collect them by overloading this with
# their own report pass)
# otherwise, it means malformed report pass call, and we should inform users of it
function (rp::ReportPass)(T#=::Type{<:InferenceErrorReport}=#, @nospecialize(args...))
    if !(T === NativeRemark ||
         T === InvalidConstantRedefinition ||
         T === InvalidConstantDeclaration)
        throw(MethodError(rp, (T, args...)))
    end
    return false
end

# interface 5
# -----------
# 5. `get_cache_key(analyzer::NewAnalyzer) -> cache_key::UInt`

"""
    get_cache_key(analyzer::AbstractAnalyzer) -> cache_key::UInt

Returns the cache key for this `analyzer::AbstractAnalyzer`.
`AbstractAnalyzer`s that have different cache keys will use different cache so that their
analysis results are completely separated.

See also [`JET_CACHE`](@ref).
"""
@noinline function get_cache_key(analyzer::AbstractAnalyzer)
    error(lazy"""
    missing `$AbstractAnalyzer` API:
    `$(typeof(analyzer))` is required to implement the `$get_cache_key(analyzer::$(typeof(analyzer))) -> UInt` interface.
    See the documentation of `$AbstractAnalyzer` and `$get_cache_key`.
    """)
end

# InferenceResult
# ===============
# define how AbstractAnalyzer manages `InferenceResult`

Base.getindex(analyzer::AbstractAnalyzer, result::InferenceResult) = get_results(analyzer)[result]
Base.setindex!(analyzer::AbstractAnalyzer, jetresult::AnyJETResult, result::InferenceResult) = get_results(analyzer)[result] = jetresult

function init_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    analyzer[result] = JETResult(InferenceErrorReport[])
    return nothing
end
function set_cached_result!(analyzer::AbstractAnalyzer, result::InferenceResult, cache::Vector{InferenceErrorReport})
    analyzer[result] = JETCachedResult(result.src, cache)
    return nothing
end

get_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::JETResult).reports
get_cached_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::JETCachedResult).reports
get_any_reports(analyzer::AbstractAnalyzer, result::InferenceResult) = (analyzer[result]::AnyJETResult).reports

"""
    add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, report::InferenceErrorReport)

Adds new [`report::InferenceErrorReport`](@ref InferenceErrorReport) associated with `result::InferenceResult`.
"""
function add_new_report!(analyzer::AbstractAnalyzer, result::InferenceResult, report::InferenceErrorReport)
    push!(get_reports(analyzer, result), report)
    return report
end

function add_cached_report!(analyzer::AbstractAnalyzer, caller::InferenceResult, cached::InferenceErrorReport)
    cached = copy_report′(cached)
    push!(get_reports(analyzer, caller), cached)
    return cached
end

add_caller_cache!(analyzer::AbstractAnalyzer, report::InferenceErrorReport) = push!(get_caller_cache(analyzer), report)
add_caller_cache!(analyzer::AbstractAnalyzer, reports::Vector{InferenceErrorReport}) = append!(get_caller_cache(analyzer), reports)

# AbstractInterpreter
# ===================
# provide default implementations for the API requirements

CC.InferenceParams(analyzer::AbstractAnalyzer)    = InferenceParams(get_native(analyzer))
CC.OptimizationParams(analyzer::AbstractAnalyzer) = OptimizationParams(get_native(analyzer))
CC.get_world_counter(analyzer::AbstractAnalyzer)  = get_world_counter(get_native(analyzer))

# JET only works for runtime inference
CC.lock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing
CC.unlock_mi_inference(::AbstractAnalyzer, ::MethodInstance) = nothing

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` wraps remarks by `NativeInterpreter`.
"remarks" are information that Julia's native compiler emits about how its type inference goes,
and those remarks are less interesting in term of "error checking", so currently any of JET's
pre-defined report passes doesn't make any use of `NativeRemark`.
"""
@jetreport struct NativeRemark <: InferenceErrorReport
    s::String
end
function print_report_message(io::IO, (; s)::NativeRemark)
    print(io, s)
end
CC.add_remark!(analyzer::AbstractAnalyzer, sv::InferenceState, s) = ReportPass(analyzer)(NativeRemark, sv, s) # ignored by default

CC.may_optimize(analyzer::AbstractAnalyzer)      = true
CC.may_compress(analyzer::AbstractAnalyzer)      = false
CC.may_discard_trees(analyzer::AbstractAnalyzer) = false
CC.verbose_stmt_info(analyzer::AbstractAnalyzer) = false

@static if IS_AFTER_42082
let # overload `inlining_policy`
    @static if isdefined(CC, :CallInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(src), @nospecialize(info::CC.CallInfo), stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
        args_ex = :(analyzer::AbstractInterpreter,
            src::Any, info::CC.CallInfo, stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(src), stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
        args_ex = :(analyzer::AbstractInterpreter,
            src::Any, stmt_flag::UInt8, mi::MethodInstance, argtypes::Argtypes)
    end
    @eval begin
        @doc """
            inlining_policy(analyzer::AbstractAnalyzer, @nospecialize(src), ...) -> source::Any

        Implements inlining policy for `AbstractAnalyzer`.
        Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
        [`JETResult`](@ref) or [`JETCachedResult`](@ref), this implementation needs to forward
        their wrapped source to `inlining_policy(::AbstractInterpreter, ::Any, ::UInt8)`.
        """
        function CC.inlining_policy($(sigs_ex.args...))
            if isa(src, JETCachedResult)
                src = src.src
            end
            return @invoke CC.inlining_policy($(args_ex.args...))
        end
    end
end
else # @static if IS_AFTER_42082
@doc """
    inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
    jet_inlining_policy(@nospecialize(src)) -> source::Any

`jet_inlining_policy` implements `Core.Compiler.inlining_policy` for `AbstractAnalyzer`.
Since `AbstractAnalyzer` works on `InferenceResult` whose `src` field keeps
[`JETResult`](@ref) or [`JETCachedResult`](@ref), `jet_inlining_policy` forwards
their wrapped source to `Core.Compiler.default_inlining_policy`.
"""
CC.inlining_policy(::AbstractAnalyzer) = jet_inlining_policy
@inline function jet_inlining_policy(@nospecialize(src))
    if isa(src, JETCachedResult)
        src = src.src
    end
    return CC.default_inlining_policy(src)
end
end # @static if IS_AFTER_42082

# AbstractAnalyzer
# ================
# AbstractAnalyzer specific APIs

function get_param_key(inf_params::InferenceParams)
    h = @static UInt === UInt64 ? 0xa49bd446c0a5d90e : 0xe45361ac
    h = hash(inf_params, h)
    return h
end

function may_init_cache!(analyzer::AbstractAnalyzer)
    cache_key = get_cache_key(analyzer)
    if !haskey(JET_CACHE, cache_key)
        JET_CACHE[cache_key] = IdDict{MethodInstance,CodeInstance}()
    end
end

is_global_slot(analyzer::AbstractAnalyzer, slot::Int)   = slot in keys(get_global_slots(analyzer))
is_global_slot(analyzer::AbstractAnalyzer, slot::Slot)  = is_global_slot(analyzer, slot_id(slot))
is_global_slot(analyzer::AbstractAnalyzer, sym::Symbol) = sym in values(get_global_slots(analyzer))

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
function default_aggregation_policy(@nospecialize(report#=::InferenceErrorReport=#))
    return DefaultReportIdentity(
        typeof(Base.inferencebarrier(report))::DataType,
        report.sig,
        # VirtualFrameNoLinfo(first(report.vst)),
        VirtualFrameNoLinfo(last(report.vst)),
        )
end
@withmixedhash struct VirtualFrameNoLinfo
    file::Symbol
    line::Int
    sig::Signature
    # linfo::MethodInstance # ignore the idenity of `MethodInstace`
    VirtualFrameNoLinfo(vf::VirtualFrame) = new(vf.file, vf.line, vf.sig)
end
@withmixedhash struct DefaultReportIdentity
    T::DataType
    sig::Signature
    # entry_frame::VirtualFrameNoLinfo
    error_frame::VirtualFrameNoLinfo
end
