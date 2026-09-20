struct TrimAnalyzer <: AbstractAnalyzer
    state::AnalyzerState
    analysis_token::AnalysisToken
    method_table::CC.CachedMethodTable{CC.OverlayMethodTable}
    function TrimAnalyzer(state::AnalyzerState, analysis_token::AnalysisToken)
        method_table = CC.CachedMethodTable(CC.OverlayMethodTable(state.world, TRIM_METHOD_TABLE))
        return new(state, analysis_token, method_table)
    end
end
function TrimAnalyzer(state::AnalyzerState)
    analysis_cache_key = compute_hash(state.inf_params)
    analysis_token = get!(AnalysisToken, TRIM_ANALYZER_CACHE, analysis_cache_key)
    return TrimAnalyzer(state, analysis_token)
end

# AbstractInterpreter API
# =======================

# TrimAnalyzer does not need any sources, so discard them always
CC.method_table(analyzer::TrimAnalyzer) = analyzer.method_table

# AbstractAnalyzer API
# ====================

JETInterface.AnalyzerState(analyzer::TrimAnalyzer) = analyzer.state
function JETInterface.AbstractAnalyzer(analyzer::TrimAnalyzer, state::AnalyzerState)
    return TrimAnalyzer(state, analyzer.analysis_token)
end
JETInterface.AnalysisToken(analyzer::TrimAnalyzer) = analyzer.analysis_token

const TRIM_ANALYZER_CACHE = Dict{UInt, AnalysisToken}()

# TRIM_METHOD_TABLE
# ===============

using Base.Experimental: @overlay
Base.Experimental.@MethodTable TRIM_METHOD_TABLE

@eval @overlay TRIM_METHOD_TABLE Core.DomainError(@nospecialize(val), @nospecialize(msg::AbstractString)) = (@noinline; $(Expr(:new, :DomainError, :val, :msg)))

@overlay TRIM_METHOD_TABLE (f::Base.RedirectStdStream)(io::Core.CoreSTDOUT) = Base._redirect_io_global(io, f.unix_fd)

@overlay TRIM_METHOD_TABLE Base.depwarn(msg, funcsym; force::Bool=false) = nothing
@overlay TRIM_METHOD_TABLE Base._assert_tostring(msg) = ""
@overlay TRIM_METHOD_TABLE Base.reinit_stdio() = nothing
@overlay TRIM_METHOD_TABLE Base.JuliaSyntax.enable_in_core!() = nothing
@overlay TRIM_METHOD_TABLE Base.init_active_project() = Base.ACTIVE_PROJECT[] = nothing
@overlay TRIM_METHOD_TABLE Base.set_active_project(projfile::Union{AbstractString,Nothing}) = Base.ACTIVE_PROJECT[] = projfile
@overlay TRIM_METHOD_TABLE Base.disable_library_threading() = nothing
@overlay TRIM_METHOD_TABLE Base.start_profile_listener() = nothing
@overlay TRIM_METHOD_TABLE Base.invokelatest(f, args...; kwargs...) = f(args...; kwargs...)
@overlay TRIM_METHOD_TABLE function Base.sprint(f::F, args::Vararg{Any,N}; context=nothing, sizehint::Integer=0) where {F<:Function,N}
    s = IOBuffer(sizehint=sizehint)
    if context isa Tuple
        f(IOContext(s, context...), args...)
    elseif context !== nothing
        f(IOContext(s, context), args...)
    else
        f(s, args...)
    end
    String(Base._unsafe_take!(s))
end
function show_typeish(io::IO, @nospecialize(T))
    if T isa Type
        show(io, T)
    elseif T isa TypeVar
        print(io, (T::TypeVar).name)
    else
        print(io, "?")
    end
end
@overlay TRIM_METHOD_TABLE function Base.show(io::IO, T::Type)
    if T isa DataType
        print(io, T.name.name)
        if T !== T.name.wrapper && length(T.parameters) > 0
            print(io, "{")
            first = true
            for p in T.parameters
                if !first
                    print(io, ", ")
                end
                first = false
                if p isa Int
                    show(io, p)
                elseif p isa Type
                    show(io, p)
                elseif p isa Symbol
                    print(io, ":")
                    print(io, p)
                elseif p isa TypeVar
                    print(io, p.name)
                else
                    print(io, "?")
                end
            end
            print(io, "}")
        end
    elseif T isa Union
        print(io, "Union{")
        show_typeish(io, T.a)
        print(io, ", ")
        show_typeish(io, T.b)
        print(io, "}")
    elseif T isa UnionAll
        print(io, T.body::Type)
        print(io, " where ")
        print(io, T.var.name)
    end
end
@overlay TRIM_METHOD_TABLE Base.show_type_name(io::IO, tn::Core.TypeName) = print(io, tn.name)

@overlay TRIM_METHOD_TABLE Base.mapreduce(f::F, op::F2, A::Base.AbstractArrayOrBroadcasted; dims=:, init=Base._InitialValue()) where {F, F2} =
    Base._mapreduce_dim(f, op, init, A, dims)
@overlay TRIM_METHOD_TABLE Base.mapreduce(f::F, op::F2, A::Base.AbstractArrayOrBroadcasted...; kw...) where {F, F2} =
    reduce(op, map(f, A...); kw...)

@overlay TRIM_METHOD_TABLE Base._mapreduce_dim(f::F, op::F2, nt, A::Base.AbstractArrayOrBroadcasted, ::Colon) where {F, F2} =
    Base.mapfoldl_impl(f, op, nt, A)

@overlay TRIM_METHOD_TABLE Base._mapreduce_dim(f::F, op::F2, ::Base._InitialValue, A::Base.AbstractArrayOrBroadcasted, ::Colon) where {F, F2} =
    Base._mapreduce(f, op, IndexStyle(A), A)

@overlay TRIM_METHOD_TABLE Base._mapreduce_dim(f::F, op::F2, nt, A::Base.AbstractArrayOrBroadcasted, dims) where {F, F2} =
    Base.mapreducedim!(f, op, Base.reducedim_initarray(A, dims, nt), A)

@overlay TRIM_METHOD_TABLE Base._mapreduce_dim(f::F, op::F2, ::Base._InitialValue, A::Base.AbstractArrayOrBroadcasted, dims) where {F,F2} =
    Base.mapreducedim!(f, op, Base.reducedim_init(f, op, A, dims), A)

@overlay TRIM_METHOD_TABLE Base.mapreduce_empty_iter(f::F, op::F2, itr, ItrEltype) where {F, F2} =
    Base.reduce_empty_iter(Base.MappingRF(f, op), itr, ItrEltype)
@overlay TRIM_METHOD_TABLE Base.mapreduce_first(f::F, op::F2, x) where {F,F2} = Base.reduce_first(op, f(x))

@overlay TRIM_METHOD_TABLE Base._mapreduce(f::F, op::F2, A::Base.AbstractArrayOrBroadcasted) where {F,F2} = Base._mapreduce(f, op, Base.IndexStyle(A), A)
@overlay TRIM_METHOD_TABLE Base.mapreduce_empty(::typeof(identity), op::F, T) where {F} = Base.reduce_empty(op, T)
@overlay TRIM_METHOD_TABLE Base.mapreduce_empty(::typeof(abs), op::F, T) where {F}      = abs(Base.reduce_empty(op, T))
@overlay TRIM_METHOD_TABLE Base.mapreduce_empty(::typeof(abs2), op::F, T) where {F}     = abs2(Base.reduce_empty(op, T))

@overlay TRIM_METHOD_TABLE Base.Sys.__init_build() = nothing

# function __init__()
#     try
#         ccall((:__gmp_set_memory_functions, libgmp), Cvoid,
#             (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}),
#             cglobal(:jl_gc_counted_malloc),
#             cglobal(:jl_gc_counted_realloc_with_old_size),
#             cglobal(:jl_gc_counted_free_with_size))
#         ZERO.alloc, ZERO.size, ZERO.d = 0, 0, C_NULL
#         ONE.alloc, ONE.size, ONE.d = 1, 1, pointer(_ONE)
#     catch ex
#         Base.showerror_nostdio(ex, "WARNING: Error during initialization of module GMP")
#     end
#     # This only works with a patched version of GMP, ignore otherwise
#     try
#         ccall((:__gmp_set_alloc_overflow_function, libgmp), Cvoid,
#             (Ptr{Cvoid},),
#             cglobal(:jl_throw_out_of_memory_error))
#         ALLOC_OVERFLOW_FUNCTION[] = true
#     catch ex
#         # ErrorException("ccall: could not find function...")
#         if typeof(ex) != ErrorException
#             rethrow()
#         end
#     end
# end

@overlay TRIM_METHOD_TABLE Base.Sort.issorted(itr;
    lt::T=isless, by::F=identity, rev::Union{Bool,Nothing}=nothing, order::Base.Sort.Ordering=Forward) where {T,F} =
    Base.Sort.issorted(itr, Base.Sort.ord(lt,by,rev,order))

@overlay TRIM_METHOD_TABLE function Base.TOML.try_return_datetime(p, year, month, day, h, m, s, ms)
    return Base.TOML.DateTime(year, month, day, h, m, s, ms)
end
@overlay TRIM_METHOD_TABLE function Base.TOML.try_return_date(p, year, month, day)
    return Base.TOML.Date(year, month, day)
end
@overlay TRIM_METHOD_TABLE function Base.TOML.parse_local_time(l::Base.TOML.Parser)
    h = Base.TOML.@try Base.TOML.parse_int(l, false)
    h in 0:23 || return Base.TOML.ParserError(Base.TOML.ErrParsingDateTime)
    _, m, s, ms = Base.TOML.@try Base.TOML._parse_local_time(l, true)
    # TODO: Could potentially parse greater accuracy for the
    # fractional seconds here.
    return Base.TOML.try_return_time(l, h, m, s, ms)
end
@overlay TRIM_METHOD_TABLE function Base.TOML.try_return_time(p, h, m, s, ms)
    return Base.TOML.Time(h, m, s, ms)
end

# analysis injections
# ===================

function CC.abstract_call_gf_by_type(analyzer::TrimAnalyzer,
    @nospecialize(func), arginfo::CC.ArgInfo, si::CC.StmtInfo, @nospecialize(atype), sv::CC.InferenceState,
    max_methods::Int)
    ret = @invoke CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer,
        func::Any, arginfo::CC.ArgInfo, si::CC.StmtInfo, atype::Any, sv::CC.InferenceState, max_methods::Int)
    atype′ = Ref{Any}(atype)
    function after_abstract_call_gf_by_type(analyzer′::TrimAnalyzer, sv′::CC.InferenceState)
        ret′ = ret[]
        report_dispatch_error!(analyzer′, sv′, ret′, atype′[])
        return true
    end
    if isready(ret)
        after_abstract_call_gf_by_type(analyzer, sv)
    else
        push!(sv.tasks, after_abstract_call_gf_by_type)
    end
    return ret
end

# analysis
# ========

# DispatchErrorReport
# -------------------

@jetreport struct DispatchErrorReport <: InferenceErrorReport
    @nospecialize t # ::Union{Type, Vector{Type}}
end
JETInterface.print_report_message(io::IO, report::DispatchErrorReport) = print(io, "Unresolved call found")

function is_inlineable(analyzer::TrimAnalyzer, match, info)
    mi = CC.specialize_method(match; preexisting=true)
    isnothing(mi) && return false
    ci = get(CC.code_cache(analyzer), mi, nothing)
    isnothing(ci) && return false
    src = @atomic :monotonic ci.inferred
    return CC.src_inlining_policy(analyzer, src, info, zero(UInt32))
end

function report_dispatch_error!(analyzer::TrimAnalyzer, sv::CC.InferenceState, call::CC.CallMeta, @nospecialize(atype))
    (is_compileable_mi(sv.linfo) || is_entry(analyzer, sv.linfo)) || return
    info = call.info
    if info === CC.NoCallInfo()
        report = DispatchErrorReport(sv, atype)
        add_new_report!(analyzer, sv.result, report)
    else
        if info isa CC.ConstCallInfo
            info = info.call
        end
        if info isa CC.MethodMatchInfo
            for match in info.results
                if (isnothing(CC.get_compileable_sig(match.method, match.spec_types, match.sparams)) &&
                    !is_inlineable(analyzer, match, info))
                    report = DispatchErrorReport(sv, atype)
                    add_new_report!(analyzer, sv.result, report)
                end
            end
        else
            @assert info isa CC.UnionSplitInfo
            for info in info.split
                for match in info.results
                    if (isnothing(CC.get_compileable_sig(match.method, match.spec_types, match.sparams)) &&
                        !is_inlineable(analyzer, match, info))
                        report = DispatchErrorReport(sv, atype)
                        add_new_report!(analyzer, sv.result, report)
                    end
                end
            end
        end
    end
end

# Constructor
# ===========

# the entry constructor
function TrimAnalyzer(world::UInt = Base.get_world_counter(); jetconfigs...)
    jetconfigs = kwargs_dict(jetconfigs)
    jetconfigs[:max_methods] = 3
    # jetconfigs[:assume_bindings_static] = true # TODO
    state = AnalyzerState(world; jetconfigs...)
    return TrimAnalyzer(state)
end

JETInterface.valid_configurations(::TrimAnalyzer) = GENERAL_CONFIGURATIONS

# Interactive entry points
# ========================

function report_trim(args...; jetconfigs...)
    analyzer = TrimAnalyzer(; jetconfigs...)
    return analyze_and_report_call!(analyzer, args...; jetconfigs...)
end
macro report_trim(ex0...)
    return InteractiveUtils.gen_call_with_extracted_types_and_kwargs(__module__, :report_trim, ex0)
end

# Test.jl integration
# ===================

macro test_trim(ex0...)
    return call_test_ex(:report_trim, Symbol("@test_trim"), ex0, __module__, __source__)
end

function test_trim(args...; jetconfigs...)
    return func_test(report_trim, :test_trim, args...; jetconfigs...)
end
