# inter-procedural
# ================

function collect_callee_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    reports = get_caller_cache(analyzer)
    if !isempty(reports)
        vf = get_virtual_frame(sv)
        for report in reports
            pushfirst!(report.vst, vf)
            add_new_report!(analyzer, sv.result, report)
        end
        empty!(reports)
    end
end


let # overload `abstract_call_method`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, si::StmtInfo, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            method::Method, sig::Any, sparams::SimpleVector, hardlimit::Bool, si::StmtInfo, sv::InferenceState)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            method::Method, sig::Any, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    end
    @eval function CC.abstract_call_method($(sigs_ex.args...))
        ret = @invoke CC.abstract_call_method($(args_ex.args...))
        collect_callee_reports!(analyzer, sv)
        return ret
    end
end

let # overload `abstract_call_method_with_const_args`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, si::StmtInfo, match::MethodMatch,
            sv::InferenceState, $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, si::StmtInfo, match::MethodMatch,
            sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    elseif isdefined(CC, :InvokeCall)
        # https://github.com/JuliaLang/julia/pull/46743
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, invokecall::Union{Nothing,CC.InvokeCall})
    elseif hasmethod(CC.abstract_call_method_with_const_args, (AbstractInterpreter,
        MethodCallResult, Any, ArgInfo, MethodMatch,
        InferenceState, Any))
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, @nospecialize(invoketypes=nothing))
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState, invoketypes::Any)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            result::MethodCallResult, @nospecialize(f), arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            result::MethodCallResult, f::Any, arginfo::ArgInfo, match::MethodMatch,
            sv::InferenceState)
    end
    @eval function CC.abstract_call_method_with_const_args($(sigs_ex.args...))
        set_cacher!(analyzer, :abstract_call_method_with_const_args => sv.result)
        const_result =
            @invoke CC.abstract_call_method_with_const_args($(args_ex.args...))
        # we should make sure we reset the cacher because at this point we may have not hit
        # `CC.cache_lookup(linfo::MethodInstance, given_argtypes::Argtypes, view::AbstractAnalyzerView)`
        set_cacher!(analyzer, nothing)
        if const_result !== nothing
            # successful constant prop', we also need to update reports
            collect_callee_reports!(analyzer, sv)
        end
        return const_result
    end
end

let # overload `concrete_eval_call`
    sv_available = true
    @static if VERSION ≥ v"1.9.0-DEV.1502"
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
    elseif @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
        sv_available = false
    elseif isdefined(CC, :InvokeCall)
        # https://github.com/JuliaLang/julia/pull/46743
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
            $(Expr(:kw, :(invokecall::Union{Nothing,CC.InvokeCall}), :nothing)))
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState,
            invokecall::Union{Nothing,CC.InvokeCall})
    else
        sigs_ex = :(analyzer::AbstractAnalyzer,
            @nospecialize(f), result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
        args_ex = :(analyzer::AbstractInterpreter,
            f::Any, result::MethodCallResult, arginfo::ArgInfo, sv::InferenceState)
    end
    @eval function CC.concrete_eval_call($(sigs_ex.args...))
        ret = @invoke CC.concrete_eval_call($(args_ex.args...))
        if $(sv_available)
            if $(isdefined(CC, :ConstCallResults) ? :(ret isa CC.ConstCallResults) : :(ret !== nothing))
                # this frame has been happily concretized, now we throw away reports collected
                # during the previous abstract-interpretation based analysis
                filter_lineages!(analyzer, sv.result, result.edge::MethodInstance)
            end
        end
        return ret
    end
end

let # overload `abstract_call`
    @static if hasfield(InferenceParams, :max_methods) # VERSION ≥ v"1.10.0-DEV.105"
        sigs_ex = :(analyzer::AbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).max_methods))))
        args_ex = :(analyzer::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :(arginfo.argtypes)
    elseif @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).MAX_METHODS))))
        args_ex = :(analyzer::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :(arginfo.argtypes)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer, arginfo::ArgInfo, sv::InferenceState,
            $(Expr(:kw, :(max_methods::Int), :(InferenceParams(analyzer).MAX_METHODS))))
        args_ex = :(analyzer::AbstractInterpreter, arginfo::ArgInfo, sv::InferenceState,
            max_methods::Int)
        argtypes_ex = :(arginfo.argtypes)
    end
    @eval function CC.abstract_call($(sigs_ex.args...))
        ret = @invoke CC.abstract_call($(args_ex.args...))
        analyze_task_parallel_code!(analyzer, $(argtypes_ex), sv)
        return ret
    end
end

"""
    analyze_task_parallel_code!(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)

Adds special cased analysis pass for task parallelism.
In Julia's task parallelism implementation, parallel code is represented as closure and it's
wrapped in a `Task` object. `Core.Compiler.NativeInterpreter` doesn't infer nor optimize the
bodies of those closures when compiling code that creates parallel tasks, but JET will try
to run additional analysis pass by recurring into the closures.

See also: <https://github.com/aviatesk/JET.jl/issues/114>

!!! note
    JET won't do anything other than doing JET analysis, e.g. won't annotate return type
    of wrapped code block in order to not confuse the original `AbstractInterpreter` routine
    track <https://github.com/JuliaLang/julia/pull/39773> for the changes in native abstract
    interpretation routine.
"""
function analyze_task_parallel_code!(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
    f = singleton_type(argtypes[1])

    # TODO we should analyze a closure wrapped in a `Task` only when it's `schedule`d
    # But the `Task` construction may not happen in the same frame where it's `schedule`d
    # and so we may not be able to access to the closure at that point.
    # As a compromise, here we invoke the additional analysis on `Task` construction,
    # regardless of whether it's really `schedule`d or not.
    if f === Task &&
       length(argtypes) ≥ 2 &&
       (v = argtypes[2]; v ⊑ Function)
        # if we encounter `Task(::Function)`, try to get its inner function and run analysis on it
        # the closure can be a nullary lambda that really doesn't depend on
        # the captured environment, and in that case we can retrieve it as
        # a function object, otherwise we will try to retrieve the type of the closure
        ft = (isa(v, Const) ? Core.Typeof(v.val) :
              isa(v, Core.PartialStruct) ? v.typ :
              isa(v, DataType) ? v :
              return)
        analyze_additional_pass_by_type!(analyzer, Tuple{ft}, sv)
        return
    end

    return nothing
end

# run additional interpretation with a new analyzer
function analyze_additional_pass_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt), sv::InferenceState)
    newanalyzer = AbstractAnalyzer(analyzer)

    # in order to preserve the inference termination, we keep to use the current frame
    # and borrow the `AbstractInterpreter`'s cycle detection logic
    # XXX the additional analysis pass by `abstract_call_method` may involve various site-effects,
    # but what we're doing here is essentially equivalent to modifying the user code and inlining
    # the threaded code block as a usual code block, and thus the side-effects won't (hopefully)
    # confuse the abstract interpretation, which is supposed to terminate on any kind of code
    match = find_single_match(tt, newanalyzer)
    @static if @isdefined(StmtInfo)
        abstract_call_method(newanalyzer, match.method, match.spec_types, match.sparams, #=hardlimit=#false, #=si=#StmtInfo(false), sv)
    else
        abstract_call_method(newanalyzer, match.method, match.spec_types, match.sparams, #=hardlimit=#false, sv)
    end

    return nothing
end

let # overload `return_type_tfunc`
    @static if @isdefined(StmtInfo)
        sigs_ex = :(analyzer::AbstractAnalyzer, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
        args_ex = :(AbstractAnalyzer(analyzer)::AbstractInterpreter, argtypes::Argtypes, si::StmtInfo, sv::InferenceState)
    else
        sigs_ex = :(analyzer::AbstractAnalyzer, argtypes::Argtypes, sv::InferenceState)
        args_ex = :(AbstractAnalyzer(analyzer)::AbstractInterpreter, argtypes::Argtypes, sv::InferenceState)
    end
    # `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
    # and here we should NOT catch error reports detected within the simulated call
    # because it is really not any abstraction of actual execution
    @eval function CC.return_type_tfunc($(sigs_ex.args...))
        # stash and discard the result from the simulated call, and keep the original result (`result0`)
        result = sv.result
        oldresult = analyzer[result]
        init_result!(analyzer, result)
        ret = @invoke return_type_tfunc($(args_ex.args...))
        analyzer[result] = oldresult
        return ret
    end
end

# cache
# =====

cache_report!(cache, @nospecialize(report::InferenceErrorReport)) =
    push!(cache, copy_report′(report)::InferenceErrorReport)

struct AbstractAnalyzerView{Analyzer<:AbstractAnalyzer}
    analyzer::Analyzer
end

# global
# ------

function CC.code_cache(analyzer::AbstractAnalyzer)
    view = AbstractAnalyzerView(analyzer)
    worlds = WorldRange(get_world_counter(analyzer))
    return WorldView(view, worlds)
end

AnalysisCache(wvc::WorldView{<:AbstractAnalyzerView}) = AnalysisCache(wvc.cache.analyzer)

CC.haskey(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance) = haskey(AnalysisCache(wvc), mi)

function CC.typeinf_edge(analyzer::AbstractAnalyzer, method::Method, @nospecialize(atype), sparams::SimpleVector, caller::InferenceState)
    # enable the report cache restoration at `code = get(code_cache(interp), mi, nothing)`
    set_cacher!(analyzer, :typeinf_edge => caller.result)
    return @invoke typeinf_edge(analyzer::AbstractInterpreter, method::Method, atype::Any, sparams::SimpleVector, caller::InferenceState)
end

function CC.get(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance, default)
    codeinf = get(AnalysisCache(wvc), mi, default) # will ignore native code cache for a `MethodInstance` that is not analyzed by JET yet

    analyzer = wvc.cache.analyzer

    # XXX this relies on a very dirty analyzer state manipulation, the reason for this is
    # that this method (and `code_cache(::AbstractAnalyzer)`) can be called from multiple
    # contexts including edge inference, constant prop' heuristics and inlining, where we
    # want to use report cache only in edge inference, but we can't tell which context is
    # the caller of this specific method call here and thus can't tell whether we should
    # enable report cache reconstruction without the information
    # XXX move this logic into `typeinf_edge` ?
    cacher = get_cacher(analyzer)
    if isa(cacher, Pair{Symbol,InferenceResult})
        setter, caller = cacher
        if setter === :typeinf_edge
            if isa(codeinf, CodeInstance)
                # cache hit, now we need to append cached reports associated with this `MethodInstance`
                @static if VERSION ≥ v"1.9.0-DEV.1115"
                    inferred = @atomic :monotonic codeinf.inferred
                else
                    inferred = codeinf.inferred
                end
                for cached in (inferred::CachedAnalysisResult).reports
                    restored = add_cached_report!(analyzer, caller, cached)
                    @static if JET_DEV_MODE
                        actual, expected = first(restored.vst).linfo, mi
                        @assert actual === expected "invalid global cache restoration, expected $expected but got $actual"
                    end
                    add_caller_cache!(analyzer, restored) # should be updated in `abstract_call` (after exiting `typeinf_edge`)
                end
            end
            set_cacher!(analyzer, nothing)
        end
    end

    return codeinf
end

function CC.getindex(wvc::WorldView{<:AbstractAnalyzerView}, mi::MethodInstance)
    r = CC.get(wvc, mi, nothing)
    r === nothing && throw(KeyError(mi))
    return r::CodeInstance
end

@static if !hasmethod(CC.transform_result_for_cache, (
    AbstractInterpreter, MethodInstance, WorldRange, InferenceResult))
function CC.cache_result!(analyzer::AbstractAnalyzer, result::InferenceResult)
    valid_worlds = result.valid_worlds
    if CC.last(valid_worlds) == get_world_counter()
        # if we've successfully recorded all of the backedges in the global reverse-cache,
        # we can now widen our applicability in the global cache too
        valid_worlds = WorldRange(CC.first(valid_worlds), typemax(UInt))
    end
    # check if the existing linfo metadata is also sufficient to describe the current inference result
    # to decide if it is worth caching this
    linfo = result.linfo
    already_inferred = CC.already_inferred_quick_test(analyzer, linfo)
    if !already_inferred && CC.haskey(WorldView(code_cache(analyzer), valid_worlds), linfo)
        already_inferred = true
    end

    # TODO: also don't store inferred code if we've previously decided to interpret this function
    if !already_inferred
        inferred_result = transform_result_for_cache(analyzer, linfo, valid_worlds, result)
        CC.setindex!(code_cache(analyzer), CodeInstance(result, inferred_result, valid_worlds), linfo)
    end
    unlock_mi_inference(analyzer, linfo)
    nothing
end
end # @static if hasmethod(CC.transform_result_for_cache, (...))

function CC.transform_result_for_cache(analyzer::AbstractAnalyzer,
    linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    cache = InferenceErrorReport[]
    for report in get_reports(analyzer, result)
        @static if JET_DEV_MODE
            actual, expected = first(report.vst).linfo, linfo
            @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
        end
        cache_report!(cache, report)
    end
    @static if hasmethod(CC.transform_result_for_cache, (
        AbstractInterpreter, MethodInstance, WorldRange, InferenceResult))
        inferred_result = @invoke transform_result_for_cache(analyzer::AbstractInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, result::InferenceResult)
    else
        inferred_result = @invoke transform_result_for_cache(analyzer::AbstractInterpreter,
        linfo::MethodInstance, valid_worlds::WorldRange, result.src::Any, result.ipo_effects::CC.Effects)
    end
    return CachedAnalysisResult(inferred_result, cache)
end

function CC.setindex!(wvc::WorldView{<:AbstractAnalyzerView}, ci::CodeInstance, mi::MethodInstance)
    analysis_cache = AnalysisCache(wvc)
    add_jet_callback!(mi, analysis_cache)
    analysis_cache[mi] = ci
end

function add_jet_callback!(mi::MethodInstance, analysis_cache::AnalysisCache)
    callback = jet_callback(analysis_cache)
    if !isdefined(mi, :callbacks)
        mi.callbacks = Any[callback]
    else
        callbacks = mi.callbacks::Vector{Any}
        if !any(@nospecialize(cb)->cb===callback, callbacks)
            push!(callbacks, callback)
        end
    end
    return nothing
end

function jet_callback(analysis_cache::AnalysisCache)
    return function (replaced::MethodInstance, max_world,
                     seen::IdSet{MethodInstance} = IdSet{MethodInstance}())
        push!(seen, replaced)
        delete!(analysis_cache, replaced)
        if isdefined(replaced, :backedges)
            for item in replaced.backedges
                isa(item, MethodInstance) || continue # might be `Type` object representing an `invoke` signature
                mi = item
                mi in seen && continue # otherwise fail into an infinite loop
                var"#self#"(mi, max_world, seen)
            end
        end
        return nothing
    end
end

# local
# -----

CC.get_inference_cache(analyzer::AbstractAnalyzer) = AbstractAnalyzerView(analyzer)

let
    if isdefined(CC, :AbstractLattice)
        sigs_ex = :(lattice::CC.AbstractLattice, linfo::MethodInstance, given_argtypes::Argtypes, view::AbstractAnalyzerView)
        args_ex = :(lattice, linfo, given_argtypes, get_inf_cache(view.analyzer))
    else
        sigs_ex = :(linfo::MethodInstance, given_argtypes::Argtypes, view::AbstractAnalyzerView)
        args_ex = :(linfo, given_argtypes, get_inf_cache(view.analyzer))
    end
    @eval function CC.cache_lookup($(sigs_ex.args...))
        # XXX the very dirty analyzer state observation again
        # this method should only be called from the single context i.e. `abstract_call_method_with_const_args`,
        # and so we should reset the cacher immediately we reach here
        analyzer = view.analyzer
        setter, caller = get_cacher(analyzer)::Pair{Symbol,InferenceResult}
        @assert setter === :abstract_call_method_with_const_args
        set_cacher!(analyzer, nothing)

        inf_result = cache_lookup($(args_ex.args...))

        isa(inf_result, InferenceResult) || return inf_result

        # constant prop' hits a cycle (recur into same non-constant analysis), we just bail out
        isa(inf_result.result, InferenceState) && return inf_result

        # cache hit, try to restore local report caches

        # corresponds to the throw-away logic in `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
        filter_lineages!(analyzer, caller, linfo)

        for cached in get_cached_reports(analyzer, inf_result)
            restored = add_cached_report!(analyzer, caller, cached)
            @static if JET_DEV_MODE
                actual, expected = first(restored.vst).linfo, linfo
                @assert actual === expected "invalid local cache restoration, expected $expected but got $actual"
            end
            add_caller_cache!(analyzer, restored) # should be updated in `abstract_call_method_with_const_args`
        end

        return inf_result
    end
end

CC.push!(view::AbstractAnalyzerView, inf_result::InferenceResult) = CC.push!(get_inf_cache(view.analyzer), inf_result)

# main driver
# ===========

# in this overload we will work on some meta/debug information management per inference frame
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    (; linfo, parent, result) = frame
    isentry = isnothing(parent)

    #= logging stage1 start =#
    # io = stdout
    # sec = time()
    # depth = get_depth(analyzer)
    # print_rails(io, depth)
    # printstyled(io, "┌ @ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
    # print(io, linfo)
    # file, line = get_file_line(linfo)
    # print(io, ' ', file, ':', line)
    # println(io)
    # set_depth!(analyzer, get_depth(analyzer) + 1) # manipulate this only in debug mode
    #= logging stage1 end =#

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previous non-constant inference result (under the current design constant prop' always
    # happens after inference with non-constant abstract elements)
    # JET also needs that in order to reduce false positive reports, and here we will
    # throw-away previously-collected error reports that are "lineage" of this frame,
    # when it is being re-inferred with constants
    # NOTE `frame.linfo` is the exactly same object as that of the previous non-constant inference
    # IDEA we may still want to keep some "serious" error reports like `UndefVarErrorReport`
    # even when constant prop' reveals it never happ∫ens given the current constant arguments
    if is_constant_propagated(frame) && !isentry
        filter_lineages!(analyzer, parent.result, linfo)
    end

    ret = @invoke typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    #= logging stage2 start =#
    # elapsed = round(time() - sec; digits = 3)
    # print_rails(io, depth)
    # printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
    # printstyled(io, frame.bestguess; color = TYPE_ANNOTATION_COLOR)
    # println(io, " (", join(filter(!isnothing, (
    #                  linfo,
    #                  ret ? nothing : "in cycle",
    #                  string(length(get_any_reports(analyzer, result)), " reports"),
    #                  string(elapsed, " sec"),
    #                  )), ", "),
    #              ')')
    # set_depth!(analyzer, get_depth(analyzer) - 1) # manipulate this only in debug mode
    #= logging stage2 end =#

    return ret
end

"""
    islineage(parent::MethodInstance, current::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport`
- is generated from `current`, and
- is "lineage" of `parent` (i.e. entered from it).

This function is supposed to be used when additional analysis with extended lattice information
happens in order to filter out reports collected from `current` by analysis without
using that extended information. When a report should be filtered out, the first virtual
stack frame represents `parent` and the second does `current`.

Example:
```
entry
└─ linfo1 (report1: linfo1->linfo2)
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report2: linfo3->linfo2)
   │  └─ linfo2 (report2: linfo2)
   └─ linfo3′ (~~report2: linfo3->linfo2~~)
```
In the example analysis above, `report2` should be filtered out on re-entering into `linfo3′`
(i.e. when we're analyzing `linfo3` with constant arguments), nevertheless `report1` shouldn't
because it is not detected within `linfo3` but within `linfo1` (so it's not a "lineage of `linfo3`"):
- `islineage(linfo1, linfo3)(report2) === true`
- `islineage(linfo1, linfo3)(report1) === false`
"""
function islineage(parent::MethodInstance, current::MethodInstance)
    function (report::InferenceErrorReport)
        @nospecialize report
        @inbounds begin
            vst = report.vst
            length(vst) > 1 || return false
            vst[1].linfo === parent || return false
            return vst[2].linfo === current
        end
    end
end

function filter_lineages!(analyzer::AbstractAnalyzer, caller::InferenceResult, current::MethodInstance)
     filter!(!islineage(caller.linfo, current), get_reports(analyzer, caller))
end

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    CC.typeinf_nocycle(analyzer, frame) || return false # frame is now part of a higher cycle
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    valid_worlds = WorldRange()
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
        # might might not fully intersect these earlier, so do that now
        valid_worlds = CC.intersect(caller.valid_worlds, valid_worlds)
    end
    for caller in frames
        caller.valid_worlds = valid_worlds
        CC.finish(caller, analyzer)
        # finalize and record the linfo result
        caller.inferred = true
    end
    # NOTE we don't discard `InferenceState`s here so that some analyzers can use them in `finish!`
    # # collect results for the new expanded frame
    # results = Tuple{InferenceResult, Vector{Any}, Bool}[
    #         ( frames[i].result,
    #           frames[i].stmt_edges[1]::Vector{Any},
    #           frames[i].cached )
    #     for i in 1:length(frames) ]
    # empty!(frames)
    for frame in frames
        caller = frame.result
        opt = caller.src
        if (@static VERSION ≥ v"1.9.0-DEV.1636" ?
            (opt isa OptimizationState{typeof(analyzer)}) :
            (opt isa OptimizationState))
            CC.optimize(analyzer, opt, OptimizationParams(analyzer), caller)
            # # COMBAK we may want to enable inlining ?
            # if opt.const_api
            #     # XXX: The work in ir_to_codeinf! is essentially wasted. The only reason
            #     # we're doing it is so that code_llvm can return the code
            #     # for the `return ...::Const` (which never runs anyway). We should do this
            #     # as a post processing step instead.
            #     CC.ir_to_codeinf!(opt)
            #     if result_type isa Const
            #         caller.src = result_type
            #     else
            #         @assert CC.isconstType(result_type)
            #         caller.src = Const(result_type.parameters[1])
            #     end
            # end
            caller.valid_worlds = CC.getindex((opt.inlining.et::CC.EdgeTracker).valid_worlds)
        end
    end

    for frame in frames
        caller = frame.result
        edges = frame.stmt_edges[1]::Vector{Any}
        cached = frame.cached
        valid_worlds = caller.valid_worlds
        if CC.last(valid_worlds) >= get_world_counter()
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            CC.store_backedges(caller, edges)
        end
        CC.finish!(analyzer, frame)

        reports = get_reports(analyzer, caller)

        # XXX this is a dirty fix for performance problem, we need more "proper" fix
        # https://github.com/aviatesk/JET.jl/issues/75
        unique!(aggregation_policy(analyzer), reports)

        # global cache management
        if cached && !istoplevel(frame)
            CC.cache_result!(analyzer, caller)
        end

        if frame.parent !== nothing
            # inter-procedural handling: get back to the caller what we got from these results
            add_caller_cache!(analyzer, reports)

            # local cache management
            # TODO there are duplicated work here and `transform_result_for_cache`
            cache = InferenceErrorReport[]
            for report in reports
                cache_report!(cache, report)
            end
            set_cached_result!(analyzer, caller, cache)
        end
    end

    return true
end

# by default, this overload just is forwarded to the AbstractInterpreter's implementation
# but the only reason we have this overload is that some analyzers (like `JETAnalyzer`)
# can further overload this to generate `InferenceErrorReport` with an access to `frame`
function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
    return CC.finish!(analyzer, frame.result)
end

# top-level bridge
# ================

"""
    mutable struct AbstractGlobal
        t::Any     # analyzed type
        isconst::Bool # is this abstract global variable declarared as constant or not
    end

Wraps a global variable whose type is analyzed by abtract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
analysis may refer to or alter its type on future load and store operations.

!!! note
    The type of the wrapped global variable will be propagated only when in a toplevel frame,
    and thus we don't care about the analysis cache invalidation on a refinement of the
    wrapped global variable, since JET doesn't cache the toplevel frame.
"""
mutable struct AbstractGlobal
    t::Any        # analyzed type
    isconst::Bool # is this abstract global variable declarared as constant or not
    AbstractGlobal(@nospecialize(t), isconst::Bool) = new(t, isconst)
end

@doc """
    bail_out_toplevel_call(analyzer::AbstractAnalyzer, ...)

This overload allows JET to keep inference performed by `AbstractAnalyzer` going on
non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
@static if VERSION ≥ v"1.10.0-DEV.679"
    CC.bail_out_toplevel_call(::AbstractAnalyzer, ::CC.InferenceLoopState, ::InferenceState) = false
else
    CC.bail_out_toplevel_call(::AbstractAnalyzer, @nospecialize(sig), ::InferenceState) = false
end

function CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    istoplevel = JET.istoplevel(sv)

    if istoplevel
        if isa(e, SlotNumber) && is_global_slot(analyzer, e)
            if get_slottype((sv, get_currpc(sv)), e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract intepret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(get_toplevelmod(analyzer), get_slotname(sv, e))
            end
        end
    end

    ret = @invoke CC.abstract_eval_special_value(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)

    if istoplevel
        if isa(e, GlobalRef)
            mod, name = e.mod, e.name
            if isdefined(mod, name)
                # eagerly propagate the type of this global variable:
                # of course the traced type might be difference from its type in actual execution
                # e.g. we don't track global variable assignments that can happen somewhere
                # in this call graph, but it's highly possible this is a toplevel callsite
                # and we take a risk here since we can't enter the analysis otherwise
                val = getglobal(mod, name)
                ret = isa(val, AbstractGlobal) ? val.t : Const(val)
            end
        end
    end

    return ret
end

function CC.abstract_eval_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke CC.abstract_eval_value(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)

    # HACK if we encounter `_INACTIVE_EXCEPTION`, it means `ConcreteInterpreter` tried to
    # concretize an exception which was not actually thrown – yet the actual error hasn't
    # happened thanks to JuliaInterpreter's implementation detail, i.e. JuliaInterpreter
    # could retrieve `FrameData.last_exception`, which is initialized with
    # `_INACTIVE_EXCEPTION.instance` – but it's obviously not a sound approximation of an
    # actual execution and so here we will fix it to `Any`, since we don't analyze types of
    # exceptions in general
    if is_inactive_exception(ret)
        ret = Any
    end

    return ret
end

function is_inactive_exception(@nospecialize rt)
    return isa(rt, Const) && rt.val === _INACTIVE_EXCEPTION()
end

function CC.abstract_eval_statement(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if get_concretized(analyzer)[get_currpc(sv)]
            return Any # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end

    return @invoke CC.abstract_eval_statement(analyzer::AbstractInterpreter, e::Any, vtypes::VarTable, sv::InferenceState)
end

function CC.builtin_tfunction(analyzer::AbstractAnalyzer,
    @nospecialize(f), argtypes::Vector{Any},
    sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke CC.builtin_tfunction(analyzer::AbstractInterpreter,
        f::Any, argtypes::Vector{Any},
        sv::Union{InferenceState,Nothing})

    if f === getglobal
        if istoplevel_getproperty(sv)
            ret = narrow_toplevel_getglobal(argtypes, ret)
        end
    elseif f === Core.get_binding_type
        if istoplevel(sv)
            ret = narrow_toplevel_binding_type(argtypes, ret)
        end
    end

    return ret
end

# check if this frame is for `getproperty(::Module, ::Symbol)`,
# that may access to abstract global variable traced by `analyzer`
function istoplevel_getproperty(sv::InferenceState)
    def = sv.linfo.def
    isa(def, Method) || return false
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = sv.parent
    parent === nothing && return false
    return istoplevel(parent)
end

# if this `getglobal` access to a global variable in a module concretized by `AbstractAnalyzer`,
# eargely propagate its type (NOTE the logic here should be synced with the implementation
# of `abstract_eval_special_value(::AbstractAnalyzer, ...)`)
function narrow_toplevel_getglobal(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. constant or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    isdefined(gr.mod, gr.name) || return ret
    val = getglobal(gr.mod, gr.name)
    return isa(val, AbstractGlobal) ? val.t : Const(val)
end

# if the type for a variable hasn't been declared explicitly, return the narrower type
# declaration (`Const(Any)`) to allow JET to analyze the first assignment for the variable
# (, that would follow most of the time as generated by the frontend) more precisely
function narrow_toplevel_binding_type(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. already declared or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    @assert !isdefined(gr.mod, gr.name) "`get_binding_type` should resolve the already-defined variable type"
    return Const(Any)
end

function constant_globalref(argtypes::Vector{Any})
    length(argtypes) ≥ 2 || return nothing
    mod = argtypes[1]
    isa(mod, Const) || return nothing
    mod = mod.val
    isa(mod, Module) || return nothing
    sym = argtypes[2]
    isa(sym, Const) || return nothing
    sym = sym.val
    isa(sym, Symbol) || return nothing
    return GlobalRef(mod, sym)
end

function CC.finish(me::InferenceState, analyzer::AbstractAnalyzer)
    ret = @invoke CC.finish(me::InferenceState, analyzer::AbstractInterpreter)

    if istoplevel(me)
        # find assignments of abstract global variables, and assign types to them,
        # so that later analysis can refer to them

        stmts = me.src.code
        cfg = compute_basic_blocks(stmts)
        assigns = Dict{Int,Bool}() # slot id => is this deterministic
        for (pc, stmt) in enumerate(stmts)
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, SlotNumber)
                    slot = slot_id(lhs)
                    if is_global_slot(analyzer, slot)
                        isnd = is_assignment_nondeterministic(cfg, pc)

                        # COMBAK this approach is really not true when there're multiple
                        # assignments in different basic blocks
                        if haskey(assigns, slot)
                            assigns[slot] |= isnd
                        else
                            assigns[slot] = isnd
                        end
                    end
                end
            end
        end

        if !isempty(assigns)
            slottypes = collect_slottypes(me)
            for (slot, isnd) in assigns
                slotname = get_global_slots(analyzer)[slot]
                typ = slottypes[slot]
                set_abstract_global!(analyzer, get_toplevelmod(analyzer), slotname, typ, isnd, me)
            end
        end
    end

    return ret
end

is_global_slot(analyzer::AbstractAnalyzer, slot::Int) = slot in keys(get_global_slots(analyzer))
is_global_slot(analyzer::AbstractAnalyzer, slot::SlotNumber) = is_global_slot(analyzer, slot_id(slot))
is_global_slot(analyzer::AbstractAnalyzer, sym::Symbol) = sym in values(get_global_slots(analyzer))

# simple cfg analysis to check if the assignment at `pc` will happen non-deterministically
function is_assignment_nondeterministic(cfg::CFG, pc::Int)
    isnd = false

    blocks = cfg.blocks
    for (idx, block) in enumerate(blocks)
        if pc in rng(block)
            for block′ in blocks
                succs = block′.succs
                if idx in succs
                    isnd |= length(succs) > 1
                end
            end
        end
    end

    return isnd
end

# at this point all the types of SSA values are iterated to maximum fixed point,
# and we can compute types of slot as least upper bound of types of all the possible
# assignment of the slot (the type of assignment statement is available as SSA value type)
# the implementation is mostly same as `record_slot_assign!(sv::InferenceState)`, but
# we don't `widenconst` each SSA value type
@static if hasfield(InferenceState, :stmt_types)
function collect_slottypes(sv::InferenceState)
    states = sv.stmt_types
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    stmts = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in 1:length(sv.slottypes)]
    for i = 1:length(stmts)
        stmt = stmts[i]
        state = states[i]
        # find all reachable assignments to locals
        if isa(state, VarTable) && isexpr(stmt, :(=))
            lhs = first(stmt.args)
            if isa(lhs, SlotNumber)
                vt = ssavaluetypes[i] # don't widen const
                @assert vt !== NOT_FOUND "active slot in unreached region"
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    slottypes[id] = tmerge(otherTy, vt)
                end
            end
        end
    end
    return slottypes
end
else
function collect_slottypes(sv::InferenceState)
    body = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in 1:length(sv.slottypes)]
    ssavaluetypes = sv.ssavaluetypes
    for i = 1:length(body)
        expr = body[i]
        # find all reachable assignments to locals
        if CC.was_reached(sv, i) && isexpr(expr, :(=))
            lhs = expr.args[1]
            if isa(lhs, SlotNumber)
                typ = ssavaluetypes[i]
                @assert typ !== NOT_FOUND "active slot in unreached region"
                vt = typ
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    if otherTy === Bottom
                        slottypes[id] = vt
                    elseif otherTy === Any
                        slottypes[id] = Any
                    else
                        slottypes[id] = tmerge(otherTy, vt)
                    end
                end
            end
        end
    end
    return slottypes
end
end

function set_abstract_global!(analyzer::AbstractAnalyzer, mod::Module, name::Symbol, @nospecialize(t), isnd::Bool, sv::InferenceState)
    prev_agv = nothing
    prev_t = nothing
    isconst = is_constant_declared(name, sv)

    # check if this global variable is already assigned previously
    if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, AbstractGlobal)
            prev_t = val.t
            if val.isconst && (prev_t′ = widenconst(prev_t)) !== (t′ = widenconst(t))
                warn_invalid_const_global!(name)
                ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t′, t′)
                return
            end
            prev_agv = val
        else
            prev_t = Core.Typeof(val)
            if Base.isconst(mod, name)
                invalid = prev_t !== (t′ = widenconst(t))
                if invalid || !isa(t, Const)
                    warn_invalid_const_global!(name)
                    if invalid
                        ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t, t′) # ignored by default
                    end
                    return
                end
                # otherwise, we can just redefine this constant, and Julia will warn it
                ex = isconst ? :(const $name = $(QuoteNode(t.val))) : :($name = $(QuoteNode(t.val)))
                return Core.eval(mod, ex)
            end
        end
    end

    isnew = isnothing(prev_t)

    # if this constant declaration is invalid, just report it and bail out
    if isconst && !isnew
        warn_invalid_const_global!(name)
        ReportPass(analyzer)(InvalidConstantDeclaration, analyzer, sv, mod, name) # ignored by default
        return
    end

    # if this assignment happens non-deterministically, we need to take the previous type into account
    if isnd
        if !isnew # if this assignment is an initialization, we just need to use `t`
            t = tmerge(prev_t, t)
        end
    else
        # if this assignment happens deterministically, and the assigned value is known to be
        # constant statically, let's concretize it for good reasons;
        # we will be able to use it in concrete interpretation and so this allows to define
        # structs with type aliases, etc.
        local v
        if isa(t, Const)
            v = t.val
        elseif isconstType(t)
            v = t.parameters[1]
        elseif issingletontype(t)
            v = t.instance
        end
        if @isdefined v
            if isconst
                @assert isnew # means, this is a valid constant declaration
                return Core.eval(mod, :(const $name = $(QuoteNode(v))))
            else
                # we've checked `mod.name` wasn't declared as constant previously
                return Core.eval(mod, :($name = $(QuoteNode(v))))
            end
        end
    end

    # okay, we will define new abstract global variable from here on
    if isa(prev_agv, AbstractGlobal)
        return Core.eval(mod, :(let name = $name::$AbstractGlobal
            name.t = $t
            name
        end))
    else
        return Core.eval(mod, :($name = $(AbstractGlobal(t, isconst))))
    end
end

warn_invalid_const_global!(name::Symbol) = @warn """
JET.jl can't update the definition of this constant declared global variable: `$name`
This may fail, cause incorrect analysis, or produce unexpected errors.
"""

# IDEA we may want to hoist `InvalidConstXXX` errors into top-level errors

@jetreport struct InvalidConstantRedefinition <: InferenceErrorReport
    mod::Module
    name::Symbol
    @nospecialize t′
    @nospecialize t
end
function print_report_message(io::IO, report::InvalidConstantRedefinition)
    print(io, "invalid redefinition of constant `", report.mod, '.', report.name, "` (from `", report.t′, "` to `", report.t, "`)")
end
print_signature(::InvalidConstantRedefinition) = false

@jetreport struct InvalidConstantDeclaration <: InferenceErrorReport
    mod::Module
    name::Symbol
end
function print_report_message(io::IO, report::InvalidConstantDeclaration)
    print(io, "cannot declare a constant `", report.mod, '.', report.name, "`; it already has a value")
end
print_signature(::InvalidConstantDeclaration) = false

function is_constant_declared(name::Symbol, sv::InferenceState)
    return any(sv.src.code) do @nospecialize(x)
        if isexpr(x, :const)
            arg = first(x.args)
            # `transform_abstract_global_symbols!` replaces all the global symbols in this toplevel frame with `SlotNumber`s
            if isa(arg, SlotNumber)
                return get_slotname(sv, arg) === name
            end
        end
        return false
    end
end
