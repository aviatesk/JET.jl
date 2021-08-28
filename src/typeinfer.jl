# in this overload we will work on some meta/debug information management per inference frame
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    (; linfo, parent, result) = frame
    isentry = isnothing(parent)
    iscp = is_constant_propagated(frame)

    #= logging stage1 start =#
    local sec::Float64, depth::Int
    logger_activated = !isnothing(JETLogger(analyzer).inference_logger)
    if logger_activated
        sec = time()
        with_inference_logger(analyzer, ==(DEBUG_LOGGER_LEVEL)) do io
            depth = get_depth(analyzer)
            print_rails(io, depth)
            printstyled(io, "┌ @ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            print(io, linfo)
            if is_constant_propagated(frame)
                printstyled(io, " (constant prop': ", result.argtypes, ')'; color = NOERROR_COLOR)
            end
            file, line = get_file_line(linfo)
            print(io, ' ', file, ':', line)
            println(io)
            set_depth!(analyzer, get_depth(analyzer) + 1) # manipulate this only in debug mode
        end
    end
    #= logging stage1 end =#

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previous non-constant inference result (under the current design constant prop' always
    # happens after inference with non-constant abstract elements)
    # JET also needs that in order to reduce false positive reports, and here we will
    # throw-away previously-collected error reports that are "lineage" of this frame,
    # when it is being re-inferred with constants
    # NOTE `frame.linfo` is the exactly same object as that of the previous non-constant inference
    # IDEA we may still want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    # even when constant prop' reveals it never happ∫ens given the current constant arguments
    if iscp && !isentry
        filter!(!is_from_same_frame(parent.linfo, linfo), get_reports(parent.result))
    end

    @assert isa(result.src, JETResult)

    ret = @invoke typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    ret && @assert isa(result.src, isentry ? JETResult : JETCachedResult)

    if !iscp && !isentry
        # refinement for this `linfo` may change analysis result for parent frame
        # XXX: is this okay from performance perspective ?
        add_backedge!(linfo, parent)
    end

    #= logging stage2 start =#
    if logger_activated
        sec = round(time() - sec; digits = 3)
        with_inference_logger(analyzer, ==(INFO_LOGGER_LEVEL)) do io
            println(io, "inference on $linfo finished in $sec sec")
        end
        with_inference_logger(analyzer, ==(DEBUG_LOGGER_LEVEL)) do io
            print_rails(io, depth)
            printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            printstyled(io, frame.bestguess; color = TYPE_ANNOTATION_COLOR)
            println(io, " (", join(filter(!isnothing, (
                             linfo,
                             ret ? nothing : "in cycle",
                             string(length((isentry ? get_reports : get_cached_reports)(result)), " reports"),
                             string(sec, " sec"),
                             )), ", "),
                         ')')
            set_depth!(analyzer, get_depth(analyzer) - 1) # manipulate this only in debug mode
        end
    end
    #= logging stage2 end =#

    return ret
end

# TODO (JETAnalyzer) disable optimization for better performance, only do necessary analysis work by ourselves

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(interp::AbstractAnalyzer, frame::InferenceState)
    CC.typeinf_nocycle(interp, frame) || return false # frame is now part of a higher cycle
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
        finish(caller, interp)
        # finalize and record the linfo result
        caller.inferred = true
    end
    # collect results for the new expanded frame
    results = Tuple{InferenceState, Vector{Any}, Bool}[
            ( frames[i],
              frames[i].stmt_edges[1]::Vector{Any},
              frames[i].cached )
        for i in 1:length(frames) ]
    empty!(frames)
    for (frame, _, _) in results
        caller = frame.result
        opt = get_source(caller)
        if opt isa OptimizationState # implies `may_optimize(interp) === true`
            result_type = caller.result
            @assert !(result_type isa LimitedAccuracy)
            CC.optimize(interp, opt, OptimizationParams(interp), result_type)
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

    for (frame, edges, cached) in results
        caller = frame.result
        valid_worlds = caller.valid_worlds
        if CC.last(valid_worlds) >= get_world_counter()
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            CC.store_backedges(caller, edges)
        end
        CC.finish!(interp, frame)

        # XXX this is a dirty fix for performance problem, we need more "proper" fix
        # https://github.com/aviatesk/JET.jl/issues/75
        unique!(aggregation_policy(interp), get_reports(caller))

        if cached && !istoplevel(frame)
            CC.cache_result!(interp, caller)
        end
    end

    isentry = isnothing(frame.parent)
    for (frame, _, _) in results
        result = frame.result
        reports = get_reports(result)

        if !isentry
            # get back to the caller what we got from these results
            add_caller_cache!(interp, reports)

            # there are duplicated work here and `transform_result_for_cache`
            cache = InferenceErrorReportCache[]
            for report in reports
                cache_report!(cache, report)
            end
            set_cached_result!(result, cache)
        end
    end

    return true
end

function CC.finish!(analyzer::AbstractAnalyzer, frame::InferenceState)
    caller = frame.result

    # If we didn't transform the src for caching, we may have to transform
    # it anyway for users like typeinf_ext. Do that here.
    src = get_source(caller)
    if src isa OptimizationState # implies `may_optimize(interp) === true`
        opt = src
        if opt.ir !== nothing
            src = CC.ir_to_codeinf!(opt)
            set_source!(caller, src)
        end
    end

    # TODO make the report passes below `JETAnalyzer` specific

    if isnothing(src)
        # caught in cycle, similar error should have been reported where the source is available
        return src
    else
        code = (src::CodeInfo).code
        # report pass for (local) undef var error
        ReportPass(analyzer)(LocalUndefVarErrorReport, analyzer, frame, code)

        if frame.bestguess === Bottom
            # report pass for uncaught `throw` calls
            ReportPass(analyzer)(UncaughtExceptionReport, frame, code)
        else
            # the non-`Bottom` result may mean `throw` calls from the children frames
            # (if exists) are caught and not propagated here;
            # we don't want to cache `UncaughtExceptionReport`s for those calls for this frame
            # and its parents, and just filter them away now
            filter!(report->!isa(report, UncaughtExceptionReport), get_reports(caller))
        end

        return src
    end
end

"""
    is_from_same_frame(parent_linfo::MethodInstance, current_linfo::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport` is generated from `current_linfo`.
It also checks `current_linfo` is a "lineage" of `parent_linfo` (i.e. entered from it).

This function is supposed to be used to filter out reports collected from analysis on `current_linfo`
without using constants when entering into the constant analysis. As such, this function
assumes that when a report should be filtered out, the first elment of its virtual stack
frame `st` is for `parent_linfo` and the second element of that is for `current_linfo`.

Example: Assume `linfo2` will produce a report for some reason.
```
entry
└─ linfo1
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report1: linfo1->linfo2, report2: linfo3->linfo2)
   │  └─ linfo2 (report1: linfo1->linfo2, report2: linfo2)
   └─ linfo3′ (report1: linfo1->linfo2, ~~report2: linfo1->linfo3->linfo2~~)
```
In the example analysis above, `report2` will be filtered out on re-entering into `linfo3′`
(i.e. we're analyzing `linfo3` with constants argument), because
`is_from_same_frame(linfo1, linfo3)(report2)` returns `true`.
Note that `report1` is still kept there because of the lineage check, i.e.
`is_from_same_frame(linfo1, linfo3)(report1)` returns `false`.
"""
function is_from_same_frame(parent_linfo::MethodInstance,
                            current_linfo::MethodInstance,
                            )
    function (report::InferenceErrorReport)
        @inbounds begin
            vst = report.vst
            length(vst) > 1 || return false
            vst[1].linfo === parent_linfo || return false
            return vst[2].linfo === current_linfo
        end
    end
end

is_unreachable(@nospecialize(x)) = isa(x, ReturnNode) && !isdefined(x, :val)

@reportdef struct LocalUndefVarErrorReport <: InferenceErrorReport
    name::Symbol
end
get_msg(T::Type{LocalUndefVarErrorReport}, analyzer::AbstractAnalyzer, state, name::Symbol) =
    return "local variable $(name) is not defined"

# these report passes use `:throw_undef_if_not` and `:(unreachable)` introduced by the native
# optimization pass, and thus supposed to only work on post-optimization code
(::SoundPass)(::Type{LocalUndefVarErrorReport}, analyzer::AbstractAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, false)
(::BasicPass)(::Type{LocalUndefVarErrorReport}, analyzer::AbstractAnalyzer, frame::InferenceState, stmts::Vector{Any}) =
    report_undefined_local_slots!(analyzer, frame, stmts, true)

function report_undefined_local_slots!(analyzer::AbstractAnalyzer, frame::InferenceState, stmts::Vector{Any}, unsound::Bool)
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym = stmt.args[1]::Symbol

            # slots in toplevel frame may be a abstract global slot
            istoplevel(frame) && is_global_slot(analyzer, sym) && continue

            if unsound
                next_idx = idx + 1
                if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                    # the optimization so far has found this statement is never "reachable";
                    # JET reports it since it will invoke undef var error at runtime, or will just
                    # be dead code otherwise
                    add_new_report!(frame.result, LocalUndefVarErrorReport(analyzer, (frame, idx), sym))
                else
                    # by excluding this pass, this analysis accepts some false negatives and
                    # some undefined variable error may happen in actual execution (thus unsound)
                end
            else
                add_new_report!(frame.result, LocalUndefVarErrorReport(analyzer, (frame, idx), sym))
            end
        end
    end
end

"""
    UncaughtExceptionReport <: InferenceErrorReport

Represents general `throw` calls traced during inference.
This is reported only when it's not caught by control flow.
"""
@reportdef struct UncaughtExceptionReport <: InferenceErrorReport
    throw_calls::Vector{Tuple{Int,Expr}} # (pc, call)
end
function UncaughtExceptionReport(sv::InferenceState, throw_calls::Vector{Tuple{Int,Expr}})
    vf = get_virtual_frame(sv.linfo)
    msg = length(throw_calls) == 1 ? "may throw" : "may throw either of"
    sig = Any[]
    ncalls = length(throw_calls)
    for (i, (pc, call)) in enumerate(throw_calls)
        call_sig = _get_sig((sv, pc), call)
        append!(sig, call_sig)
        i ≠ ncalls && push!(sig, ", ")
    end
    return UncaughtExceptionReport([vf], msg, sig, throw_calls)
end

# report `throw` calls "appropriately"
# this error report pass is really special, and requires special care when overloaded
# since it is tightly bound to that of `SeriousExceptionReport`
function (::SoundBasicPass)(::Type{UncaughtExceptionReport}, frame::InferenceState, stmts::Vector{Any})
    @assert frame.bestguess === Bottom

    # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
    # `throw` calls
    # XXX it's possible that the `throw` calls within them are all caught but the other
    # critical errors still make the return type `Bottom`
    # NOTE to reduce the false positive cases described above, we count `throw` calls
    # after optimization, since it may have eliminated "unreachable" `throw` calls
    codelocs = frame.src.codelocs
    linetable = frame.src.linetable::Vector
    reported_locs = nothing
    for report in get_reports(frame.result)
        if isa(report, SeriousExceptionReport)
            if isnothing(reported_locs)
                reported_locs = LineInfoNode[]
            end
            push!(reported_locs, report.loc)
        end
    end
    throw_calls = nothing
    for (pc, stmt) in enumerate(stmts)
        isa(stmt, Expr) || continue
        is_throw_call(stmt) || continue
        # if this `throw` is already reported, don't duplciate
        if !isnothing(reported_locs) && linetable[codelocs[pc]]::LineInfoNode in reported_locs
            continue
        end
        if isnothing(throw_calls)
            throw_calls = Tuple{Int,Expr}[]
        end
        push!(throw_calls, (pc, stmt))
    end
    if !isnothing(throw_calls) && !isempty(throw_calls)
        add_new_report!(frame.result, UncaughtExceptionReport(frame, throw_calls))
    end
end
