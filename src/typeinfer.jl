# in this overload we will work on some meta/debug information management
function CC.typeinf(interp::JETInterpreter, frame::InferenceState)
    linfo = frame.linfo

    #= logging start =#
    local sec, depth
    logger_activated = !isnothing(JETLogger(interp).inference_logger)
    if logger_activated
        sec = time()
        with_inference_logger(interp, ==(DEBUG_LOGGER_LEVEL)) do io
            depth = interp.depth

            print_rails(io, depth)
            printstyled(io, "┌ @ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            print(io, linfo)
            if is_constant_propagated(frame)
                printstyled(io, " (constant prop': ", frame.result.argtypes, ')'; color = NOERROR_COLOR)
            end
            file, line = get_file_line(linfo)
            print(io, ' ', file, ':', line)
            println(io)
            interp.depth += 1 # manipulate this only in debug mode
        end
    end
    #= logging end =#

    prev_frame = interp.current_frame
    interp.current_frame = frame

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    push!(ANALYZED_LINFOS, linfo) # analyzed !

    interp.current_frame = prev_frame

    #= logging start =#
    if logger_activated
        sec = round(time() - sec; digits = 3)
        with_inference_logger(interp, ==(INFO_LOGGER_LEVEL)) do io
            println(io, "inference on $linfo finished in $sec sec")
        end
        with_inference_logger(interp, ==(DEBUG_LOGGER_LEVEL)) do io
            print_rails(io, depth)
            printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            result = get_result(frame)
            isa(result, InferenceState) || printstyled(io, result; color = TYPE_ANNOTATION_COLOR)
            println(io, " (",
                        join(filter(!isnothing, (
                             linfo,
                             ret ? nothing : "in cycle",
                             "$(length(interp.reports)) reports",
                             "$sec sec"
                             )), ", "),
                        ')')
            interp.depth -= 1 # manipulate this only in debug mode
        end
    end
    #= logging end =#

    return ret
end

# TODO: disable optimization for performance, only do necessary analysis work by ourselves

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(interp::JETInterpreter, frame::InferenceState)
    linfo = frame.linfo
    parent = frame.parent
    isentry = parent === nothing
    iscp = is_constant_propagated(frame)
    reports = interp.reports

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previously-inferred lousy result; JET.jl also needs that to reduce false positive reports,
    # and so here we will throw-away previously-collected error reports that are "lineage" of
    # this frame, when it is being re-inferred with constant-prop'ed inputs
    # - constant prop' only happens after inference with non-constant abstract values (i.e. types)
    # - NOTE track the change in the native abstract interpretation logic, like
    #   * https://github.com/JuliaLang/julia/pull/39305
    # - IDEA: we may want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    #   even when constant prop' reveals it never happens given the current constant arguments
    if iscp
        # use `frame.linfo` instead of `frame` for lineage check since the program counter
        # for this frame is not initialized yet; note that `frame.linfo` is the exactly same
        # object as that of the previous only-type inference
        if !isentry
            filter!(!is_from_same_frame(parent.linfo, linfo), reports)
        end
    end

    reports_before             = Set(reports)
    uncaught_exceptions_before = Set(interp.uncaught_exceptions)

    ret = @invoke _typeinf(interp::AbstractInterpreter, frame::InferenceState)

    stmts = frame.src.code

    # report (local) undef var error
    # this only works when optimization is enabled, just because `:throw_undef_if_not` and
    # `:(unreachable)` are introduced by `optimize`
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym, _ = stmt.args

            # slots in toplevel frame may be a abstract global slot
            istoplevel(interp, frame) && is_global_slot(interp, sym) && continue

            next_idx = idx + 1
            if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                # the optimization so far has found this statement is never "reachable";
                # JET reports it since it will invoke undef var error at runtime, or will just
                # be dead code otherwise
                report!(interp, LocalUndefVarErrorReport(interp, frame, sym))
            # else
                # by excluding this pass, JET accepts some false negatives (i.e. don't report
                # those that may actually happen on actual execution)
            end
        end
    end

    # XXX this is a dirty fix for performance problem, we need more "proper" fix
    # https://github.com/aviatesk/JET.jl/issues/75
    unique!(report_identity_key, reports)

    reports_after = Set(reports)

    # report `throw` calls "appropriately"
    if get_result(frame) === Bottom
        # if the return type here is `Bottom` annotated, this _may_ mean there're uncaught
        # `throw` calls
        # XXX: well, it's possible that the `throw` calls within them are all caught but the
        # other critical errors make the return type `Bottom`
        # NOTE: to reduce the false positive `UncaughtExceptionReport`s described above, we count
        # `throw` calls here after optimization, since it may have eliminated "unreachable"
        # `throw` calls
        codelocs    = frame.src.codelocs
        linetable   = frame.src.linetable::Vector
        throw_locs  = LineInfoNode[]
        throw_calls = Expr[]
        for r in reports
            if isa(r, ExceptionReport) && last(r.st).linfo === linfo
                push!(throw_locs, r.lin)
            end
        end
        for (i, stmt) in enumerate(stmts)
            is_throw_call_expr(interp, frame, stmt) || continue
            # if this `throw` is already reported, don't duplciate
            linetable[codelocs[i]]::LineInfoNode in throw_locs && continue
            push!(throw_calls, stmt)
        end
        if !isempty(throw_calls)
            stash_uncaught_exception!(interp, UncaughtExceptionReport(interp, frame, throw_calls))
        end
    else
        # the non-`Bottom` result here may mean `throw` calls from the children frames
        # (if exists) are caught and not propagated here;
        # we don't want to cache `UncaughtExceptionReport`s for those calls for this frame
        # and its parents, so just filter them away
        empty!(interp.uncaught_exceptions)
    end

    uncaught_exceptions_after = Set(interp.uncaught_exceptions)

    # compute JET analysis results that should be cached for this linfo
    this_caches = union!(setdiff!(reports_after, reports_before),
                         setdiff!(uncaught_exceptions_after, uncaught_exceptions_before))

    if !isempty(this_caches)
        if iscp
            result = frame.result
            argtypes = result.argtypes
            cache = interp.cache
            @assert jet_cache_lookup(linfo, argtypes, cache) === nothing "invalid local caching $linfo, $argtypes"
            local_cache = InferenceErrorReportCache[]
            for report in this_caches
                # # TODO make this hold
                # @assert first(report.st).linfo === linfo "invalid local caching"
                cache_report!(local_cache, report)
            end
            # branching on https://github.com/JuliaLang/julia/pull/39972
            given_argtypes, overridden_by_const = @static if VERSION ≥ v"1.7.0-DEV.705"
                def = result.linfo.def
                va_overwride = isa(def, Method) && def.is_for_opaque_closure
                matching_cache_argtypes(linfo, argtypes, va_overwride)
            else
                matching_cache_argtypes(linfo, argtypes)
            end
            push!(cache, AnalysisResult(linfo, given_argtypes, overridden_by_const, local_cache))
        elseif frame.cached # only cache when `NativeInterpreter` does
            @assert !haskey(JET_GLOBAL_CACHE, linfo) || isentry "invalid global caching $linfo"
            global_cache = InferenceErrorReportCache[]
            for report in this_caches
                # # TODO make this hold
                # @assert first(report.st).linfo === linfo "invalid global caching"
                cache_report!(global_cache, report)
            end
            JET_GLOBAL_CACHE[linfo] = global_cache
        end
    end

    interp.to_be_updated = this_caches

    if !iscp && !isentry
        # refinement for this `linfo` may change analysis result for parent frame
        # XXX: is this okay from performance perspective ?
        add_backedge!(linfo, parent)
    end

    return ret
end

"""
    is_from_same_frame(parent_linfo::MethodInstance, current_linfo::MethodInstance) ->
        (report::InferenceErrorReport) -> Bool

Returns a function that checks if a given `InferenceErrorReport` is generated from `current_linfo`.
It also checks `current_linfo` is a "lineage" of `parent_linfo` (i.e. entered from it).

This function is supposed to be used to filter out reports from analysis on `current_linfo`
  without using constants when entering into the constant analysis. As such, this function
  assumes that when a report should be filtered out, the first elment of its virtual stack
  frame `st` is for `parent_linfo` and the second element of that is for `current_linfo`.

Example:
Assume `linfo2` will produce a report for some reason.
In the example analysis below, `report2` will be filtered out on re-entering into `linfo3`
  with constants (i.e. `linfo3′`). Note that `report1` is still kept there because of the
  lineage check.
```
entry
└─ linfo1
   ├─ linfo2 (report1: linfo2)
   ├─ linfo3 (report1: linfo1->linfo2, report2: linfo3->linfo2)
   │  └─ linfo2 (report1: linfo2, report2: linfo2)
   └─ linfo3′ (report1: linfo1->linfo2, ~~report2: linfo1->linfo3->linfo2~~)
```
"""
function is_from_same_frame(parent_linfo::MethodInstance,
                            current_linfo::MethodInstance,
                            )
    return @inbounds function (report::InferenceErrorReport)
        st = report.st
        length(st) > 1 || return false
        st[1].linfo === parent_linfo || return false
        return st[2].linfo === current_linfo
    end
end

is_unreachable(@nospecialize(x)) = isa(x, ReturnNode) && !isdefined(x, :val)

@withmixedhash struct ReportIdentityKey
    T::Type{<:InferenceErrorReport}
    sig::Vector{Any}
    # entry_frame::VirtualFrame
    error_frame::VirtualFrame
end

report_identity_key(report::T) where {T<:InferenceErrorReport} =
    ReportIdentityKey(T, report.sig, #=first(report.st),=# last(report.st))

# basically same as `is_throw_call`, but also toplevel module handling added
function is_throw_call_expr(interp::JETInterpreter, frame::InferenceState, @nospecialize(e))
    if isa(e, Expr)
        if e.head === :call
            f = e.args[1]
            if istoplevel(interp, frame) && isa(f, Symbol)
                f = GlobalRef(interp.toplevelmod, f)
            end
            if isa(f, GlobalRef)
                ff = CC.abstract_eval_global(f.mod, f.name)
                if isa(ff, Const) && ff.val === Core.throw
                    return true
                end
            end
        end
    end
    return false
end
