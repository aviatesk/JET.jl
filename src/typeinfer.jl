# in this overload we will work on some meta/debug information management
function CC.typeinf(interp::JETInterpreter, frame::InferenceState)
    # # print debug info before typeinf
    # depth = interp.depth
    # io = stdout::IO
    # color = RAIL_COLORS[(depth+1)%N_RAILS+1]
    # print_rails(io, depth)
    # printstyled(io, "┌ @ "; color)
    # print(io, frame.linfo)
    # if is_constant_propagated(frame)
    #     printstyled(io, " (constant prop': ", frame.result.argtypes, ')'; color = :cyan)
    # end
    # file, line = get_file_line(frame.linfo)
    # print(io, ' ', file, ':', line)
    # println(io)

    prev_frame = interp.current_frame
    interp.current_frame = frame
    interp.depth += 1 # for debug

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    push!(ANALYZED_LINFOS, frame.linfo) # analyzed !

    interp.current_frame = prev_frame
    interp.depth -= 1 # for debug

    # # print debug info after typeinf
    # print_rails(io, depth)
    # printstyled(io, "└─→ "; color)
    # result = get_result(frame)
    # isa(result, InferenceState) || printstyled(io, get_result(frame); color = TYPE_ANNOTATION_COLOR)
    # printlnstyled(io, " ($(length(interp.reports)) reports)"; color = ERROR_COLOR)

    return ret
end

# TODO: disable optimization for performance, only do necessary analysis work by ourselves

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(interp::JETInterpreter, frame::InferenceState)
    linfo = frame.linfo
    iscp = is_constant_propagated(frame)
    reports = interp.reports

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previously-inferred lousy result; JET.jl also needs that to reduce false positive reports,
    # and so here we will throw-away previously-collected error reports that are "lineage" of
    # this frame, when it is being re-inferred with constant-prop'ed inputs
    # - constant prop' only happens after inference with non-constant abstract values (i.e. types)
    # - xref to track the change in the native abstract interpretation logic:
    #   https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    # - IDEA: we may want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    #   even when constant prop' reveals it never happens given the current constant arguments
    if iscp
        # use `frame.linfo` instead of `frame` for lineage check since the program counter
        # for this frame is not initialized yet; note that `frame.linfo` is the exactly same
        # object as that of the previous only-type inference
        filter!(r->!is_lineage(r.lineage, frame.parent::InferenceState, linfo), reports)
    end

    before = Set(reports)

    ret = @invoke _typeinf(interp::AbstractInterpreter, frame::InferenceState)

    stmts = frame.src.code

    # report (local) undef var error
    # this only works when optimization is enabled, just because `:throw_undef_if_not` and
    # `:(unreachable)` are introduced by `optimize`
    if may_optimize(interp)
        for (idx, stmt) in enumerate(stmts)
            if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
                sym, _ = stmt.args
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
    end

    # XXX this is a dirty fix for performance problem, we need more "proper" fix
    # https://github.com/aviatesk/JET.jl/issues/75
    unique!(get_identity_key, interp.reports)

    after = Set(interp.reports)
    reports_for_this_linfo = setdiff(after, before)

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
        linetable   = frame.src.linetable
        throw_locs  = LineInfoNode[]
        throw_calls = Expr[]
        for r in reports
            if isa(r, ExceptionReport) && last(r.st).linfo === linfo
                push!(throw_locs, r.lin)
            end
        end
        for (i, stmt) in enumerate(stmts)
            is_throw_call′(stmt) || continue
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

    # cache uncaught exceptions so far
    if !isempty(interp.uncaught_exceptions)
        push!(reports_for_this_linfo, interp.uncaught_exceptions...)
    end

    if !isempty(reports_for_this_linfo)
        if iscp
            argtypes = frame.result.argtypes
            cache = interp.cache

            @assert !haskey(cache, argtypes) "invalid local caching"
            local_cache = cache[argtypes] = InferenceErrorReportCache[]

            for report in reports_for_this_linfo
                cache_report!(report, linfo, local_cache)
            end
        elseif frame.cached # only cache when `NativeInterpreter` does
            @assert !haskey(JET_GLOBAL_CACHE, linfo) || isnothing(frame.parent) "invalid global caching"
            global_cache = JET_GLOBAL_CACHE[linfo] = InferenceErrorReportCache[]

            for report in reports_for_this_linfo
                cache_report!(report, linfo, global_cache)
            end
        end
    end

    if !iscp
        # refinement for this `linfo` may change analysis result for parent frame
        # XXX: is this okay from performance perspective ?
        if (parent = frame.parent; !isnothing(parent))
            add_backedge!(linfo, parent)
        end
    end

    return ret
end

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)
