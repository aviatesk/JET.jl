# in this overload we will work on some meta/debug information management
function CC.typeinf(interp::JETInterpreter, frame::InferenceState)
    # # print debug info before typeinf
    # depth = interp.depth[]
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

    interp.depth[] += 1 # for debug

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    push!(ANALYZED_LINFOS, frame.linfo) # analyzed !

    interp.depth[] -= 1 # for debug

    # # print debug info after typeinf
    # print_rails(io, depth)
    # printstyled(io, "└─→ "; color)
    # result = get_result(frame)
    # isa(result, InferenceState) || printstyled(io, get_result(frame); color = TYPE_ANNOTATION_COLOR)
    # printlnstyled(io, " ($(length(interp.reports)) reports)"; color = ERROR_COLOR)

    return ret
end

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(interp::JETInterpreter, frame::InferenceState)
    linfo = frame.linfo

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previously-inferred lousy result; JET.jl also needs that to reduce false positive reports,
    # and so here we will throw-away previously-collected error reports that are "lineage" of
    # this frame, when it is being re-inferred with constant-prop'ed inputs
    #
    # NOTE:
    # - constant prop' only happens after inference with non-constant abstract values (i.e. types)
    # - xref to track the change in the native constant propagation logic:
    #   https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    # IDEA:
    # we may want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    # even when constant prop' reveals it never happens given the current constant arguments
    if is_constant_propagated(frame)
        # use `frame.linfo` instead of `frame` for lineage check since the program counter
        # for this frame is not initialized yet; note that `frame.linfo` is the exactly same
        # object as that of the previous only-type inference
        filter!(r->!is_lineage(r.lineage, frame.parent, linfo), interp.reports)
    end

    before = Set(interp.reports)

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
        throw_calls = Any[]
        for r in reports_for_this_linfo
            if isa(r, ExceptionReport)
                push!(throw_locs, r.lin)
            end
        end
        for (i, stmt) in enumerate(stmts)
            is_throw_call′(stmt) || continue
            # if this `throw` is already reported, don't duplciate
            linetable[codelocs[i]] in throw_locs && continue
            push!(throw_calls, stmt)
        end
        isempty(throw_calls) || push!(interp.uncaught_exceptions, UncaughtExceptionReport(interp, frame, throw_calls))
    else
        # the non-`Bottom` result here means `throw` calls within the children frames
        # reported so far (if exist) are caught and not propagated to the result;
        # we don't want to cache for this frame and its parents, so just filter them away
        # NOTE: this is critical for performance issue https://github.com/aviatesk/JET.jl/issues/71
        isempty(interp.uncaught_exceptions) || empty!(interp.uncaught_exceptions)
    end

    isempty(interp.uncaught_exceptions) || push!(reports_for_this_linfo, interp.uncaught_exceptions...)

    if !isempty(reports_for_this_linfo)
        if is_constant_propagated(frame)
            argtypes = frame.result.argtypes
            cache = interp.cache

            # if haskey(cache, argtypes)
            #     @warn "this control flow shouldn't happen ..." linfo argtypes
            # end
            local_cache = cache[argtypes] = InferenceErrorReportCache[]

            for report in reports_for_this_linfo
                cache_report!(report, linfo, local_cache)
            end
        else
            # if haskey(JET_GLOBAL_CACHE, linfo)
            #     # can happen when the inference for this frame was manually invoked,
            #     # e.g. calling `@report_call sum("julia")`  two times from REPL
            #     # but we will just update the cache anyway
            #     @warn "deplicated analysis happened" linfo
            # end
            global_cache = JET_GLOBAL_CACHE[linfo] = InferenceErrorReportCache[]

            for report in reports_for_this_linfo
                cache_report!(report, linfo, global_cache)
            end
        end
    end

    return ret
end

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

"""
    function overload_typeinf_edge!()
        ...
    end
    push_inithook!(overload_typeinf_edge!)

the aims of this overload are:
1. invalidate code cache for a `MethodInstance` that is not yet analyzed by JET;
   it happens when the `MethodInstance` is cached within the system image itself
2. when cache is hit (i.e. any further inference won't occcur for the cached frame),
   append cached reports associated with the cached `MethodInstance`
"""
function overload_typeinf_edge!()

# %% for easier interactive update of typeinf_edge
Core.eval(CC, quote

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::$(JETInterpreter), method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    #=== typeinf_edge patch-point 1 start ===#
    code = $(∉)(mi, $(ANALYZED_LINFOS)) ? nothing : get(code_cache(interp), mi, nothing)
    #=== typeinf_edge patch-point 1 end ===#
    if code isa CodeInstance # return existing rettype if the code is already inferred
        #=== typeinf_edge patch-point 2 start ===#
        # cache hit, now we need to append cached reports associated with this `MethodInstance`
        global_cache = $(get)($(JET_GLOBAL_CACHE), mi, nothing)
        if isa(global_cache, $(Vector{InferenceErrorReportCache}))
            $(foreach)(global_cache) do cached
                $(restore_cached_report!)(cached, interp, caller)
            end
        end
        #=== typeinf_edge patch-point 2 end ===#
        update_valid_age!(caller, WorldRange(min_world(code), max_world(code)))
        $(@static if isdefined(CC, :InterConditional) quote
        rettype = code.rettype
        if isdefined(code, :rettype_const)
            rettype_const = code.rettype_const
            if isa(rettype_const, InterConditional)
                return rettype_const, mi
            elseif isa(rettype_const, Vector{Any}) && !(Vector{Any} <: rettype)
                return PartialStruct(rettype, rettype_const), mi
            else
                return Const(rettype_const), mi
            end
        else
            return rettype, mi
        end
        end else quote
        if isdefined(code, :rettype_const)
            if isa(code.rettype_const, Vector{Any}) && !(Vector{Any} <: code.rettype)
                return PartialStruct(code.rettype, code.rettype_const), mi
            else
                return Const(code.rettype_const), mi
            end
        else
            return code.rettype, mi
        end
        end end)
    end
    if ccall(:jl_get_module_infer, Cint, (Any,), method.module) == 0
        return Any, nothing
    end
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame === false
        # completely new
        lock_mi_inference(interp, mi)
        result = InferenceResult(mi)
        frame = InferenceState(result, #=cached=#true, interp) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            unlock_mi_inference(interp, mi)
            return Any, nothing
        end
        if caller.cached || caller.limited # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        typeinf(interp, frame)
        update_valid_age!(frame, caller)
        return frame.bestguess, frame.inferred ? mi : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(frame, caller)
    return frame.bestguess, nothing
end

end) # Core.eval(CC, quote
# %% for easier interactive update of typeinf_edge

end # function overload_typeinf_edge!()
push_inithook!(overload_typeinf_edge!)
