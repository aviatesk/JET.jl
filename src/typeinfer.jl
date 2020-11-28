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
    # println(io)

    interp.depth[] += 1 # for debug

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    push!(ANALYZED_LINFOS, frame.linfo) # analyzed !

    interp.depth[] -= 1 # for debug

    # # print debug info after typeinf
    # print_rails(io, depth)
    # printstyled(io, "└─> "; color)
    # printstyled(io, get_result(frame); color = TYPE_ANNOTATION_COLOR)
    # printlnstyled(io, " ($(length(interp.reports)) reports)"; color = ERROR_COLOR)

    return ret
end

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(interp::JETInterpreter, frame::InferenceState)
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
    #
    # XXX: constant prop' may not happen always, especially when inferring on cached frames,
    #      and then error reports can be different for uncached/cached frames, which would be
    #      super confusing
    #
    # TODO: we may still want to keep reports on some kinds of "serious" errors, like
    #       `GlobalUndefVarErrorReport` even if it's been threw-away by constant prop'
    linfo = frame.linfo

    if is_constant_propagated(frame)
        filter!(r -> ∉(linfo, r.lineage), interp.reports)
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

                    add_remark!(interp, frame, LocalUndefVarErrorReport(interp, frame, sym))
                # else
                    # by excluding this pass, JET accepts some false negatives (i.e. don't report
                    # those that may actually happen on actual execution)
                end
            end
        end
    end

    # report `throw` calls "appropriately" by simple inter-frame analysis
    # the basic stance here is really conservative so we don't report them unless they
    # will be inevitably called and won't be caught by `try/catch` in frame at any level
    # NOTE:
    # this is better to happen here because constant propagation can reduce the chance of
    # false negative reports by excluding unreachable control flows
    if get_result(frame) === Bottom
        # report `throw`s only if there is no circumvent pass, which is represented by
        # `Bottom`-annotated return type inference with non-empty `throw` blocks
        throw_calls = filter(is_throw_call′, stmts)
        if !isempty(throw_calls)
            push!(interp.exception_reports, length(interp.reports) => ExceptionReport(interp, frame, throw_calls))
        end
    end

    # TODO: cache constant analysis into `interp.cache`
    # local cache for constant analysis is done by `_typeinf(interp::JETInterpreter, frame::InferenceState)`

    after = Set(interp.reports)
    reports_for_this_linfo = setdiff(after, before)
    if !isempty(reports_for_this_linfo)
        global_cache = if haskey(JET_GLOBAL_CACHE, linfo)
            @debug "deplicated analysis happened" linfo is_constant_propagated(frame)
            JET_GLOBAL_CACHE[linfo]
        else
            JET_GLOBAL_CACHE[linfo] = InferenceErrorReportCache[]
        end

        for report in reports_for_this_linfo
            cache_global_report!(report, linfo, global_cache)
        end
    end

    return ret
end

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

function cache_global_report!(report::T, linfo, global_cache) where {T<:InferenceErrorReport}
    st = reverse(report.st)
    st = view(st, 1:findfirst(sf->sf.linfo==linfo, st)::Int)
    new = InferenceErrorReportCache(T, st, report.msg, report.sig, spec_args(report))
    push!(global_cache, new)
end

"""
    function overload_typeinf_edge!()
        ...
    end
    push_inithook!(overload_typeinf_edge!)

the aims of this overload are:
1. invalidate code cache for a `MethodInstance` that is not yet analyzed by JET; this happens
   e.g. the `MethodInstance` is cached within the system image itself
2. when cache is hit (i.e. any further inference won't occcur for the cached frame),
   append cached reports associated with the cached `MethodInstance`
"""
function overload_typeinf_edge!()

# %% for easier interactive update of typeinf_edge
Core.eval(CC, quote

# compute (and cache) an inferred AST and return the current best estimate of the result type
function typeinf_edge(interp::$(JETInterpreter), method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    #=== typeinf_edge monkey-patch 1 start ===#
    #= keep original code
    code = get(code_cache(interp), mi, nothing)
    =#
    code = $(∉)(mi, $(ANALYZED_LINFOS)) ? nothing : get(code_cache(interp), mi, nothing)
    #=== typeinf_edge monkey-patch 1 end ===#
    if code isa CodeInstance # return existing rettype if the code is already inferred
        #=== typeinf_edge monkey-patch 2 start ===#
        # cache hit, now we need to append cached reports associated with this `MethodInstance`
        global_cache = $(get)($(JET_GLOBAL_CACHE), mi, nothing)
        if global_cache !== nothing
            # caches = $(last)(global_cache)::
            $(foreach)(global_cache::$(Vector{InferenceErrorReportCache})) do cached
                $(restore_cached_report!)(cached, interp, caller)
            end
        end
        #=== typeinf_edge monkey-patch 2 end ===#
        update_valid_age!(caller, WorldRange(min_world(code), max_world(code)))
        if isdefined(code, :rettype_const)
            if isa(code.rettype_const, Vector{Any}) && !(Vector{Any} <: code.rettype)
                return PartialStruct(code.rettype, code.rettype_const), mi
            else
                return Const(code.rettype_const), mi
            end
        else
            return code.rettype, mi
        end
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
        return widenconst_bestguess(frame.bestguess), frame.inferred ? mi : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
    end
    # return the current knowledge about this cycle
    frame = frame::InferenceState
    update_valid_age!(frame, caller)
    return widenconst_bestguess(frame.bestguess), nothing
end

end) # Core.eval(CC, quote
# %% for easier interactive update of typeinf_edge

end # function overload_typeinf_edge!()
push_inithook!(overload_typeinf_edge!)

const ANALYZED_LINFOS  = IdSet{MethodInstance}() # keeps `MethodInstance`s analyzed by JET
