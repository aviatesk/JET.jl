# in this overload we will work on some meta/debug information management
function CC.typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    linfo = frame.linfo

    #= logging start =#
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
                printstyled(io, " (constant prop': ", frame.result.argtypes, ')'; color = NOERROR_COLOR)
            end
            file, line = get_file_line(linfo)
            print(io, ' ', file, ':', line)
            println(io)
            set_depth!(analyzer, get_depth(analyzer) + 1) # manipulate this only in debug mode
        end
    end
    #= logging end =#

    prev_frame = get_current_frame(analyzer)
    set_current_frame!(analyzer, frame)

    ret = @invoke typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    empty!(get_throw_locs(analyzer))
    set_current_frame!(analyzer, prev_frame)

    #= logging start =#
    if logger_activated
        sec = round(time() - sec; digits = 3)
        with_inference_logger(analyzer, ==(INFO_LOGGER_LEVEL)) do io
            println(io, "inference on $linfo finished in $sec sec")
        end
        with_inference_logger(analyzer, ==(DEBUG_LOGGER_LEVEL)) do io
            print_rails(io, depth)
            printstyled(io, "└─→ "; color = RAIL_COLORS[(depth+1)%N_RAILS+1])
            result = get_result(frame)
            isa(result, InferenceState) || printstyled(io, result; color = TYPE_ANNOTATION_COLOR)
            println(io, " (",
                        join(filter(!isnothing, (
                             linfo,
                             ret ? nothing : "in cycle",
                             "$(length(get_reports(analyzer))) reports",
                             "$sec sec"
                             )), ", "),
                        ')')
            set_depth!(analyzer, get_depth(analyzer) - 1) # manipulate this only in debug mode
        end
    end
    #= logging end =#

    return ret
end

# TODO: disable optimization for better performance, only do necessary analysis work by ourselves

# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC._typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)
    linfo = frame.linfo
    parent = frame.parent
    isentry = parent === nothing
    iscp = is_constant_propagated(frame)

    # some methods like `getproperty` can't propagate accurate types without actual values,
    # and constant prop' plays a somewhat critical role in those cases by overwriteing the
    # previous non-constant inference result (under the current design constant prop' always
    # happens after inference with non-constant abstract elements)
    # JET also needs that in order to reduce false positive reports, and here we will
    # throw-away previously-collected error reports that are "lineage" of this frame,
    # when it is being re-inferred with constants
    # NOTE `frame.linfo` is the exactly same object as that of the previous non-constant inference
    # IDEA we may still want to keep some "serious" error reports like `GlobalUndefVarErrorReport`
    # even when constant prop' reveals it never happens given the current constant arguments
    iscp && !isentry && filter!(!is_from_same_frame(parent.linfo, linfo), get_reports(analyzer))

    reports_before             = Set(get_reports(analyzer))
    uncaught_exceptions_before = Set(get_uncaught_exceptions(analyzer))

    ret = @invoke _typeinf(analyzer::AbstractInterpreter, frame::InferenceState)

    # report pass for (local) undef var error
    ReportPass(analyzer)(LocalUndefVarErrorReport, analyzer, frame, frame.src.code)

    # XXX this is a dirty fix for performance problem, we need more "proper" fix
    # https://github.com/aviatesk/JET.jl/issues/75
    unique!(report_identity_key, get_reports(analyzer))

    # report pass for uncaught `throw` calls
    ReportPass(analyzer)(UncaughtExceptionReport, analyzer, frame, frame.src.code)

    reports_after = Set(get_reports(analyzer))
    uncaught_exceptions_after = Set(get_uncaught_exceptions(analyzer))

    # compute JET analysis results that should be cached for this linfo
    this_caches = union!(setdiff!(reports_after, reports_before),
                         setdiff!(uncaught_exceptions_after, uncaught_exceptions_before))

    if !isempty(this_caches)
        if iscp
            result = frame.result
            argtypes = result.argtypes
            cache = get_cache(analyzer)
            @static JET_DEV_MODE && @assert jet_cache_lookup(linfo, argtypes, cache) === nothing "invalid local caching $linfo, $argtypes"
            local_cache = InferenceErrorReportCache[]
            for report in this_caches
                cache_report!(local_cache, report)
                # TODO make this holds when the `analyzer` hooks into `finish` or `optimize`
                # more generally, handle cycles correctly
                @static JET_DEV_MODE && if isa(analyzer, JETAnalyzer)
                    actual, expected = first(report.vst).linfo, linfo
                    @assert actual === expected "invalid local caching detected, expected $expected but got $actual"
                end
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
            cache = jet_report_cache(analyzer)
            @static JET_DEV_MODE && @assert !haskey(cache, linfo) || isentry "invalid global caching $linfo"
            global_cache = InferenceErrorReportCache[]
            for report in this_caches
                cache_report!(global_cache, report)
                # TODO make this holds when the `analyzer` hooks into `finish` or `optimize`
                # more generally, handle cycles correctly
                @static JET_DEV_MODE && if isa(analyzer, JETAnalyzer)
                    actual, expected = first(report.vst).linfo, linfo
                    @assert actual === expected "invalid global caching detected, expected $expected but got $actual"
                end
            end
            cache[linfo] = global_cache
        end
    end

    set_to_be_updated!(analyzer, this_caches)

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
            istoplevel(analyzer, frame) && is_global_slot(analyzer, sym) && continue

            if unsound
                next_idx = idx + 1
                if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                    # the optimization so far has found this statement is never "reachable";
                    # JET reports it since it will invoke undef var error at runtime, or will just
                    # be dead code otherwise
                    add_new_report!(LocalUndefVarErrorReport(analyzer, (frame, idx), sym), analyzer)
                else
                    # by excluding this pass, this analysis accepts some false negatives and
                    # some undefined variable error may happen in actual execution (thus unsound)
                end
            else
                add_new_report!(LocalUndefVarErrorReport(analyzer, (frame, idx), sym), analyzer)
            end
        end
    end
end

@withmixedhash struct ReportIdentityKey
    T::Type{<:InferenceErrorReport}
    sig::Vector{Any}
    # entry_frame::VirtualFrame
    error_frame::VirtualFrame
end

report_identity_key(report::T) where {T<:InferenceErrorReport} =
    ReportIdentityKey(T, report.sig, #=first(report.vst),=# last(report.vst))

"""
    UncaughtExceptionReport <: InferenceErrorReport

Represents general `throw` calls traced during inference.
This is reported only when it's not caught by control flow.
"""
@reportdef struct UncaughtExceptionReport <: InferenceErrorReport
    throw_calls::Vector{Tuple{Int,Expr}} # (pc, call)
end
function UncaughtExceptionReport(analyzer::AbstractAnalyzer, sv::InferenceState, throw_calls::Vector{Tuple{Int,Expr}})
    vf = get_virtual_frame(analyzer, sv.linfo)
    msg = length(throw_calls) == 1 ? "may throw" : "may throw either of"
    sig = Any[]
    ncalls = length(throw_calls)
    for (i, (pc, call)) in enumerate(throw_calls)
        call_sig = _get_sig(analyzer, (sv, pc), call)
        append!(sig, call_sig)
        i ≠ ncalls && push!(sig, ", ")
    end
    return UncaughtExceptionReport([vf], msg, sig, throw_calls)
end

# this error report is really special, and might not be eligible for possible overloads since:
# - this report does not only report error points but also "clean up" caught error points
# - this pass is tightly bound to that of `SeriousExceptionReport`
function (::SoundBasicPass)(::Type{UncaughtExceptionReport}, analyzer::AbstractAnalyzer, frame::InferenceState, stmts::Vector{Any})
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
        throw_locs  = get_throw_locs(analyzer)
        throw_calls = Tuple{Int,Expr}[]
        for (pc, stmt) in enumerate(stmts)
            is_throw_call_expr(analyzer, frame, stmt) || continue
            # if this `throw` is already reported, don't duplciate
            linetable[codelocs[pc]]::LineInfoNode in throw_locs && continue
            push!(throw_calls, (pc, stmt))
        end
        if !isempty(throw_calls)
            add_new_report!(UncaughtExceptionReport(analyzer, frame, throw_calls), analyzer)
        end
    else
        # the non-`Bottom` result here may mean `throw` calls from the children frames
        # (if exists) are caught and not propagated here;
        # we don't want to cache `UncaughtExceptionReport`s for those calls for this frame
        # and its parents, so just filter them away
        empty!(get_uncaught_exceptions(analyzer))
    end
end

# basically same as `is_throw_call`, but also toplevel module handling added
function is_throw_call_expr(analyzer::AbstractAnalyzer, frame::InferenceState, @nospecialize(e))
    if isa(e, Expr)
        if e.head === :call
            f = e.args[1]
            if istoplevel(analyzer, frame) && isa(f, Symbol)
                f = GlobalRef(get_toplevelmod(analyzer), f)
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
