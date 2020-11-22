# in this overload we can work on `frame.src::CodeInfo` (and also `frame::InferenceState`)
# where type inference (and also optimization if applied) already ran on
function CC.typeinf(interp::JETInterpreter, frame::InferenceState)
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
    if is_constant_propagated(frame)
        linfo = frame.linfo
        filter!(r -> linfo ∉ r.lineage, interp.reports)
    end

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

    interp.depth[] -= 1 # for debug

    # # print debug info after typeinf
    # print_rails(io, depth)
    # printstyled(io, "└─> "; color)
    # printlnstyled(io, get_result(frame); color = TYPE_ANNOTATION_COLOR)

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

    return ret
end

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)
