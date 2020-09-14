# entry
# -----

function profile_file(io::IO,
                      filename::AbstractString,
                      mod::Module = Main;
                      kwargs...)
    text = read(filename, String)
    return profile_text(io, text, filename, mod; kwargs...)
end
profile_file(args...; kwargs...) = profile_file(stdout, args...; kwargs...)

function profile_text(io::IO,
                      text::AbstractString,
                      filename::AbstractString = "top-level",
                      mod::Module = Main;
                      profiling_logger::Union{Nothing,IO} = nothing,
                      filter_native_remarks::Bool = true,
                      kwargs...)
    included_files, reports, postprocess = report_errors(profiling_logger,
                                                         mod,
                                                         text,
                                                         filename;
                                                         filter_native_remarks
                                                         )
    return included_files, print_reports(io, reports, postprocess; kwargs...)
end
profile_text(args...; kwargs...) = profile_text(stdout, args...; kwargs...)

report_errors(::Nothing, args...; kwargs...) = return report_errors(args...; kwargs...)
function report_errors(logger::IO, args...; kwargs...)
    print(logger, "profiling from ", #= filename =# last(args), " ...")
    s = time()

    ret = report_errors(args...; kwargs...)

    sec = round(time() - s; digits = 3)

    print(logger, '\b'^3)
    println(logger, "(finished in $(sec) sec)")

    return ret
end

function report_errors(actualmod, text, filename; filter_native_remarks = true)
    virtualmod = gen_virtual_module(actualmod)

    interp = TPInterpreter(; filter_native_remarks) # dummy
    actualmodsym = Symbol(actualmod)
    ret, interp = virtual_process!(text, filename, actualmodsym, virtualmod, interp)

    return ret.included_files,
           # non-empty `ret.toplevel_error_reports` means critical errors happened during
           # the AST transformation, so they always have precedence over `ret.inference_error_reports`
           !isempty(ret.toplevel_error_reports) ? ret.toplevel_error_reports : ret.inference_error_reports,
           gen_postprocess(virtualmod, actualmod)
end

# fix virtual module printing based on string manipulation; the "actual" modules may not be
# loaded into this process
function gen_postprocess(virtualmod, actualmod)
    virtual = string(virtualmod)
    actual  = string(actualmod)
    return actualmod == Main ?
        Fix2(replace, "Main." => "") ∘ Fix2(replace, virtual => actual) :
        Fix2(replace, virtual => actual)
end

# inference
# ---------

# TODO:
# - handle multiple applicable methods ?
# - `profile_call_builtin!` ?

profile_call_gf(@nospecialize(tt::Type{<:Tuple}), world::UInt = get_world_counter(); kwargs...) =
    return profile_call_gf!(TPInterpreter(world; kwargs...), tt)
function profile_call_gf!(interp::TPInterpreter,
                          @nospecialize(tt::Type{<:Tuple}),
                          world::UInt = get_world_counter(interp)
                          )
    ms = _methods_by_ftype(tt, -1, world)
    @assert !(ms === false || length(ms) != 1) "unable to find single applicable method for $(tt)"

    atypes, sparams, m = first(ms)

    mi = specialize_method(m, atypes, sparams)

    result = InferenceResult(mi)

    frame = InferenceState(result, #= cached =# true, interp)

    typeinf(interp, frame)

    # if return type is `Bottom`-annotated for this frame, this may mean some `throw`(s)
    # aren't caught by at any level and get propagated here, or there're other critical
    # inference error found
    if get_result(frame) === Bottom
        # let's report report `ExceptionReport`s only if there is no other error reported
        # TODO: change behaviour according to severity of collected report, e.g. don't take
        # into account `NativeRemark`s, etc
        isempty(interp.reports) && append!(interp.reports, last.(interp.exception_reports))

        # # just append collected `ExceptionReport`s
        # for (i, (idx, report)) in enumerate(interp.exception_reports)
        #     insert!(interp.reports, idx + i, report)
        # end
    end

    return interp, frame
end

# for testing, interactive session
# --------------------------------

@nospecialize

function profile_call(f, argtypes::Type...; kwargs...)
    tt = to_tuple_type([typeof′(f), argtypes...])
    return profile_call_gf(tt; kwargs...)
end

profile_call(f, argtypes; kwargs...) = profile_call(f, argtypes...; kwargs...)

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

macro profile_call(ex, kwargs...)
    @assert isexpr(ex, :call) "function call expression should be given"
    f = first(ex.args)
    args = ex.args[2:end]

    return quote let
        argtypes = $(typeof′).(($(map(esc, args)...),))
        interp, frame = $(profile_call)($(esc(f)), argtypes)
        $(print_reports)(stdout, interp.reports; $(map(esc, kwargs)...))
        $(get_result)(frame) # maybe want to widen const ?
    end end
end
