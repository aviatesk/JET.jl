# toplevel
# --------

# basic approach:
# - transforms toplevel expressions so that it can be wrapped into a single virtual function
#   (`parse_and_transform`)
# - and then run the inference on it and collect possible error points (`profile_call`)
#
# known limitations:
# - macro expansion and `include` calls including access to global variables:
#   TP will try to expand any macro, but any macro expansion including access to global
#   variables will fail since TP just infers their types and avoids the actual evaluation

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
    print(logger, "profiling from $(#=filename=# last(args)) ...")
    s = time()

    ret = report_errors(args...; kwargs...)

    sec = round(time() - s; digits = 3)

    print(logger, '\b'^3)
    println(logger, "(finished in $(sec) sec)")

    return ret
end

function report_errors(actualmod, text, filename; filter_native_remarks = true)
    virtualmod = generate_virtual_module(actualmod)

    ret = parse_and_transform(actualmod, virtualmod, text, filename)

    included_files = map(a->a.filename, ret)
    reportsary = map(a->a.reports, ret)
    # non-empty `reports` means critical errors happened during the AST transformation
    if any(!isempty, reportsary)
        reports = collect(flatten(filter!(!isempty, reportsary)))
        return included_files, reports, generate_postprocess(virtualmod, actualmod)
    end

    # run profiler on each included file, otherwise file/line number information gets messed
    # we can manually fix them, but we will need this kind of logic in the future module
    # handling as well, so let's take this way

    # filter out empty expression, i.e. there is nothing to profile
    toplevelexs = filter(!Fix2(isexpr, :empty), map(a->a.transformed, ret))

    # XXX:
    # we need to create TPInterpreter __after__ creating `λs`, otherwise world age error
    # not sure why `TPInterpreter(typemax(UInt); istoplevel = true)` doesn't help ...
    λs     = flatten(generate_virtual_lambdas.(Ref(virtualmod), toplevelexs))
    interp = TPInterpreter(;
                           istoplevel = true, # enable virtual global assignments
                           filter_native_remarks
                           )
    for λ in λs
        profile_call_gf!(interp, Tuple{typeof(λ)})
    end

    return included_files, interp.reports, generate_postprocess(virtualmod, actualmod)
end

generate_virtual_module(actualmod) =
    return Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))::Module

function generate_virtual_lambdas(mod, toplevelex)
    @assert isexpr(toplevelex, :toplevel) "toplevel expression should be given"

    lnns    = filter(islnn, toplevelex.args)
    bodyexs = filter(!islnn, toplevelex.args)

    @assert length(lnns) == length(bodyexs) "expressions and line number nodes unmatched"

    return map(zip(lnns, bodyexs)) do (lnn, bodyex)
        funcbody = Expr(:block, lnn, bodyex)
        funcex   = Expr(:function, #=nullary lambda=# Expr(:tuple), funcbody)
        Core.eval(mod, funcex)::Function
    end
end

const islnn = Fix2(isa, LineNumberNode)

# fix virtual module printing based on string manipulation; the "actual" modules may not be
# loaded into this process
function generate_postprocess(virtualmod, actualmod)
    virtual = string(virtualmod)
    actual  = string(actualmod)
    return Fix2(replace, virtual => actual)
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
    (ms === false || length(ms) != 1) && error("Unable to find single applicable method for $tt")

    atypes, sparams, m = ms[1]

    # grab the appropriate method instance for these types
    mi = specialize_method(m, atypes, sparams)

    # create an InferenceResult to hold the result
    result = InferenceResult(mi)

    # create an InferenceState to begin inference, give it a world that is always newest
    frame = InferenceState(result, #=cached=# true, interp)

    # run type inference on this frame
    typeinf(interp, frame)

    return interp, frame
end

# utility for interactive session
@nospecialize

function profile_call(f, args...)
    tt = to_tuple_type(typeof′.([f, args...]))
    return profile_call_gf(tt)
end

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

macro profile_call(ex, kwargs...)
    @assert Meta.isexpr(ex, :call) "function call expression should be given"
    f = ex.args[1]
    args = ex.args[2:end]

    quote let
        interp, frame = $(profile_call)($(esc(f)), $(map(esc, args)...))
        $(print_reports)(stdout, interp.reports; $(map(esc, kwargs)...))
        $(get_result)(frame) # maybe want to widen const ?
    end end
end
