# toplevel
# --------

# basic approach:
# - transforms toplevel expressions so that it can be wrapped into a single virtual function
#   (`parse_and_transform`)
# - and then run the inference on it and collect possible error points (`profile_call`)
#
# known limitations:
# - macro including access to global variables:
#   TP will try to expand any macro, but any macro expansion including access to global
#   variables will fail since TP just infers their types and avoids the actual evaluation

function profile_file(io::IO, filename::AbstractString, mod::Module = Main; kwargs...)
    text = read(filename, String)
    profile_text(io, text, filename, mod; kwargs...)
end
profile_file(args...; kwargs...) = profile_file(stdout, args...; kwargs...)

function profile_text(io::IO, text::AbstractString, filename::AbstractString, mod::Module = Main; kwargs...)
    virtualmod = generate_virtual_module(mod)
    reports = report_errors(virtualmod, text, filename)

    # fix virtual module printing based on string manipulation, because "actual" modules may
    # not be loaded into this process once TP comes to be able to profile modules other than
    # Main
    postprocess = let
        virtual = string(virtualmod)
        actual  = string(mod)
        s      -> replace(s, virtual => actual)
    end
    print_reports(io, reports, postprocess; kwargs...)
end
profile_text(args...; kwargs...) = profile_text(stdout, args...; kwargs...)

function report_errors(mod, text, filename)
    ret = parse_and_transform(mod, text, filename)
    isa(ret, Vector{<:ToplevelErrorReport}) && return ret

    λ = generate_virtual_lambda(mod, ret)
    # Core.eval(@__MODULE__, :(λ = $(λ)))
    interp, = profile_call(λ)
    return interp.reports
end

generate_virtual_module(actualmod::Module) =
    return Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))

function generate_virtual_lambda(mod::Module, toplevelex::Expr)
    @assert isexpr(toplevelex, :toplevel) "toplevel expression should be given"

    body = Expr(:block, toplevelex.args...)
    ex = Expr(:function, #=nullary lambda=# Expr(:tuple), body)
    return Core.eval(mod, ex)
end

# inference
# ---------

@nospecialize

function profile_call(f, args...)
    tt = to_tuple_type(typeof′.([f, args...]))
    return profile_call_gf(tt)
end

typeof′(x) = typeof(x)
typeof′(x::Type{T}) where {T} = Type{T}

@specialize

# FIXME:
# cached method specializations won't let abstract interpretation to happen again and
# so may suppress error reports
#
# mre would be
# ```julia
# julia> @profile_call sum("julia") # error reported
# julia> sum("julia") # cause actual error
# julia> @profile_call sum("julia") # error won't reported again
# ```
#
# maybe we can do either of
# - always invalidate cached method specializations
# - or attach error reports to each method specializations and use that when using cached method specialization cache
#
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
macro profile_call(ex, kwargs...)
    @assert Meta.isexpr(ex, :call) "function call expression should be given"
    f = ex.args[1]
    args = ex.args[2:end]

    quote let
        interp, frame = $(profile_call)($(esc(f)), $(map(esc, args)...))
        $(print_reports)(stdout, interp.reports; $(map(esc, kwargs)...))
        $(get_rettyp)(frame)
    end end
end

get_rettyp(frame::InferenceState) = frame.result.result # want to unwrap const ?
