# configuration
# -------------

"""
JET configurations for error printing.
If the entry renders the collected error points, the configurations below will be active.

---
- `print_toplevel_success::Bool = false` \\
  If `true`, prints a message when there is no toplevel errors found.
---
- `print_inference_success::Bool = true` \\
  If `true`, print a message when there is no errors found in abstract interpretation based analysis pass.
---
- `annotate_types::Bool = false` \\
  When set to `true`, annotates types when printing analyzed call stack.
  Here are examples:
  * with `annotate_types = false` (default):
    ```julia
    julia> @report_call sum("julia")
    ═════ 2 possible errors found ═════
    ┌ @ reduce.jl:530 Base.#sum#241(Base.pairs(Core.NamedTuple()), #self#, a)
    │┌ @ reduce.jl:530 Base.sum(Base.identity, a)
    ││┌ @ reduce.jl:503 Base.#sum#240(Base.pairs(Core.NamedTuple()), #self#, f, a)
    │││┌ @ reduce.jl:503 Base.mapreduce(f, Base.add_sum, a)
    ││││┌ @ reduce.jl:289 Base.#mapreduce#237(Base.pairs(Core.NamedTuple()), #self#, f, op, itr)
    │││││┌ @ reduce.jl:289 Base.mapfoldl(f, op, itr)
    ││││││┌ @ reduce.jl:162 Base.#mapfoldl#233(Base._InitialValue(), #self#, f, op, itr)
    │││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f, op, init, itr)
    ││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
    │││││││││┌ @ reduce.jl:48 Base._foldl_impl(op, nt, itr)
    ││││││││││┌ @ reduce.jl:62 op(v, Base.getindex(y, 1))
    │││││││││││┌ @ reduce.jl:81 Base.getproperty(op, :rf)(acc, x)
    ││││││││││││┌ @ reduce.jl:24 Base.+(x, y)
    │││││││││││││ no matching method found for call signature: Base.+(x::Char, y::Char)
    ││││││││││││└────────────────
    │││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op, itr)
    ││││││││││┌ @ reduce.jl:356 Base.reduce_empty_iter(op, itr, Base.IteratorEltype(itr))
    │││││││││││┌ @ reduce.jl:357 Base.reduce_empty(op, Base.eltype(itr))
    ││││││││││││┌ @ reduce.jl:330 Base.reduce_empty(Base.getproperty(op, :rf), _)
    │││││││││││││┌ @ reduce.jl:322 Base.reduce_empty(Base.+, _)
    ││││││││││││││┌ @ reduce.jl:313 Base.zero(_)
    │││││││││││││││ no matching method found for call signature: Base.zero(_::Type{Char})
    ││││││││││││││└─────────────────
    Char
    ```
  * with `annotate_types = true`
    ```julia
    julia> @report_call annotate_types = true sum("julia")
    ═════ 2 possible errors found ═════
    ┌ @ reduce.jl:530 Base.#sum#241(Base.pairs(Core.NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(sum), a::String)
    │┌ @ reduce.jl:530 Base.sum(Base.identity, a::String)
    ││┌ @ reduce.jl:503 Base.#sum#240(Base.pairs(Core.NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(sum), f::typeof(identity), a::String)
    │││┌ @ reduce.jl:503 Base.mapreduce(f::typeof(identity), Base.add_sum, a::String)
    ││││┌ @ reduce.jl:289 Base.#mapreduce#237(Base.pairs(Core.NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(mapreduce), f::typeof(identity), op::typeof(Base.add_sum), itr::String)
    │││││┌ @ reduce.jl:289 Base.mapfoldl(f::typeof(identity), op::typeof(Base.add_sum), itr::String)
    ││││││┌ @ reduce.jl:162 Base.#mapfoldl#233(Base._InitialValue()::Base._InitialValue, #self#::typeof(mapfoldl), f::typeof(identity), op::typeof(Base.add_sum), itr::String)
    │││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f::typeof(identity), op::typeof(Base.add_sum), init::Base._InitialValue, itr::String)
    ││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′::Base.BottomRF{typeof(Base.add_sum)}, nt::Base._InitialValue, itr′::String)
    │││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String)
    ││││││││││┌ @ reduce.jl:356 Base.reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String, Base.IteratorEltype(itr::String)::Base.HasEltype)
    │││││││││││┌ @ reduce.jl:357 Base.reduce_empty(op::Base.BottomRF{typeof(Base.add_sum)}, Base.eltype(itr::String)::Type{Char})
    ││││││││││││┌ @ reduce.jl:330 Base.reduce_empty(Base.getproperty(op::Base.BottomRF{typeof(Base.add_sum)}, :rf::Symbol)::typeof(Base.add_sum), _::Type{Char})
    │││││││││││││┌ @ reduce.jl:322 Base.reduce_empty(Base.+, _::Type{Char})
    ││││││││││││││┌ @ reduce.jl:313 Base.zero(_::Type{Char})
    │││││││││││││││ no matching method found for call signature: Base.zero(_::Type{Char})
    ││││││││││││││└─────────────────
    │││││││││┌ @ reduce.jl:48 Base._foldl_impl(op::Base.BottomRF{typeof(Base.add_sum)}, nt::Base._InitialValue, itr::String)
    ││││││││││┌ @ reduce.jl:62 op::Base.BottomRF{typeof(Base.add_sum)}(v::Char, Base.getindex(y::Tuple{Char, Int64}, 1)::Char)
    │││││││││││┌ @ reduce.jl:81 Base.getproperty(op::Base.BottomRF{typeof(Base.add_sum)}, :rf::Symbol)::typeof(Base.add_sum)(acc::Char, x::Char)
    ││││││││││││┌ @ reduce.jl:24 Base.+(x::Char, y::Char)
    │││││││││││││ no matching method found for call signature: Base.+(x::Char, y::Char)
    ││││││││││││└────────────────
    Char
    ```

  !!! note
      JET always annotates types when printing the error point, e.g. in the example above,
      the error points below are always type-annotated regardless of this configuration:
      - `no matching method found for call signature: Base.zero(_::Type{Char})`
      - `no matching method found for call signature: Base.+(x::Char, y::Char)`
---
- `fullpath::Bool = false` \\
  Controls whether or not expand a file path to full path when printing analyzed call stack.
  Note that paths of Julia's `Base` files will also be expanded when set to `true`.
"""
struct PrintConfig
    print_toplevel_success::Bool
    print_inference_success::Bool
    annotate_types::Bool
    fullpath::Bool
    @jetconfigurable PrintConfig(; print_toplevel_success::Bool = false,
                                   print_inference_success::Bool = true,
                                   annotate_types::Bool         = false,
                                   fullpath::Bool               = false,
                                   ) =
        return new(print_toplevel_success,
                   print_inference_success,
                   annotate_types,
                   fullpath,
                   )
end

# utility
# -------

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = (
    # preserve yellow for future performance linting
    45, # light blue
    123, # light cyan
    150, # ???
    215, # orange
    231, # white
)
const N_RAILS = length(RAIL_COLORS)
const LEFT_ROOF  = "═════ "
const RIGHT_ROOF = " ═════"
const HEADER_COLOR = :reverse
const ERROR_SIG_COLOR = :bold
const TYPE_ANNOTATION_COLOR = :light_cyan
const HINT_COLOR = :light_green

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    string(n, ' ', isone(n) ? one : more)

printlnstyled(args...; kwarg...) = printstyled(args..., '\n'; kwarg...)

function print_rails(io, depth)
    for i = 1:depth
        color = RAIL_COLORS[i%N_RAILS+1]
        printstyled(io, '│'; color)
    end
end

function with_bufferring(f, args...)
    buf = IOBuffer()
    io = IOContext(buf, args...)
    f(io)
    return String(take!(buf))
end

# we may need something like this for stdlibs as well ?

function tofullpath(filename::AbstractString)
    path = abspath(filename)
    return isfile(path) ? path : fullbasepath(filename)
end
function fullbasepath(filename)
    return @static occursin("DEV", string(VERSION)) ? # TODO make this configurable
           normpath(Sys.BINDIR::String, "..", "..", "..", "julia", "base", filename) :
           normpath(Sys.BINDIR::String, Base.DATAROOTDIR, "julia", "base", filename)
end

# toplevel
# --------

function print_reports(io::IO,
                       reports::AbstractVector{<:ToplevelErrorReport},
                       @nospecialize(postprocess = identity);
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    if isempty(reports)
        if config.print_toplevel_success
            printlnstyled(io, "No toplevel errors !"; color = NOERROR_COLOR)
        end
        return false
    end

    arg = :color => get(io, :color, false)
    with_bufferring(arg) do io
        s = string(pluralize(length(reports), "toplevel error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        color = ERROR_COLOR

        rail = with_bufferring(arg) do io
            printstyled(io, "│ "; color)
        end

        for report in reports
            s = string("┌ @ ", (config.fullpath ? tofullpath : identity)(string(report.file)), ':', report.line, ' ')
            printlnstyled(io, s; color)

            errlines = with_bufferring(arg) do io
                print_report(io, report)
            end |> strip
            println(io, join(string.(rail, split(errlines, '\n')), '\n'))

            s = string('└', '─'^(length(s)-1))
            printlnstyled(io, s; color)
        end
    end |> postprocess |> Fix1(print, io)

    return true
end

# don't show stacktrace for syntax errors
print_report(io, report::SyntaxErrorReport) = showerror(io, report.err)
function print_report(io, report::RecursiveIncludeErrorReport)
    printstyled(io, "ERROR: "; bold = true, color = ERROR_COLOR)
    println(io, "recursive `include` call detected:")
    println(io, " ⚈ duplicated file: ", report.duplicated_file)
    println(io, " ⚈  included files: ", join(report.files, ' '))
end
# TODO: add context information, i.e. during macroexpansion, defining something
print_report(io, report::ActualErrorWrapped) = showerror(io, report.err, report.st)
function print_report(io, report::MissingConcretization)
    printstyled(io, "HINT: "; bold = true, color = HINT_COLOR)
    printlnstyled(io, """
    the following error happened mostly because of the missing concretization of global variables,
    and this could be fixed with the `concretization_patterns` configuration.
    Check https://aviatesk.github.io/JET.jl/dev/config/#JET.ToplevelConfig for the details.
    ---"""; color = HINT_COLOR)

    showerror(io, report.err, report.st)
end

# inference
# ---------

function print_reports(io::IO,
                       reports::AbstractVector{<:InferenceErrorReport},
                       @nospecialize(postprocess = identity);
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    # XXX the same hack is already imposed in `_typeinf`, so we may not need this
    reports = unique(get_identity_key, reports)

    if isempty(reports)
        if config.print_inference_success
            printlnstyled(io, "No errors !"; color = NOERROR_COLOR)
        end
        return false
    end

    with_bufferring(:color => get(io, :color, false)) do io
        s = string(pluralize(length(reports), "possible error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        # don't duplicated virtual stack frames for reports from the same toplevel frame
        toplevel_linfo_hash = hash(:dummy)
        wrote_linfos = Set{UInt64}()
        for report in reports
            new_toplevel_linfo_hash = hash(first(report.st))
            if toplevel_linfo_hash != new_toplevel_linfo_hash
                toplevel_linfo_hash = new_toplevel_linfo_hash
                wrote_linfos = Set{UInt64}()
            end
            print_report(io, report, config, wrote_linfos)
        end
    end |> postprocess |> Fix1(print, io)

    return true
end

# traverse abstract call stack, print frames
function print_report(io, report::InferenceErrorReport, config, wrote_linfos, depth = 1)
    if length(report.st) == depth # error here
        return print_error_frame(io, report, config, depth)
    end

    frame = report.st[depth]

    # cache current frame info
    linfo_hash = hash(frame)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    # print current frame and go into deeper
    should_print && print_frame(io, frame, config, depth, false)
    print_report(io, report, config, wrote_linfos, depth + 1)
end

function print_error_frame(io, report, config, depth)
    frame = report.st[depth]

    len = print_frame(io, frame, config, depth, true)
    print_rails(io, depth-1)
    print_error_report(io, report)

    print_rails(io, depth-1)
    printlnstyled(io, '└', '─'^len; color = ERROR_COLOR)
end

function print_frame(io, frame, config, depth, is_err)
    print_rails(io, depth-1)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth)%N_RAILS+1]
    s = string("┌ @ ", (config.fullpath ? tofullpath : identity)(string(frame.file)), ':', frame.line)
    printstyled(io, s, ' '; color)
    print_signature(io, frame.sig, config)

    return length(s) # the length of frame info string
end

function print_signature(io, sig, config; kwargs...)
    for a in sig
        _print_signature(io, a, config; kwargs...)
    end
    println(io)
end
_print_signature(io, a::Union{AbstractChar,AbstractString}, config; kwargs...) =
    printstyled(io, a; kwargs...)
function _print_signature(io, @nospecialize(typ), config; kwargs...)
    config.annotate_types || return
    printstyled(io, "::", string(typ); color = TYPE_ANNOTATION_COLOR, kwargs...)
end

# default error report printer
function print_error_report(io, report::InferenceErrorReport)
    printstyled(io, "│ ", report.msg, ": "; color = ERROR_COLOR)
    print_signature(io, report.sig,
                    (; annotate_types = true); # always annotate types for errored signatures
                    bold = true,
                    )
end
# those don't need explicit signatures
print_error_report(io, report::NoFieldErrorReport)       = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::LocalUndefVarErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::DivideErrorReport)        = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::UndefKeywordErrorReport)  = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
