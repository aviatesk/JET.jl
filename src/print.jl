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

@jetconfigurable function print_reports(io::IO,
                                        reports::AbstractVector{<:ToplevelErrorReport},
                                        @nospecialize(postprocess = identity);
                                        print_toplevel_sucess::Bool = false,
                                        fullpath::Bool = false,
                                        )
    if isempty(reports)
        if print_toplevel_sucess
            printlnstyled(io, "No toplevel errors !"; color = NOERROR_COLOR)
        end
        return false
    end

    with_bufferring(:color => get(io, :color, false)) do io
        s = string(pluralize(length(reports), "toplevel error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        color = ERROR_COLOR

        rail = with_bufferring(arg) do io
            printstyled(io, "│ "; color)
        end

        for report in reports
            s = string("┌ @ ", (fullpath ? tofullpath : identity)(string(report.file)), ':', report.line, ' ')
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

# inference
# ---------

@jetconfigurable function print_reports(io::IO,
                                        reports::AbstractVector{<:InferenceErrorReport},
                                        @nospecialize(postprocess = identity);
                                        print_inference_sucess::Bool = true,
                                        fullpath::Bool = false,
                                        annotate_types::Bool = false,
                                        )
    # XXX the same hack is already imposed in `_typeinf`, so we may not need this
    reports = unique(get_identity_key, reports)

    if isempty(reports)
        if print_inference_sucess
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
            print_report(io, report, wrote_linfos; fullpath, annotate_types)
        end
    end |> postprocess |> Fix1(print, io)

    return true
end

# traverse abstract call stack, print frames
function print_report(io, report::InferenceErrorReport, wrote_linfos, depth = 1; kwargs...)
    if length(report.st) == depth # error here
        return print_error_frame(io, report, depth; kwargs...)
    end

    frame = report.st[depth]

    # cache current frame info
    linfo_hash = hash(frame)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    # print current frame and go into deeper
    should_print && print_frame(io, frame, depth, false; kwargs...)
    print_report(io, report, wrote_linfos, depth + 1; kwargs...)
end

function print_error_frame(io, report, depth; kwargs...)
    frame = report.st[depth]

    len = print_frame(io, frame, depth, true; kwargs...)
    print_rails(io, depth-1)
    print_error_report(io, report)

    print_rails(io, depth-1)
    printlnstyled(io, '└', '─'^len; color = ERROR_COLOR)
end

function print_frame(io, frame, depth, is_err;
                     fullpath = false,
                     kwargs...)
    print_rails(io, depth-1)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth)%N_RAILS+1]
    s = string("┌ @ ", (fullpath ? tofullpath : identity)(string(frame.file)), ':', frame.line)
    printstyled(io, s, ' '; color)
    print_signature(io, frame.sig; kwargs...)

    return length(s) # the length of frame info string
end

function print_signature(io, sig; kwargs...)
    for a in sig
        _print_signature(io, a; kwargs...)
    end
    println(io)
end
_print_signature(io, a::Union{AbstractChar,AbstractString};
                 annotate_types = false,
                 kwargs...) =
    printstyled(io, a)
function _print_signature(io, @nospecialize(typ); annotate_types = false, kwargs...)
    annotate_types || return

    printstyled(io, "::", string(typ); color = TYPE_ANNOTATION_COLOR)
end

# default error report printer
function print_error_report(io, report::InferenceErrorReport)
    printstyled(io, "│ ", report.msg, ": "; color = ERROR_COLOR)
    print_signature(io, report.sig;
                    annotate_types = true, # always annotate types for errored signatures
                    bold = true,
                    )
end
# those don't need explicit signatures
print_error_report(io, report::NoFieldErrorReport)       = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::LocalUndefVarErrorReport) = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::DivideErrorReport)        = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
print_error_report(io, report::UndefKeywordErrorReport)  = printlnstyled(io, "│ ", report.msg; color = ERROR_COLOR)
