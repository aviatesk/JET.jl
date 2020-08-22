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

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    return string(n, ' ', isone(n) ? one : more)

printlnstyled(args...; kwarg...) = printstyled(args..., '\n'; kwarg...)

function print_rails(io, depth)
    for i = 1:depth
        color = RAIL_COLORS[i%N_RAILS+1]
        printstyled(io, '│'; color)
    end
    return
end

# FIXME: we may need something like this for stdlibs as well, or just always use `RELEASE_DIR`
const SRC_DIR = normpath(Sys.BINDIR, "..", "..", "base")
const RELEASE_DIR = normpath(Sys.BINDIR, "..", "share", "julia", "base")
expandbasepath(filename) = normpath((@static isdir(SRC_DIR) ? SRC_DIR : RELEASE_DIR), filename)
function tofullpath(filename::AbstractString)
    path = abspath(filename)
    return isfile(path) ? path : expandbasepath(filename)
end

# toplevel
# --------

function print_reports(io::IO,
                       filename::AbstractString,
                       reports::Vector{<:ToplevelErrorReport},
                       @nospecialize(postprocess) = identity;
                       print_toplevel_sucess::Bool = false,
                       color::Bool = get(io, :color, false),
                       fullpath::Bool = false,
                       __kwargs...)
    if isempty(reports)
        if print_toplevel_sucess
            printlnstyled(io, "No toplevel errors !"; color = NOERROR_COLOR)
        end
        return false
    end

    hascolor = color
    color = ERROR_COLOR

    buf = IOBuffer()
    ioctx = IOContext(buf, :color => hascolor)

    s = string(pluralize(length(reports), "toplevel error"), " found in ",
               (fullpath ? tofullpath : identity)(filename)
               )
    printlnstyled(ioctx, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

    tmpbuf = IOBuffer()
    tmpioctx = IOContext(tmpbuf, :color => hascolor)
    rail = (printstyled(tmpioctx, "│ "; color); String(take!(tmpbuf)))
    for report in reports
        s = string("┌ @ ", (fullpath ? tofullpath : identity)(string(report.file)), ":", report.line, ' ')
        printlnstyled(ioctx, s; color)

        print_report(tmpioctx, report)
        errlines = strip(String(take!(tmpbuf)))
        println(ioctx, join(string.(rail, split(errlines, '\n')), '\n'))

        s = string('└', '─'^(length(s)-1))
        printlnstyled(ioctx, s; color)
    end

    s = String(take!(buf))
    print(io, postprocess(s))

    return true
end

print_report(io, report::SyntaxErrorReport) = showerror′(io, report.err) # don't show stacktrace for syntax errors
# TODO: add context information, i.e. during macroexpansion, defining something
print_report(io, report::ActualErrorWrapped) = showerror′(io, report.err, report.st)

function showerror′(io, er, bt = nothing)
    showerror(IOContext(io, :limit => true), er, bt, backtrace = !isnothing(bt))
    println(io)
end

# inference
# ---------

function print_reports(io::IO,
                       filename::AbstractString,
                       reports::Vector{<:InferenceErrorReport},
                       @nospecialize(postprocess) = identity;
                       filter_native_remarks::Bool = true,
                       print_inference_sucess::Bool = true,
                       color::Bool = get(io, :color, false),
                       fullpath::Bool = false,
                       __kwargs...)
    if filter_native_remarks
        reports = filter(r->!isa(r, NativeRemark), reports)
    end

    uniquify_reports!(reports)

    if isempty(reports)
        if print_inference_sucess
            printlnstyled(io, "No errors !"; color = NOERROR_COLOR)
        end
        return false
    end

    hascolor = color
    buf = IOBuffer()
    ioctx = IOContext(buf, :color => hascolor)

    s = string(pluralize(length(reports), "possible error"), " found in ",
               (fullpath ? tofullpath : identity)(filename)
               )
    printlnstyled(ioctx, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

    wrote_linfos = Set{UInt64}()
    for report in reports
        print_report(ioctx, report, wrote_linfos; fullpath)
    end

    s = String(take!(buf))
    print(io, postprocess(s))

    return true
end

# FIXME:
# this is a dirty fix for duplicated reports; see comments above of the overwriting of
# `typeinf_local` in src/abstractinterpretation.jl
function uniquify_reports!(reports::Vector{<:InferenceErrorReport})
    return unique!(reports) do report
        return last(report.st), report.msg, report.sig # uniquify keys
    end
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

    return
end

function print_error_frame(io, report, depth; kwargs...)
    frame = report.st[depth]
    color = ERROR_COLOR

    print_frame(io, frame, depth, true; kwargs...)
    print_rails(io, depth-1)
    printstyled(io, "│ ", report.msg, ": "; color)
    printlnstyled(io, report.sig; color = :bold)
    print_rails(io, depth-1)
    printlnstyled(io, '└'; color)

    return
end

function print_frame(io, (file, line, sig), depth, is_err; fullpath = false)
    print_rails(io, depth-1)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth)%N_RAILS+1]
    printstyled(io, "┌ @ ", (fullpath ? tofullpath : identity)(string(file)), ":", line; color)
    println(io, ' ', sig)

    return
end
