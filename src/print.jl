# utility
# -------

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = [:bold, :light_cyan, :light_green, :light_yellow]

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    return string(n, ' ', isone(n) ? one : more)

printlnstyled(args...; kwarg...) = printstyled(args..., '\n'; kwarg...)

function print_rails(io, depth)
    n = length(RAIL_COLORS)
    for i = 1:depth
        color = RAIL_COLORS[i%n+1]
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

function print_reports(io,
                       reports::Vector{<:ToplevelErrorReport},
                       postprocess = identity;
                       print_toplevel_sucess = false,
                       color = get(io, :color, false),
                       fullpath = false,
                       __kwargs...)
    isempty(reports) && return if print_toplevel_sucess
        printstyled(io, "No toplevel errors !\n"; color = NOERROR_COLOR)
    end

    hascolor = color
    color = ERROR_COLOR

    buf = IOBuffer()
    ioctx = IOContext(buf, :color => hascolor)

    s = string(pluralize(length(reports), "toplevel error"), " found", '\n')
    printlnstyled(ioctx, s; color)

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

    return
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

function print_reports(io,
                       reports::Vector{<:InferenceErrorReport},
                       postprocess = identity;
                       filter_native_remarks = true,
                       print_inference_sucess = true,
                       color = get(io, :color, false),
                       fullpath = false,
                       __kwargs...)
    if filter_native_remarks
        reports = filter(r->!isa(r, NativeRemark), reports)
    end

    isempty(reports) && return if print_inference_sucess
        printstyled(io, "No errors !\n"; color = NOERROR_COLOR)
    end

    hascolor = color
    buf = IOBuffer()
    ioctx = IOContext(buf, :color => hascolor)

    s = string(pluralize(length(reports), "error"), " found", '\n')
    printlnstyled(ioctx, s; color = ERROR_COLOR)
    wrote_linfos = Set{UInt64}()
    for report in reports
        print_report(ioctx, report, wrote_linfos; fullpath)
    end

    s = String(take!(buf))
    print(io, postprocess(s))

    return
end

function print_report(io, report::InferenceErrorReport, wrote_linfos; kwargs...)
    n = length(report.st) - 1
    color = ERROR_COLOR

    print_calltrace(io, report.st, wrote_linfos; kwargs...)
    print_rails(io, n)
    printstyled(io, "│ ", report.msg, ": "; color)
    println(io, report.sig)
    print_rails(io, n)
    printlnstyled(io, '└'; color)

    return
end

# traverse abstract call stack, print frames

function print_calltrace(io, st, wrote_linfos, depth = 0; kwargs...)
    i = depth + 1
    frame = st[i]

    linfo_hash = hash(frame)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    if length(st) == i # error here
        return print_frame(io, frame, depth, true; kwargs...)
    end

    # print current frame adn go into deeper
    should_print && print_frame(io, frame, depth, false; kwargs...)
    print_calltrace(io, st, wrote_linfos, depth + 1; kwargs...)

    return
end

function print_frame(io, (file, line, sig), depth, is_err; fullpath = false)
    # rail
    print_rails(io, depth)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth+1)%length(RAIL_COLORS)+1]
    printstyled(io, "┌ @ ", (fullpath ? tofullpath : identity)(string(file)), ":", line; color)
    println(io, ' ', sig)

    return
end
