# TODO:
# - absolute pass mode
# - disable color output (setup keyword argument like `color = get(io, :color, false)`) ?

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

const SRC_DIR = normpath(Sys.BINDIR, "..", "..", "base")
const RELEASE_DIR = normpath(Sys.BINDIR, "..", "share", "julia", "base")
basepath(filename) = normpath((@static isdir(SRC_DIR) ? SRC_DIR : RELEASE_DIR), filename)
function fullpath(filename)
    path = isabspath(filename) ? filename : basepath(filename)
    return try
        realpath(path)
    catch
        path
    end
end

# toplevel
# --------

function print_reports(io,
                       reports::Vector{<:ToplevelErrorReport},
                       postprocess = nothing;
                       print_toplevel_sucess = false,
                       __kwargs...)
    isempty(reports) && return if print_toplevel_sucess
        printstyled(io, "No toplevel errors !\n"; color = NOERROR_COLOR)
    end

    if !isnothing(postprocess)
        buffer = IOBuffer()
        target = io
        io = IOContext(buffer, :color => true)
    end

    color = ERROR_COLOR
    colsize = last(displaysize(io)) ÷ 2

    s = string(pluralize(length(reports), "toplevel error"), " found", '\n')
    printlnstyled(io, s; color)

    foreach(reports) do report
        s = string("┌ @ ", report.file, ":", report.line, ' ')
        printlnstyled(io, s; color)
        print_report(io, report)
        s = rpad('└', length(s), '—')
        printlnstyled(io, s; color)
    end

    if !isnothing(postprocess)
        s = String(take!(buffer))
        print(target, postprocess(s))
    end

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
                       postprocess = nothing; # TODO
                       filter_native_remarks = true,
                       print_inference_sucess = true,
                       __kwargs...)
    if filter_native_remarks
        reports = filter(r->!isa(r, NativeRemark), reports)
    end

    isempty(reports) && return if print_inference_sucess
        printstyled(io, "No errors !\n"; color = NOERROR_COLOR)
    end

    if !isnothing(postprocess)
        buffer = IOBuffer()
        target = io
        io = IOContext(buffer, :color => true)
    end

    s = string(pluralize(length(reports), "error"), " found", '\n')
    printlnstyled(io, s; color = ERROR_COLOR)
    wrote_linfos = Set{UInt64}()
    foreach(reports) do report
        print_report(io, report, wrote_linfos)
    end

    if !isnothing(postprocess)
        s = String(take!(buffer))
        print(target, postprocess(s))
    end

    return
end

function print_report(io, report::InferenceErrorReport, wrote_linfos)
    n = length(report.st) - 1
    color = ERROR_COLOR

    print_calltrace(io, report.st, wrote_linfos)
    print_rails(io, n)
    printstyled(io, "│ ", report.msg, ": "; color)
    println(io, report.sig)
    print_rails(io, n)
    printlnstyled(io, '└'; color)

    return
end

# traverse abstract call stack, print frames

function print_calltrace(io, st, wrote_linfos, depth = 0)
    i = depth + 1
    frame = st[i]

    linfo_hash = hash(frame)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    if length(st) == i # error here
        print_frame(io, frame, depth, true)
        return
    end

    # print current frame adn go into deeper
    should_print && print_frame(io, frame, depth, false)
    print_calltrace(io, st, wrote_linfos, depth + 1)
    return
end

function print_frame(io, (file, line, sig), depth, is_err)
    # rail
    print_rails(io, depth)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth+1)%length(RAIL_COLORS)+1]
    printstyled(io, "┌ @ ", file, ":", line; color)
    println(io, ' ', sig)

    return
end
