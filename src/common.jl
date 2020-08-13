# interfaces
# ----------

abstract type ErrorReport end

function print_reports(io, reports; filter_native_remarks = true, kwargs...)
    if filter_native_remarks
        reports = filter(r->!isa(r, Profiler.NativeRemark), reports)
    end

    if isempty(reports)
        printstyled(io, "No errors !\n"; color = NOERROR_COLOR)
        return
    end

    s = string(pluralize(length(reports), "error"), " found")
    printstyled(io, s, '\n'; color = ERROR_COLOR)
    wrote_linfos = Set{UInt64}()
    foreach(reports) do report
        print_report(io, report, wrote_linfos; kwargs...)
    end

    return
end

function print_report end # should be overloaded

# utilities
# ---------

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = [:bold, :light_cyan, :light_green, :light_yellow]

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    return string(n, ' ', isone(n) ? one : more)

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
