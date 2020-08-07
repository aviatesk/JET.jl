# print reports nicely:
# - TODO: maybe we want to show callsites instead of method sigs in a calltrace
# - TODO: method sigs or callsites can be type annotated with profiled types

# entry methods
# -------------

function print_reports(io::IO, interp::TPInterpreter; kwargs...)
    print_reports(io, interp.reports; kwargs...)
    return
end

function print_reports(io::IO, reports::Vector{<:ErrorReport};
                       view::Symbol = :inline,
                       filter_native_remarks::Bool = true)
    if filter_native_remarks
        reports = filter(r->!isa(r, NativeRemark), reports)
    end

    if isempty(reports)
        printstyled(io, "No errors !\n"; color = NOERROR_COLOR)
        return
    end

    if view === :inline
        s = string(pluralize(length(reports), "error"), " found")
        printstyled(io, s, '\n'; color = ERROR_COLOR)
        wrote_linfos = Set{UInt64}()
        foreach(reports) do report
            print_report(io, report, wrote_linfos)
        end
    elseif view === :separate
        foreach(reports) do report
            print(io, "Error: ")
            s = report_string(report)
            printstyled(io, s, '\n'; color = ERROR_COLOR)
            println(io, "Calltrace:")
            print_report(io, report)
        end
    else
        error("keyword argument `view` should either of :inline or :separate")
    end

    return
end

print_reports(args...; kwargs...) = print_reports(stdout, args...; kwargs...)

function print_report(io, report, wrote_linfos = Set{UInt64}())
    print_calltrace(io, report.acs, wrote_linfos)
    n = length(report.acs) - 1
    print_rails(io, n)
    printstyled(io, "│ ", report_string(report), '\n'; color = ERROR_COLOR)
    print_rails(io, n)
    printstyled(io, '└', '\n'; color = ERROR_COLOR)
    return
end

# traverse backedges, collect locations
# -------------------------------------

function print_calltrace(io, linfos, wrote_linfos, depth = 0)
    i = depth + 1
    linfo = linfos[i]

    linfo_hash = hash(linfo)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    if length(linfos) == i # error here
        print_location(io, linfo, depth, true)
        return
    end

    # print current frame adn go into deeper
    should_print && print_location(io, linfo, depth, false)
    print_calltrace(io, linfos, wrote_linfos, depth + 1)
    return
end

function print_location(io, linfo, depth, is_err)
    file, line = get_file_line(linfo)

    # rail
    print_rails(io, depth)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth+1)%length(RAIL_COLORS)+1]
    printstyled(io, "┌ @ ", file, ":", line; color)

    # source
    path = fullpath(string(file))
    source_line = if isfile(path)
        strip(readlines(path)[line])
    else
        string("within `", linfo.def, ''') # when the file doesn't exist, e.g. defined in REPL
    end
    println(io, ' ', source_line)

    return
end

# utils
# -----

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

get_file_line(mi::MethodInstance) = get_file_line(mi.def)
get_file_line(m::Method) = (; m.file, m.line)
get_file_line(m::Module) = error("get_file_line(::Module) unimplemented")

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
