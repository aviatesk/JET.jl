# print reports nicely:
# - TODO: maybe we want to show callsites instead of method sigs in a calltrace
# - TODO: method sigs or callsites can be type annotated with profiled types

# entry methods
# -------------

print_reports(io::IO, interp::TPInterpreter, frame::InferenceState; kwargs...) =
    print_reports(io, interp.reports, frame; kwargs...)

function print_reports(io::IO, reports::Vector{<:ErrorReport}, frame::InferenceState;
                       view::Symbol = :inline)
    isempty(reports) && return printstyled(io, "No errors !\n"; color = NOERROR_COLOR)

    if view === :inline
        printstyled(io, length(reports), " errors found\n"; color = ERROR_COLOR)
        wrote_linfos = Set{UInt64}()
        foreach(reports) do report
            print_report(io, report, frame, wrote_linfos)
        end
    elseif view === :separate
        for report in reports
            print(io, "Error: ")
            printstyled(io, report_string(report), '\n'; color = ERROR_COLOR)
            println(io, "Calltrace:")
            print_report(io, report, frame)
        end
    else
        error("keyword argument `view` should either of :inline or :separate")
    end

    return
end

print_reports(args...; kwargs...) = print_reports(stdout, args...; kwargs...)

function print_report(io, report, frame, wrote_linfos = Set{UInt64}())
    depth = print_calltrace(io, report.linfo, frame, wrote_linfos)
    print_rails(io, depth - 1)
    printstyled(io, "│ ", report_string(report), '\n'; color = ERROR_COLOR)
    print_rails(io, depth - 1)
    printstyled(io, '└', '\n'; color = ERROR_COLOR)
    return
end

# traverse backedges, collect locations
# -------------------------------------

function print_calltrace(io, linfo, frame, wrote_linfos)
    root_lins = Set(flatten(filter(!isnothing, frame.stmt_edges)))
    return _print_calltrace(io, linfo, linfo, wrote_linfos, root_lins)
end

function _print_calltrace(io, linfo, err_linfo, wrote_linfos, root_lins)
    linfo_hash = hash(linfo)
    (should_print = linfo_hash ∉ wrote_linfos) && push!(wrote_linfos, linfo_hash)
    is_err = linfo == err_linfo

    if linfo in root_lins
        should_print && print_location(io, linfo, 0, is_err)
        return 1
    end

    @assert length(linfo.backedges) === 1
    prev_linfo = first(linfo.backedges)

    # prewalk
    depth = _print_calltrace(io, prev_linfo, err_linfo, wrote_linfos, root_lins)
    should_print && print_location(io, linfo, depth, is_err)
    return depth + 1
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
        string("within `", linfo.def, ''') # when the file doesn't exist, e.g. REPL
    end
    println(io, ' ', source_line)

    return
end

# utils
# -----

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = [:bold, :light_cyan, :light_green, :light_yellow]

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
