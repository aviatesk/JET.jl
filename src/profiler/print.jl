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
    depth = print_calltrace(io, report.linfo, wrote_linfos)
    print_rails(io, depth - 1)
    printstyled(io, "│ ", report_string(report), '\n'; color = ERROR_COLOR)
    print_rails(io, depth - 1)
    printstyled(io, '└', '\n'; color = ERROR_COLOR)
    return
end

# traverse backedges, collect locations
# -------------------------------------

print_calltrace(io, linfo, wrote_linfos) =
    return _print_calltrace(io, linfo, linfo, wrote_linfos)

function _print_calltrace(io, linfo, err_linfo, wrote_linfos)
    linfo_hash = hash(linfo)
    (should_print = linfo_hash ∉ wrote_linfos) && push!(wrote_linfos, linfo_hash)
    is_err = linfo == err_linfo

    if !isdefined(linfo, :backedges) # root here
        should_print && print_location(io, linfo, 0, is_err)
        return 1
    end

    prev_linfo = get_latest_backedge!(linfo)

    # prewalk
    depth = _print_calltrace(io, prev_linfo, err_linfo, wrote_linfos)
    should_print && print_location(io, linfo, depth, is_err)
    return depth + 1
end

# FIXME:
# this backedge traverse is fragile and doesn't work in cases when trying to show `NativeRemark`s,
# e.g. `@profile_call sum([])`
function get_latest_backedge!(linfo)
    if isempty(linfo.backedges)
        @error "no backedges found for this linfo"
        throw(linfo)
    end

    ret = if length(linfo.backedges) !== 1
        # XXX:
        # there may be cases when there're multiple backedges with different signatures ?
        # such a case should be problematic and reported for inspection
        unique(linfo.backedges) do backedge
            backedge.def.sig
        end |> length === 1 || begin
            @error "multiple backedges with different signatures found for this linfo"
            throw(linfo)
        end

        # we're here when profiled method defined multiple time with the same signature,
        # and this can happen so often in an interactive session like REPL,
        # let's just get the latest definion
        msg = "more than one backedge found for: $(linfo.backedges)"
        isinteractive() ? @debug(msg) : @warn(msg)

        sort(linfo.backedges;
             by = backedge -> backedge.def.primary_world,
             rev = true)
    else
        linfo.backedges
    end |> first

    if linfo == ret
        @error "recursive backedge detected"
        throw(linfo)
    end

    return ret
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
