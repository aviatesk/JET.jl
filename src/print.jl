# TODO: absolute pass mode

# utility
# -------

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = [:bold, :light_cyan, :light_green, :light_yellow]

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    return string(n, ' ', isone(n) ? one : more)

printlnstyled(args...; kwarg...) = printstyled(args..., '\n'; kwarg...)
printsuccess(io) = printstyled(io, "No errors !\n"; color = NOERROR_COLOR)

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

function print_reports(io, reports::Vector{<:ToplevelErrorReport};
                       print_toplevel_sucess = false,
                       __kwargs...)
    isempty(reports) && return print_toplevel_sucess ? printsuccess(io) : nothing

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

    return
end

print_report(io, report::SyntaxErrorReport) = Base.display_error(report.err, []) # don't show stacktrace for syntax errors
# TODO: add context information, i.e. during macroexpansion, defining something
print_report(io, report::ActualErrorWrapped) = Base.display_error(report.err, report.bt)

# inference
# ---------

function print_reports(io, reports::Vector{<:InferenceErrorReport};
                       filter_native_remarks = true,
                       __kwargs...)
    if filter_native_remarks
        reports = filter(r->!isa(r, NativeRemark), reports)
    end

    isempty(reports) && return printsuccess(io)

    s = string(pluralize(length(reports), "error"), " found", '\n')
    printlnstyled(io, s; color = ERROR_COLOR)
    wrote_linfos = Set{UInt64}()
    foreach(reports) do report
        print_report(io, report, wrote_linfos)
    end

    return
end

function print_report(io, report::InferenceErrorReport, wrote_linfos)
    n = length(report.acs) - 1
    color = ERROR_COLOR

    print_calltrace(io, report.acs, wrote_linfos)
    print_rails(io, n)
    printlnstyled(io, "│ ", report_string(report); color)
    print_rails(io, n)
    printlnstyled(io, '└'; color)

    return
end

# traverse abstract call stack, collect locations
# - TODO: maybe we want to show callsites instead of method sigs in a calltrace
# - TODO: method sigs or callsites can be type annotated with profiled types

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

report_string(er::NoMethodErrorReport) = er.unionsplit ?
    "for one of the union split cases, no matching method found for signature: $(tt_to_signature_str(er.tt))" :
    "no matching method found for signature: $(tt_to_signature_str(er.tt))"
report_string(er::InvalidBuiltinCallErrorReport) =
    "invalid builtin function call: $(tt_to_signature_str(er.tt))"
report_string(er::UndefVarErrorReport) = isnothing(er.mod) ?
    "variable $(er.name) is not defined" :
    "variable $(er.mod).$(er.name) is not defined"
report_string(er::NonBooleanCondErrorReport) =
    "non-boolean ($(er.t)) used in boolean context"
report_string(r::NativeRemark) = r.s

# returns a call signature string from tt
function tt_to_signature_str(@nospecialize(tt::Type{<:Tuple}))
    fn = ft_to_fname(tt.parameters[1])
    args = join("::" .* string.(tt.parameters[2:end]), ", ")
    return string(fn, '(', args, ')')
end

# returns function name from its type
function ft_to_fname(@nospecialize(ft))
    return if isconstType(ft)
        ft.parameters[1]
    elseif ft isa DataType && isdefined(ft, :instance)
        ft.instance
    else
        ft
    end |> string
end

get_file_line(mi::MethodInstance) = get_file_line(mi.def)
get_file_line(m::Method) = (; m.file, m.line)
get_file_line(m::Module) = error("get_file_line(::Module) unimplemented")
