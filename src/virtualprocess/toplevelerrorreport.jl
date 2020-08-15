# types
# -----

abstract type ToplevelErrorReport end

struct SyntaxErrorReport <: ToplevelErrorReport
    err::ErrorException
    file::String
    line::Int
    SyntaxErrorReport(msg::AbstractString, file, line) = new(ErrorException(msg), file, line)
end

# wraps general errors from actual Julia process
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    bt::Vector{Union{Ptr{Nothing},Base.InterpreterIP}}
    file::String
    line::Int

    # default constructor
    ActualErrorWrapped(err, bt, file, line) = new(err, bt, file, line)

    # bypass syntax error
    function ActualErrorWrapped(err::ErrorException, bt, file, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, file, line)
        else
            new(err, bt, file, line)
        end
    end
end

# print
# -----

function print_reports(io, reports::Vector{<:ToplevelErrorReport})
    isempty(reports) && return

    s = string(pluralize(length(reports), "toplevel error"), " found")
    printstyled(io, s, '\n'; color = ERROR_COLOR)

    foreach(reports) do report
        printstyled(io, " @ ", report.file, ':', report.line, '\n')
        print_report(io, report)
        println("---")
    end

    return
end

print_report(io, report::SyntaxErrorReport) = Base.display_error(report.err, []) # don't show stacktrace for syntax errors
# TODO:
# - crop internal backtraces
# - add context information, i.e. during macroexpansion, defining something
print_report(io, report::ActualErrorWrapped) = Base.display_error(report.err, report.bt)
