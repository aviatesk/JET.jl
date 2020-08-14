# types
# -----

abstract type ToplevelErrorReport <: ErrorReport end

struct SyntaxErrorReport <: ToplevelErrorReport
    msg::String
    line::Int
end

# wraps general errors from actual Julia process
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    line::Int

    # default constructor
    ActualErrorWrapped(err, line) = new(err, line)

    # bypass syntax error
    function ActualErrorWrapped(err::ErrorException, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, line)
        else
            new(err, line)
        end
    end
end

# print
# -----

function print_report(io, report::ToplevelErrorReport, wrote_linfos; kwargs...)
    # print_calltrace(io, report.acs, wrote_linfos)
    # n = length(report.acs) - 1
    # print_rails(io, n)
    # printstyled(io, "│ ", report_string(report), '\n'; color = ERROR_COLOR)
    # print_rails(io, n)
    # printstyled(io, '└', '\n'; color = ERROR_COLOR)
    printstyled(io, "implement this !"; color = ERROR_COLOR)

    return
end
