# NOTE: known limitations
# - macro including access to global variables:
#   TP will try to expand any macro, but since macros can access to any global object and so
#   any macro expansion including access to global variables will fail as far as TP avoids
#   evaluation of them but just profiles/preserves their types.
module VirtualProcess

# loaded symbols
import Base:
    parse_input_line, Meta.isexpr, Meta._parse_string

import ..TypeProfiler:
    ErrorReport, ERROR_COLOR,  NOERROR_COLOR, RAIL_COLORS, print_rails, fullpath, pluralize

using ..TypeProfiler.Profiler

include("toplevelerrorreport.jl")
include("surfaceast.jl")

function profile_file(io::IO, filename::AbstractString, mod::Module = Main)
    text = read(filename, String)
    profile_text(io, text, filename, mod)
end
profile_file(args...) = profile_file(stdout, args...)

function profile_text(io::IO, text::AbstractString, filename::AbstractString, mod::Module = Main)
    virtualmod = generate_virtual_module(mod)
    reports = report_errors(virtualmod, text, filename)
    print_reports(io, reports)
end
profile_text(args...) = profile_text(stdout, args...)

function report_errors(mod, text, filename)
    ret = parse_and_transform(mod, text, filename)
    isa(ret, Vector{<:ToplevelErrorReport}) && return ret

    λ = generate_virtual_lambda(mod, ret)
    # Core.eval(@__MODULE__, :(λ = $(λ)))
    interp, = profile_call(λ)
    return fix_virtual_traces!(interp.reports)
end

generate_virtual_module(actualmod::Module) =
    return Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))

function generate_virtual_lambda(mod::Module, toplevelex::Expr)
    @assert isexpr(toplevelex, :toplevel) "toplevel expression should be given"

    ex = :(function ()
        $(toplevelex.args...)
    end)
    return Core.eval(mod, ex)
end

# TODO:
# - report errors in virtual lambda as toplevel error
function fix_virtual_traces!(reports)
    return reports
end

export
    profile_file, profile_text

end  # module VirtualProcess
