# NOTE: known limitations
# - macro including access to global variables:
#   TP will try to expand any macro, but since macros can access to any global object and so
#   any macro expansion including access to global variables will fail as far as TP avoids
#   evaluation of them but just profiles/preserves their types.
module VirtualMachine

# loaded symbols
import Base:
    parse_input_line, Meta.isexpr, Meta._parse_string

import ..TypeProfiler:
    ErrorReport, ERROR_COLOR,  NOERROR_COLOR, RAIL_COLORS, print_rails, fullpath,
    print_reports

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
    reports = _profile_text(virtualmod, text, filename)
    print_reports(io, reports)
end
profile_text(args...) = profile_text(stdout, args...)

function _profile_text(mod, text, filename)
    ret = parse_to_toplevel(text, filename)
    ret isa Vector{ToplevelErrorReport} && return ret

    ret = transform_for_profiling!(mod, ret)
    ret isa Vector{ToplevelErrorReport} && return ret

    λ = generate_virtual_lambda(mod, ret)
    interp, = profile_call(λ)
    return interp.reports
end

generate_virtual_module(actualmod::Module) =
    return Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))

function generate_virtual_lambda(mod::Module, toplevelex::Expr)
    @assert isexpr(toplevelex, :toplevel) "toplevel expression should be given"

    ex = quote
        function ()
            $(toplevelex.args...)
        end
    end
    return Core.eval(mod, ex)
end

export
    profile_file, profile_text

end  # module VirtualMachine
