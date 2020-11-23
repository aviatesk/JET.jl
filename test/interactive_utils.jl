using Test, JET, InteractiveUtils

const CC = JET.CC

import .CC:
    widenconst, ⊑

import JET:
    JETInterpreter, VirtualGlobalVariable, profile_call, get_result, virtual_process!,
    gen_virtual_module, ToplevelErrorReport, InferenceErrorReport, print_reports

for sym in Symbol.(last.(Base.Fix2(split, '.').(string.(vcat(subtypes(JET, ToplevelErrorReport),
                                                             subtypes(JET, InferenceErrorReport),
                                                             )))))
    Core.eval(@__MODULE__, :(import JET: $(sym)))
end

import Base:
    Fix1, Fix2

import Base.Meta:
    isexpr

const FIXTURE_DIR = normpath(@__DIR__, "fixtures")

# define virtual module and setup fixtures
macro def(ex)
    @assert isexpr(ex, :block)
    return quote let
        vmod = $(gen_virtual_module)()
        Core.eval(vmod, $(QuoteNode(ex)))
        vmod # return virtual module
    end end
end

# profile from file name
profile_file′(filename, args...; kwargs...) =
    return profile_text′(read(filename, String), args...; filename, kwargs...)

# profile from string
function profile_text′(s,
                       virtualmod = gen_virtual_module(@__MODULE__);
                       filename = "top-level",
                       actualmodsym = Symbol(parentmodule(virtualmod)),
                       interp = JETInterpreter(),
                       )
    return virtual_process!(s,
                            filename,
                            virtualmod,
                            actualmodsym,
                            interp)
end

# profile from toplevel expression
macro profile_toplevel(virtualmod, ex)
    return profile_toplevel(virtualmod, ex, __source__)
end

macro profile_toplevel(ex)
    virtualmod = gen_virtual_module(__module__)
    return profile_toplevel(virtualmod, ex, __source__)
end

function profile_toplevel(virtualmod, ex, lnn)
    toplevelex = (isexpr(ex, :block) ?
                  Expr(:toplevel, lnn, ex.args...) : # flatten here
                  Expr(:toplevel, lnn, ex)
                  ) |> QuoteNode
    return quote let
        interp = $(JETInterpreter)()
        ret = $(JET.gen_virtual_process_result)()
        virtualmod = $(esc(virtualmod))
        actualmodsym = Symbol(parentmodule(virtualmod))
        $(virtual_process!)($(toplevelex), $(string(lnn.file)), virtualmod, actualmodsym, interp, ret)
    end end
end

is_concrete(mod, sym) = isdefined(mod, sym) && !isa(getfield(mod, sym), VirtualGlobalVariable)
is_abstract(mod, sym) = isdefined(mod, sym) && isa(getfield(mod, sym), VirtualGlobalVariable)
abstract_isa(vgv, @nospecialize(typ)) = isa(vgv, VirtualGlobalVariable) && vgv.t ⊑ typ
