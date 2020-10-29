using Test, JET, InteractiveUtils

# show version info in CI
if get(ENV, "CI", nothing) == "true"
    versioninfo()
end

import Core.Compiler:
    widenconst

import JET:
    JETInterpreter, VirtualGlobalVariable, profile_call, get_result, virtual_process!,
    gen_virtual_module, report_errors, ToplevelErrorReport, InferenceErrorReport,
    print_reports

for sym in Symbol.(last.(Base.Fix2(split, '.').(string.(vcat(subtypes(JET, ToplevelErrorReport),
                                                             subtypes(JET, InferenceErrorReport),
                                                             )))))
    Core.eval(@__MODULE__, :(import JET: $(sym)))
end

import Base:
    Fix1, Fix2

import Base.Meta:
    isexpr

import Core.Compiler:
    ⊑

const CC = Core.Compiler
const FIXTURE_DIR = normpath(@__DIR__, "fixtures")

const ERROR_REPORTS_FROM_SUM_OVER_STRING = let
    interp, frame = profile_call(sum, String)
    @test !isempty(interp.reports)
    interp.reports
end

function test_sum_over_string(ers)
    @test !isempty(ers)
    for target in ERROR_REPORTS_FROM_SUM_OVER_STRING
        @test any(ers) do er
            return er.msg == target.msg && er.sig == target.sig
        end
    end
end
test_sum_over_string(res::JET.VirtualProcessResult) =
    test_sum_over_string(res.inference_error_reports)
test_sum_over_string(interp::JETInterpreter) = test_sum_over_string(interp.reports)

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

# TODO: remove once https://github.com/JuliaLang/julia/pull/38210 gets merged
using Random
@eval Random begin
    function make_seed()
        try
            return rand(RandomDevice(), UInt32, 4)
        catch
            println(stderr,
                    "Entropy pool not available to seed RNG; using ad-hoc entropy sources.")
            seed = reinterpret(UInt64, time())
            seed = hash(seed, getpid() % UInt)
            try
                seed = hash(seed, parse(UInt64,
                                        read(pipeline(`ifconfig`, `sha1sum`), String)[1:40],
                                        base = 16) % UInt)
            catch
            end
            return make_seed(seed)
        end
    end
end
