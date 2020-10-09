# %% setup
# --------

using Test, TypeProfiler, InteractiveUtils

# show version info in CI
if get(ENV, "CI", nothing) == "true"
    versioninfo()
end

import Core.Compiler:
    widenconst

import TypeProfiler:
    TPInterpreter, VirtualGlobalVariable, profile_call, get_result, virtual_process!,
    gen_virtual_module, report_errors, ToplevelErrorReport, InferenceErrorReport,
    print_reports

for sym in Symbol.(last.(Base.Fix2(split, '.').(string.(vcat(subtypes(TypeProfiler, ToplevelErrorReport),
                                                             subtypes(TypeProfiler, InferenceErrorReport),
                                                             )))))
    Core.eval(@__MODULE__, :(import TypeProfiler: $(sym)))
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
test_sum_over_string(res::TypeProfiler.VirtualProcessResult) =
    test_sum_over_string(res.inference_error_reports)
test_sum_over_string(interp::TPInterpreter) = test_sum_over_string(interp.reports)

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
                       interp = TPInterpreter(),
                       )
    return virtual_process!(s, filename, actualmodsym, virtualmod, interp)
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
        interp = $(TPInterpreter)()
        ret = $(TypeProfiler.gen_virtual_process_result)()
        virtualmod = $(esc(virtualmod))
        actualmodsym = Symbol(parentmodule(virtualmod))
        $(virtual_process!)($(toplevelex), $(string(lnn.file)), actualmodsym, virtualmod, interp, ret)
    end end
end

is_concrete(mod, sym) = isdefined(mod, sym) && !isa(getfield(mod, sym), VirtualGlobalVariable)
is_abstract(mod, sym) = isdefined(mod, sym) && isa(getfield(mod, sym), VirtualGlobalVariable)
abstract_isa(vgv, @nospecialize(typ)) = isa(vgv, VirtualGlobalVariable) && vgv.t ⊑ typ

# %% test body
# ------------

@testset "TypeProfiler.jl" begin
    @testset "virtualprocess.jl" begin
        include("test_virtualprocess.jl")
    end

    @testset "abstractinterpretation.jl" begin
        include("test_abstractinterpretation.jl")
    end

    @testset "tfuncs.jl" begin
        include("test_tfuncs.jl")
    end

    @testset "tpcache.jl" begin
        include("test_tpcache.jl")
    end

    # tests with Windows-paths is just an hell
    @static Sys.iswindows() || @testset "print.jl" begin
        include("test_print.jl")
    end
end

# # favorite
# # --------
#
# # never ends otherwise
# fib(n) = n ≤ 2 ? n : fib(n-1) + fib(n-2)
# @profile_call fib(100000) # ::Int
# @profile_call fib(100000.) # ::Float64
# @profile_call fib(100000 + 100000im) # report !
