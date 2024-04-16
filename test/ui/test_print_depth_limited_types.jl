#=
using Revise; include(joinpath("test", "ui", "test_print_depth_limited_types.jl"))
=#
using Test
import JET

struct F49231{a,b,c,d,e,f,g}
    num::g
end;
bar(x) = rand() > 0.5 ? x : Any[0][1]
mysum(x) = sum(y-> bar(x.num), 1:5; init=0)

JET.maxtypedepth[] = 2
@testset "Depth-limited type printing" begin
    f = F49231{Float64,Float32,Int,String,AbstractString,6,Float64}(1)
    Ftype = Tuple{Vector{typeof(f)}}
    result = JET.report_opt(Ftype) do a
        mysum(a[1]) # runtime dispatch !
    end
    buf = IOBuffer()
    show(buf, result)
    s = String(take!(buf))
    @test occursin("F49231{…}", s)
end

JET.maxtypedepth[] = 100
@testset "Depth-limited type printing - show types with large depth" begin
    f = F49231{Float64,Float32,Int,String,AbstractString,6,Float64}(1)
    Ftype = Tuple{Vector{typeof(f)}}
    result = JET.report_opt(Ftype) do a
        mysum(a[1]) # runtime dispatch !
    end
    buf = IOBuffer()
    show(buf, result)
    s = String(take!(buf))
    @test !occursin("F49231{…}", s)
end
