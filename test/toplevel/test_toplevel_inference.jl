module test_toplevel_inference

include("../setup.jl")

@testset "inference with abstract binding" begin
    let res = @analyze_toplevel begin
            global a::Int
            sin(a)
        end
        isexpected = length(res.res.inference_error_reports) == 1
        @test isexpected
        if isexpected
            report = only(res.res.inference_error_reports)
            @test isa(report, UndefVarErrorReport)
            @test report.var.name === :a
            @test !report.maybeundef
        end
    end

    let res = @analyze_toplevel begin
            const a = 0
            sin(a)
        end
        @test isempty(res.res.inference_error_reports)
    end
    let res = @analyze_toplevel begin
            const a = :jetzero # should be quoted, otherwise undef var error
            String(a)
        end
        @test isempty(res.res.inference_error_reports)
    end
    let res = @analyze_toplevel begin
            global a = 0
            sin(a)
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    # sequential
    let res = @analyze_toplevel begin
            const a = rand(Int)
            println(sin(a))
            const a = 0
            println(sin(a))
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    let res = @analyze_toplevel begin
            const a = rand(Int)
            println(sin(a))
            const a = "julia"
            println(length(a))
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
    let res = @analyze_toplevel begin
            const a = rand(Int)
            println(sin(a))
            const a = "julia"
            println(sum(a))
        end
        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end
    let res = @analyze_toplevel begin
            global a = 0
            sin(a)
            global a = 0.0
            sin(a)
        end
        @test isempty(res.res.toplevel_error_reports)
    end
end

@testset "`const x_ = ...` should not be concretized by default" begin
    mktemp() do path, io
        res = @eval @analyze_toplevel begin
            using Downloads
            function parse(config::String)
                println("Do something")
            end
            const projectfile = Downloads.download(
                "https://raw.githubusercontent.com/aviatesk/JET.jl/refs/heads/master/Project.toml", $(path))
            parse(projectfile)
        end
        @test isempty(read(path, String))
        @test isempty(res.res.toplevel_error_reports)
        # @test isempty(res.res.inference_error_reports) # should be enabled once https://github.com/JuliaLang/julia/pull/58212 is merged
    end
    mktemp() do path, io
        res = @eval @analyze_toplevel begin
            const x = let s = "julia"
                println($io, s)
                s
            end
            length(x)
        end
        flush(io)
        s = read(path, String)
        @test isempty(read(path, String))
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "module usage of abstract binding" begin
    let res = @analyze_toplevel begin
            module TopModule
            const somename = "julia"
            module InnerModule
            using ..TopModule: somename
            sum(somename)
            end # module InnerModule
            end # module TopModule
        end
        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end
    let res = @analyze_toplevel begin
            module Exporter
            export exported_name
            const exported_name = "julia"
            end
            using .Exporter
            sum(exported_name)
        end
        @test isempty(res.res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "conditional assignment" begin
    let res = @analyze_toplevel begin
            if rand(Bool)
                global s::Vector{Int} = rand(Int, 10)
            end
            sum(s)
        end
        isone = length(res.res.inference_error_reports) == 1
        @test isone
        if isone
            report = only(res.res.inference_error_reports)
            @test isa(report, UndefVarErrorReport)
            @test report.var.name === :s
            @test occursin("may be undefined", get_msg(report))
        end
    end
    let res = @analyze_toplevel begin
            if rand(Bool)
                const s = "julia"
            end
            sum(s)
        end
        isexpected = length(res.res.inference_error_reports) == 3
        @test isexpected
        if isexpected
            @test any(res.res.inference_error_reports) do report
                isa(report, UndefVarErrorReport) &&
                report.var.name === :s &&
                occursin("may be undefined", get_msg(report))
            end
            test_sum_over_string(res)
        end
    end
    let res = @analyze_toplevel begin
            const s = rand(Int, 10)
            if rand(Bool)
                const s = "julia"
            end
            sum(s)
        end
        isexpected = length(res.res.inference_error_reports) == 3
        @test isexpected
        if isexpected
            @test any(res.res.inference_error_reports) do report
                isa(report, UndefVarErrorReport) &&
                report.var.name === :s &&
                occursin("may be undefined", get_msg(report))
            end
            test_sum_over_string(res)
        end
    end
end

@testset "multiple declaration/assignment" begin
    let res = @analyze_toplevel begin
            r1, r2 = rand(2)
            println(r1, r2)
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let res = @analyze_toplevel begin
            begin
                local r1, r2
                r1, r2 = rand(2)
                println(r1, r2)
            end
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let res = @analyze_toplevel begin
            let
                global r1, r2
                r1, r2 = rand(2)
            end
            println(r1, r2)
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let res = @analyze_toplevel begin
            ro1, ro2 = let
                ri1, ri2 = rand(2)
                println(ri1, ri2)
                ri1, ri2
            end
            println(ro1, ro2)
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end

    let res = @analyze_toplevel begin
            begin
                local l
                l, g = rand(2)
                println(l, g)
            end
            println(g)
        end
        @test isempty(res.res.toplevel_error_reports)
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "https://github.com/aviatesk/JET.jl/issues/142" begin
    let res = @analyze_toplevel begin
            Circle = @NamedTuple begin
                radius::Float64
            end
            function area(c::Circle)
                pi * c.radius^2
            end
            @show area(Circle(2))
        end
        @test_broken isempty(res.res.toplevel_error_reports)
    end
    let res = @analyze_toplevel begin
            const Circle = @NamedTuple begin
                radius::Float64
            end
            function area(c::Circle)
                pi * c.radius^2
            end
            @show area(Circle(2))
        end
    end
end

@testset "https://github.com/aviatesk/JET.jl/issues/280" begin
    res = @analyze_toplevel begin
        using Libdl
        let llvmpaths = filter(lib -> occursin(r"LLVM\b", basename(lib)), Libdl.dllist())
            if length(llvmpaths) != 1
                throw(ArgumentError("Found one or multiple LLVM libraries"))
            end
            libllvm = Libdl.dlopen(llvmpaths[1])
            gethostcpufeatures = Libdl.dlsym(libllvm, :LLVMGetHostCPUFeatures)
            ccall(gethostcpufeatures, Cstring, ())
        end
    end
    @test isempty(res.res.toplevel_error_reports)
    @test isempty(res.res.inference_error_reports)
end

let res = @analyze_toplevel begin
        var = rand(Bool)
        const constvar = rand(Bool)
        println(var, constvar)
    end
    @test isempty(res.res.toplevel_error_reports)
    @test isempty(res.res.inference_error_reports)
end

@testset "MissingConcretizationErrorReport" begin
    let res = @analyze_toplevel begin
            RandomType = rand((Bool,Int))
            struct Struct
                field::RandomType
            end
        end
        isone = length(res.res.toplevel_error_reports) == 1
        @test isone
        if isone
            report = only(res.res.toplevel_error_reports)
            @test isa(report, MissingConcretizationErrorReport)
            @test report.var.name === :RandomType
            @test !report.isconst
        end
    end
end

end # module test_toplevel_inference
