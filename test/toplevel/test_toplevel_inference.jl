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

    @static if isdefinedglobal(Core, :declare_const)
        let vmod = gen_virtual_module()
            ex = Expr(:block,
                Expr(:const, :undefined_const),
                :(println(undefined_const)))
            lnn = LineNumberNode(@__LINE__, Symbol(@__FILE__))
            res = analyze_toplevel(ex, lnn; context=vmod, virtualize=false)
            @test !isdefinedglobal(vmod, :undefined_const)
            @test isempty(res.res.toplevel_error_reports)
            let report = only(res.res.inference_error_reports)
                @test report isa UndefVarErrorReport
                @test report.var.name === :undefined_const
                @test !report.maybeundef
            end

            res = analyze_toplevel(:(const undefined_const = 1), lnn;
                context=vmod, virtualize=false)
            @test @invokelatest(isdefinedglobal(vmod, :undefined_const))
            @test isempty(res.res.toplevel_error_reports)
            @test isempty(res.res.inference_error_reports)
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

@testset "top-level closure in abstract loop (aviatesk/JETLS.jl#555)" begin
    res = @analyze_toplevel begin
        function calc_kite_pos(turn_angle)
            return [cos(turn_angle), sin(turn_angle), 0.0]
        end

        const THETA = [30, 45, 60, 75]
        turn_angles = 0:1:360
        ys_all = Vector{Vector{Float64}}()

        for θ in THETA
            push!(ys_all, [calc_kite_pos(deg2rad(ta))[2] for ta in turn_angles])
        end
    end
    @test isempty(res.res.toplevel_error_reports)
    @test isempty(res.res.inference_error_reports)
end

@testset "top-level @eval with spliced generator (aviatesk/JETLS.jl#341)" begin
    res = @analyze_toplevel begin
        for n = 1:4
            func_name = Symbol("fn$n")
            arg_names = Tuple(Symbol("arg$j") for j in 1:n)
            @eval function $func_name(
                    w,
                    $((:($arg_name::Int) for arg_name in arg_names)...)
                )
                return println(w, ($(arg_names...),))
            end
        end
    end
    @test isempty(res.res.toplevel_error_reports)
    @test isempty(res.res.inference_error_reports)
end

@testset "`concretization_pattern`" begin
    @test JET.concretization_pattern(:(A = rand(Int))) == :(A = x_)
    @test JET.concretization_pattern(:(A::DataType = rand(Int))) == :(A::DataType = x_)
    @test JET.concretization_pattern(:(global A = rand(Int))) == :(global A = x_)
    @test JET.concretization_pattern(:(const A = rand(Int))) == :(const A = x_)
    @test JET.concretization_pattern(:(A = B = rand(Int))) == :(A = B = x_)
    @test JET.concretization_pattern(:((A, B) = rand(Int, 2))) == :((A, B) = x_)
    for name in (:_, :__, :A_, :A__, :A__str)
        @test JET.concretization_pattern(Expr(:(=), name, :(rand(Int)))) === nothing
    end
    @test JET.concretization_pattern(:(A::DataType_ = rand(Int))) === nothing
    @test JET.concretization_pattern(:((A, B_) = rand(Int, 2))) === nothing
    @test JET.concretization_pattern(:(A = B_ = rand(Int))) == :(A = x_)
    # names that MacroTools matches literally are kept as-is
    for name in (:A_b, :USE_PULSE, :A_str, :A___, :_A)
        @test JET.concretization_pattern(Expr(:(=), name, :(rand(Int)))) == Expr(:(=), name, :x_)
    end
    # short-form function definitions never give a global binding its value
    @test JET.concretization_pattern(:(f(x) = 2x)) === nothing
    @test JET.concretization_pattern(:(f(x::T) where T = 2x)) === nothing
    @test JET.concretization_pattern(:(A + B)) === nothing
    @test JET.concretization_pattern(:A) === nothing
    # a nested assignment would need a pattern covering the whole top-level statement,
    # which is left to the user to write
    @test JET.concretization_pattern(:(let; global A = rand(Int); end)) === nothing
    @test JET.concretization_pattern(:(if c; A = 1; else; A = 2; end)) === nothing

    # the statement is located regardless of whether a pattern could be derived
    let assignment = JET.toplevel_assignment(:(let; global A = rand(Int); end), "f.jl", 2)
        @test assignment.pattern === nothing
        @test assignment.file == "f.jl"
        @test assignment.line == 2
    end
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
            @test report.assignment isa JET.ToplevelAssignment
            @test report.assignment.pattern == :(RandomType = x_)

            msg = sprint(JET.print_report, report)
            @test occursin("JET needs its concrete value", msg)
            @test occursin("JET tracked that the binding exists", msg)
            @test occursin("`const RandomType = ...`", msg)
            @test occursin("This helps only when JET can infer the concrete value", msg)
            @test occursin("if JET still cannot determine the value", msg)
            @test occursin("concretization_patterns = [:(RandomType = x_)]", msg)
            @test occursin("because matching code is executed", msg)
            @test occursin("the assignment at $(report.assignment.file):$(report.assignment.line)", msg)
        end
    end

    let res = @analyze_toplevel begin
            CONFIG_ = rand((Bool, Int))
            struct UnderscoredBindingStruct
                field::CONFIG_
            end
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa MissingConcretizationErrorReport
        @test report.assignment isa JET.ToplevelAssignment
        @test report.assignment.pattern === nothing
        msg = sprint(JET.print_report, report)
        @test occursin("treats `CONFIG_` itself", msg)
        @test occursin("match every assignment", msg)
        @test occursin("Consider renaming the binding", msg)
        @test !occursin("concretization_patterns = [:(CONFIG_ = x_)]", msg)
    end

    let res = @analyze_toplevel begin
            TypedRandomType::DataType = rand((Bool, Int))
            struct TypedStruct
                field::TypedRandomType
            end
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa MissingConcretizationErrorReport
        @test report.assignment isa JET.ToplevelAssignment
        @test report.assignment.pattern == :(TypedRandomType::DataType = x_)
    end

    let res = @analyze_toplevel concretization_patterns = [:(TypedRandomType::DataType = x_)] begin
            TypedRandomType::DataType = rand((Bool, Int))
            struct TypedStruct
                field::TypedRandomType
            end
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    let res = @analyze_toplevel begin
            global GlobalRandomType = rand((Bool, Int))
            struct GlobalStruct
                field::GlobalRandomType
            end
        end
        report = only(res.res.toplevel_error_reports)
        @test report isa MissingConcretizationErrorReport
        @test report.assignment isa JET.ToplevelAssignment
        @test report.assignment.pattern == :(global GlobalRandomType = x_)
    end

    let res = @analyze_toplevel concretization_patterns = [:(global GlobalRandomType = x_)] begin
            global GlobalRandomType = rand((Bool, Int))
            struct GlobalStruct
                field::GlobalRandomType
            end
        end
        @test isempty(res.res.toplevel_error_reports)
    end

    mktempdir() do dir
        main_file = joinpath(dir, "main.jl")
        assignment_file = joinpath(dir, "config.jl")
        use_file = joinpath(dir, "use.jl")
        write(main_file, "include(\"config.jl\")\ninclude(\"use.jl\")\n")
        write(assignment_file, "IncludedRandomType::DataType = rand((Bool, Int))\n")
        write(use_file, "struct IncludedStruct\n    field::IncludedRandomType\nend\n")

        res = report_file2(main_file)
        report = only(res.res.toplevel_error_reports)
        @test report isa MissingConcretizationErrorReport
        @test report.file == use_file
        @test report.assignment isa JET.ToplevelAssignment
        @test report.assignment.file == assignment_file
        @test report.assignment.pattern == :(IncludedRandomType::DataType = x_)
        # the report is anchored at the use site, so the message needs to point elsewhere
        @test occursin("the assignment at $assignment_file:1", sprint(JET.print_report, report))
    end

    # a pattern covering the enclosing statement is left to the user to write, so JET must
    # not suggest one; `:(NestedRandomType = x_)` in particular would never match here.
    # The statement is still located, which is what the user needs to write the pattern.
    @testset "nested assignment statement" begin
        res = JET.report_text("""
            let
                global NestedRandomType = rand((Bool, Int))
            end
            struct NestedStruct
                field::NestedRandomType
            end
            """)
        report = only(res.res.toplevel_error_reports)
        @test report isa MissingConcretizationErrorReport
        @test report.assignment isa JET.ToplevelAssignment
        @test report.assignment.pattern === nothing
        @test report.assignment.line == 1 # the `let` statement, not the use site
        @test report.line == 4
        msg = sprint(JET.print_report, report)
        @test occursin("the assignment at $(report.assignment.file):1", msg)
        @test occursin("could not derive one from the statement holding that", msg)
        @test !occursin("concretization_patterns = [:(NestedRandomType = x_)]", msg)
    end
end

end # module test_toplevel_inference
