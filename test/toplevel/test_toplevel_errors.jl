module test_toplevel_errors

include("../setup.jl")

struct RecordingInterpreter{I<:JET.ConcreteInterpreter} <: JET.ConcreteInterpreter
    inner::I
    states::Vector{JET.InterpretationState}
end
JET.InterpretationState(interp::RecordingInterpreter) = JET.InterpretationState(interp.inner)
JET.ToplevelAbstractAnalyzer(interp::RecordingInterpreter) =
    JET.ToplevelAbstractAnalyzer(interp.inner)
function JET.ConcreteInterpreter(interp::RecordingInterpreter, state::JET.InterpretationState)
    push!(interp.states, state)
    return RecordingInterpreter(JET.ConcreteInterpreter(interp.inner, state), interp.states)
end

@testset "first fatal top-level error" begin
    @testset "$Report" for (source, Report) in (
            "@undefined_macro_for_abort" => MacroExpansionErrorReport,
            "let; const local_value = 1; end" => LoweringErrorReport,
            "@eval error(\"first failure\")" => ActualErrorWrapped,
            "x = rand(Int)\nstruct NeedsConcrete <: x end" => MissingConcretizationErrorReport,
            "using .UndefinedModuleForAbort" => ActualErrorWrapped)
        context = gen_virtual_module()
        res = report_text(source * "\n@eval const after_failure = true\n";
            context, virtualize=false)
        @test res.res.toplevel_error_report isa Report
        @test get_reports(res) == [res.res.toplevel_error_report]
        @test !(@invokelatest isdefinedglobal(context, :after_failure))
    end
    let context = gen_virtual_module()
        res = report_text("""
            before_failure() = undefined_in_definition
            @eval error("first failure")
            @eval error("second failure")
            after_failure() = undefined_after_failure
            """; context, virtualize=false, analyze_from_definitions=true)
        report = res.res.toplevel_error_report
        @test report isa ActualErrorWrapped
        @test report.err == ErrorException("first failure")
        @test length(res.res.signature_infos) == 1
        @test isempty(res.res.inference_error_reports)
        @test !(@invokelatest isdefinedglobal(context, :after_failure))
    end
    let res = report_text("undefined_before_failure\n@eval error(\"failure\")")
        @test is_global_undef_var(only(res.res.inference_error_reports), :undefined_before_failure)
        @test get_reports(res) == [res.res.toplevel_error_report]
    end
end

@testset "fatal errors unwind modules and includes" begin
    mktempdir() do dir
        main = joinpath(dir, "main.jl")
        child = joinpath(dir, "child.jl")
        write(child, """
            module Child
                @eval error("nested failure")
                struct AfterFailure end
            end
            struct AfterModule end
            """)
        context = gen_virtual_module()
        states = JET.InterpretationState[]
        interp = RecordingInterpreter(JETConcreteInterpreter(JETAnalyzer()), states)
        config = JET.ToplevelConfig(; context, virtualize=false)
        old_main_uuid = JET.Preferences.main_uuid[]
        res = JET.virtual_process(interp, """
            module Outer
                include("child.jl")
                struct AfterInclude end
            end
            struct AfterOuter end
            include("never-reached.jl")
            """, main, config)
        report = res.toplevel_error_report
        @test report isa ActualErrorWrapped
        @test report.err == ErrorException("nested failure")
        @test report.file == child
        @test Set(keys(res.analyzed_files)) == Set([main, child])
        outer = @invokelatest getglobal(context, :Outer)
        childmod = @invokelatest getglobal(outer, :Child)
        @test !(@invokelatest isdefinedglobal(childmod, :AfterFailure))
        @test !(@invokelatest isdefinedglobal(outer, :AfterModule))
        @test !(@invokelatest isdefinedglobal(outer, :AfterInclude))
        @test !(@invokelatest isdefinedglobal(context, :AfterOuter))
        @test !isempty(states)
        @test all(state -> isempty(state.files_stack), states)
        @test all(state -> isempty(state.caught_callee_errors), states)
        @test JET.Preferences.main_uuid[] == old_main_uuid
    end
    mktempdir() do dir
        main = joinpath(dir, "main.jl")
        write(main, "include(\"main.jl\")\nstruct AfterFailure end\n")
        context = gen_virtual_module()
        res = report_file2(main; context, virtualize=false)
        @test res.res.toplevel_error_report isa RecursiveIncludeErrorReport
        @test !(@invokelatest isdefinedglobal(context, :AfterFailure))
    end
end

@testset "abort signals bypass user handlers" begin
    mktempdir() do dir
        main = joinpath(dir, "main.jl")
        child = joinpath(dir, "child.jl")
        write(child, "x = rand(Int)\nstruct NeedsConcrete <: x end\n")
        context = gen_virtual_module()
        states = JET.InterpretationState[]
        interp = RecordingInterpreter(JETConcreteInterpreter(JETAnalyzer()), states)
        config = JET.ToplevelConfig(; context, virtualize=false)
        res = JET.virtual_process(interp, """
            function load_child()
                try
                    include("child.jl")
                catch
                    @eval const caught_abort = true
                end
                @eval const continued_call = true
                return Integer
            end
            struct Trigger <: load_child() end
            struct AfterFailure end
            """, main, config)
        @test res.toplevel_error_report isa MissingConcretizationErrorReport
        @test res.toplevel_error_report.file == child
        @test !(@invokelatest isdefinedglobal(context, :caught_abort))
        @test !(@invokelatest isdefinedglobal(context, :continued_call))
        @test !(@invokelatest isdefinedglobal(context, :AfterFailure))
        @test all(state -> isempty(state.files_stack), states)
        @test all(state -> isempty(state.caught_callee_errors), states)
    end
    let context = gen_virtual_module()
        res = report_text("""
            @eval begin
                try
                    while true end
                catch
                    global caught_timeout = true
                end
            end
            struct AfterTimeout end
            """; context, virtualize=false, concretization_timeout=0.1)
        @test res.res.toplevel_error_report isa JET.ConcretizationTimeoutErrorReport
        @test !(@invokelatest isdefinedglobal(context, :caught_timeout))
        @test !(@invokelatest isdefinedglobal(context, :AfterTimeout))
    end
end

@testset "caught runtime exceptions are not fatal" begin
    @testset "$source" for source in (
            "@eval begin; try; error(\"caught\"); catch; end; end",
            "struct A <: try; throw(:caught); catch; Integer; end end")
        context = gen_virtual_module()
        res = report_text(source * "\nstruct AfterCatch end\n"; context, virtualize=false)
        @test isnothing(res.res.toplevel_error_report)
        @test (@invokelatest isdefinedglobal(context, :AfterCatch))
    end
end

@testset "native calls preserve interpreted rethrows" begin
    @testset "$call" for (call, expected) in (
            "rethrow()" => :original, "rethrow(:replacement)" => :replacement)
        source = "try; throw(:original); catch; $call; end"
        let res = report_text(source; concretization_patterns=[:x_])
            report = res.res.toplevel_error_report
            @test report isa ActualErrorWrapped
            @test report.err === expected
        end
        let context = gen_virtual_module()
            res = report_text("""
                try
                    $source
                catch err
                    global caught = err
                end
                """; context, virtualize=false, concretization_patterns=[:x_])
            @test isnothing(res.res.toplevel_error_report)
            @test (@invokelatest getglobal(context, :caught)) === expected
        end
    end
end

@testset "fatal errors in root handlers release frames" begin
    @testset "$body" for (body, Report) in (
            "throw(:fatal)" => ActualErrorWrapped,
            "while true end" => JET.ConcretizationTimeoutErrorReport)
        states = JET.InterpretationState[]
        interp = RecordingInterpreter(JETConcreteInterpreter(JETAnalyzer()), states)
        config = JET.ToplevelConfig(; concretization_patterns=[:x_],
            concretization_timeout=0.1)
        res = JET.virtual_process(interp, "try; throw(:caught); catch; $body; end",
            "top-level", config)
        @test res.toplevel_error_report isa Report
        @test !isempty(states)
        @test all(state -> isempty(state.caught_callee_errors), states)
        @test all(state -> isnothing(state.callee_error), states)
        @test all(state -> isempty(state.files_stack), states)
    end
end

@testset "parser warnings are not fatal" begin
    let context = gen_virtual_module()
        res = @test_logs report_text("""
            x = 1e-1000
            struct AfterWarning end
            """, "warnings.jl"; context, virtualize=false)
        @test isnothing(res.res.toplevel_error_report)
        warning = only(res.res.toplevel_warning_reports)
        @test warning isa JET.ParseWarningReport
        @test warning.diagnostic.level === :warning
        @test warning.file == "warnings.jl"
        @test warning.line == 1
        @test get_reports(res) == [warning]
        @test (@invokelatest isdefinedglobal(context, :AfterWarning))
    end
    let res = @test_logs report_text("""
            x = 1e-1000
            x = )
            y = )
            """)
        report = res.res.toplevel_error_report
        @test report isa ParseErrorReport
        @test report.diagnostic.level === :error
        @test report.line == 2
        @test only(res.res.toplevel_warning_reports) isa JET.ParseWarningReport
        @test get_reports(res) == [report]
    end
end

struct SuppressInferenceReports end
JET.configured_reports(::SuppressInferenceReports, ::Vector{InferenceErrorReport}) =
    InferenceErrorReport[]

@testset "warnings accompany inference reports" begin
    let res = @test_logs report_text("""
            x = 1e-1000
            y = 1e-1000
            f() = undefined_after_warning
            """; analyze_from_definitions=true)
        @test isnothing(res.res.toplevel_error_report)
        warnings = res.res.toplevel_warning_reports
        @test length(warnings) == 2
        @test all(r -> r isa JET.ParseWarningReport, warnings)
        @test [r.line for r in warnings] == [1, 2]
        report = only(res.res.inference_error_reports)
        @test is_global_undef_var(report, :undefined_after_warning)
        @test get_reports(res) == [warnings; report]
    end
    @testset "$configs" for configs in (
            (; target_modules=()), (; report_config=SuppressInferenceReports()))
        res = @test_logs report_text("x = 1e-1000\nundefined_after_warning"; configs...)
        @test !isempty(res.res.inference_error_reports)
        warning = only(res.res.toplevel_warning_reports)
        @test get_reports(res) == [warning]
    end
end

@testset "unsupported include mapping is a warning" begin
    mktempdir() do dir
        main = joinpath(dir, "main.jl")
        child = joinpath(dir, "child.jl")
        write(child, "struct Included end\nundefined_included\n")
        context = gen_virtual_module()
        res = @test_logs report_text("""
            mapexpr(ex) = error("mapexpr must not run")
            include(mapexpr, "child.jl")
            struct AfterInclude end
            """, main; context, virtualize=false)
        @test isnothing(res.res.toplevel_error_report)
        warning = only(res.res.toplevel_warning_reports)
        @test warning isa JET.UnsupportedFeatureReport
        @test warning.file == main
        @test warning.line == 2
        @test (@invokelatest isdefinedglobal(context, :Included))
        @test (@invokelatest isdefinedglobal(context, :AfterInclude))
        report = only(res.res.inference_error_reports)
        @test is_global_undef_var(report, :undefined_included)
        @test get_reports(res) == [warning, report]
    end
end

@testset "user warnings remain log messages" begin
    res = @test_logs (:warn, "user warning") report_text("@warn \"user warning\"";
        concretization_patterns=[:x_])
    @test isnothing(res.res.toplevel_error_report)
    @test isempty(res.res.toplevel_warning_reports)
    @test isempty(get_reports(res))
end

struct InvalidReadInterpreter{I<:JET.ConcreteInterpreter} <: JET.ConcreteInterpreter
    inner::I
end
JET.InterpretationState(interp::InvalidReadInterpreter) = JET.InterpretationState(interp.inner)
JET.ToplevelAbstractAnalyzer(interp::InvalidReadInterpreter) =
    JET.ToplevelAbstractAnalyzer(interp.inner)
JET.ConcreteInterpreter(interp::InvalidReadInterpreter, state::JET.InterpretationState) =
    InvalidReadInterpreter(JET.ConcreteInterpreter(interp.inner, state))
JET.try_read_file(::InvalidReadInterpreter, ::Module, ::AbstractString) = 42

@testset "internal errors escape analysis" begin
    context = gen_virtual_module()
    states = JET.InterpretationState[]
    interp = InvalidReadInterpreter(
        RecordingInterpreter(JETConcreteInterpreter(JETAnalyzer()), states))
    config = JET.ToplevelConfig(; context, virtualize=false)
    @test_throws JET.ToplevelInternalError JET.virtual_process(interp, """
        @eval begin
            try
                include("child.jl")
            catch
                global caught_internal_error = true
            end
        end
        struct AfterInternalError end
        """, "internal-error.jl", config)
    @test !(@invokelatest isdefinedglobal(context, :caught_internal_error))
    @test !(@invokelatest isdefinedglobal(context, :AfterInternalError))
    @test !isempty(states)
    @test all(state -> isnothing(state.res.toplevel_error_report), states)
    @test all(state -> isempty(state.res.toplevel_warning_reports), states)
    @test all(state -> isempty(state.files_stack), states)
    @test all(state -> isempty(state.caught_callee_errors), states)
    @test_throws JET.ToplevelInternalError JET.with_err_handling(
            JET.general_err_handler, first(states); scrub_offset=1) do
        throw(JET.ToplevelInternalError("invalid interpreter state"))
    end
    @test isnothing(first(states).res.toplevel_error_report)
end

@testset "deprecated top-level error vector" begin
    @testset "$source" for source in ("struct A end", "@eval error(\"failure\")")
        res = report_text(source).res
        report = res.toplevel_error_report
        reports = @test_deprecated r"removed in a future release" res.toplevel_error_reports
        @test reports isa Vector{ToplevelErrorReport}
        @test reports == (report === nothing ? ToplevelErrorReport[] : [report])
        @test :toplevel_error_reports ∉ propertynames(res)
        @test :toplevel_error_reports ∈ propertynames(res, true)
        @test !hasproperty(res, :toplevel_error_reports)
        @test hasproperty(res, :toplevel_error_report)
        empty!(reports)
        @test res.toplevel_error_report === report
    end
end

end # module test_toplevel_errors
