include("setup.jl")

# benchmark body
# ==============

const SUITE = BenchmarkGroup()
function tune_benchmarks!(g::BenchmarkGroup;
    seconds=nothing,
    gcsample=true)
    for v in values(g)
        if seconds !== nothing
            v.params.seconds = seconds
        end
        v.params.gcsample = gcsample
        v.params.evals = 1 # `setup` must be functional
    end
end

# analysis performance
# --------------------

let g = addgroup!(SUITE, "JETAnalyzer{BasicPass}")
    g["identity(nothing)"] = @benchmarkable (@report_call report_pass=FreshPass(JET.BasicPass()) identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_call report_pass=FreshPass(JET.BasicPass()) sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_call report_pass=JET.BasicPass() sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_call report_pass=FreshPass(JET.BasicPass()) rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_call report_pass=FreshPass(JET.BasicPass()) println(QuoteNode(nothing))) seconds=30.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "JETAnalyzer{BasicPass} top-level")
    g["simple"] = @benchmarkable (@analyze_toplevel report_pass=FreshPass(JET.BasicPass()) begin
        const mysum = sum
        mysum("julia")
    end) seconds=10.0
    g["demo"] = @benchmarkable report_file("demo.jl"; report_pass=FreshPass(JET.BasicPass()), toplevel_logger=nothing) seconds=60.0
    g["self analysis"] = @benchmarkable JET.analyze_method!(analyzer, m) setup = begin
        analyzer = JET.JETAnalyzer(; report_pass=FreshPass(JET.BasicPass()))
        m = only(methods(JET.virtual_process))
    end seconds=40.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "JETAnalyzer{SoundPass}")
    g["identity(nothing)"] = @benchmarkable (@report_call report_pass=FreshPass(JET.SoundPass()) identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_call report_pass=FreshPass(JET.SoundPass()) sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_call report_pass=JET.SoundPass() sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_call report_pass=FreshPass(JET.SoundPass()) rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_call report_pass=FreshPass(JET.SoundPass()) println(QuoteNode(nothing))) seconds=30.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "OptAnalyzer")
    g["identity(nothing)"] = @benchmarkable (@report_opt report_pass=FreshPass(JET.OptAnalysisPass()) identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_opt report_pass=FreshPass(JET.OptAnalysisPass()) sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_opt report_pass=JET.OptAnalysisPass() sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_opt report_pass=FreshPass(JET.OptAnalysisPass()) rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_opt report_pass=FreshPass(JET.OptAnalysisPass()) println(QuoteNode(nothing))) seconds=30.0
    tune_benchmarks!(g)
end

# print performance
# -----------------

let g = addgroup!(SUITE, "show(::IO, ::JETCallResult)")
    g["report_call"] = @benchmarkable show(io, results) setup = (
        io = IOContext(IOBuffer(), :color=>true);
        results = @report_call sum("julia"))
    g["report_opt"] = @benchmarkable show(io, results) setup = (
        io = IOContext(IOBuffer(), :color=>true);
        results = report_opt(Core.Compiler.typeinf, (Core.Compiler.NativeInterpreter, Core.Compiler.InferenceState)))
    tune_benchmarks!(g; seconds=15.0)
end

# first-time performance
# ----------------------

let g = addgroup!(SUITE, "First-Time-To-JET")
    g["package loading"] = @freshbenchmarkable (using JET)
    g["sum(\"julia\")"] = @freshbenchmarkable (@report_call sum("julia")) setup = using JET
    g["rand(Bool)"] = @freshbenchmarkable (@report_call rand(Bool)) setup = using JET
    tune_benchmarks!(g; seconds=60.0)
end
