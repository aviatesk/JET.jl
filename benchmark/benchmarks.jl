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

# warm up
@report_call identity(nothing)
@report_call __cache_hash__=gensym() identity(nothing)
@report_call mode=:sound __cache_hash__=gensym() identity(nothing)
@report_opt __cache_hash__=gensym() identity(nothing)
show(devnull, @report_call sum("julia"))

# analysis performance
# --------------------

let g = addgroup!(SUITE, "mode=:basic")
    g["identity(nothing)"] = @benchmarkable (@report_call __cache_hash__=gensym() identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_call __cache_hash__=gensym() sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_call sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_call __cache_hash__=gensym() rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_call __cache_hash__=gensym() println(QuoteNode(nothing))) seconds=30.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "mode=:basic top-level")
    g["simple"] = @benchmarkable (@analyze_toplevel __cache_hash__=gensym() begin
        const mysum = sum
        mysum("julia")
    end) seconds=10.0
    g["demo"] = @benchmarkable report_file("demo.jl"; __cache_hash__=gensym(), toplevel_logger=nothing) seconds=60.0
    g["self analysis"] = @benchmarkable JET.analyze_method!(analyzer, m) setup = begin
        analyzer = JET.JETAnalyzer(; __cache_hash__=gensym())
        m = only(methods(JET.virtual_process))
    end seconds=40.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "mode=:sound")
    g["identity(nothing)"] = @benchmarkable (@report_call mode=:sound __cache_hash__=gensym() identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_call mode=:sound __cache_hash__=gensym() sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_call mode=:sound sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_call mode=:sound __cache_hash__=gensym() rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_call mode=:sound __cache_hash__=gensym() println(QuoteNode(nothing))) seconds=30.0
    tune_benchmarks!(g)
end

let g = addgroup!(SUITE, "OptAnalyzer")
    g["identity(nothing)"] = @benchmarkable (@report_opt __cache_hash__=gensym() identity(nothing))
    g["sum(\"julia\")"] = @benchmarkable (@report_opt __cache_hash__=gensym() sum("julia"))
    g["sum(\"julia\") (cached)"] = @benchmarkable (@report_opt sum("julia"))
    g["rand(Bool)"] = @benchmarkable (@report_opt __cache_hash__=gensym() rand(Bool)) seconds=10.0
    g["println(QuoteNode(nothing))"] = @benchmarkable (@report_opt __cache_hash__=gensym() println(QuoteNode(nothing))) seconds=30.0
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
