# imports
# =======

import Test:
    record

# usings
# ======

import Test:
    Test,
    Result, Pass, Fail, Broken, Error,
    get_testset,
    TESTSET_PRINT_ENABLE,
    FallbackTestSet, DefaultTestSet,
    FallbackTestSetException

"""
    @test_call [jetconfigs...] [broken=false] [skip=false] f(args...)

Runs [`@report_call jetconfigs... f(args...)`](@ref @report_call) and tests that the generic
function call `f(args...)` is free from problems that `@report_call` can detect.
If executed inside `@testset`, returns a `Pass` result if it is, a `Fail` result if it
contains any error points detected, or an `Error` result if this macro encounters an
unexpected error. When the test `Fail`s, abstract call stack to each problem location will
also be printed to `stdout`.

```julia
julia> @test_call sincos(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call sincos(10)
```

As with [`@report_call`](@ref), any of [JET configurations](https://aviatesk.github.io/JET.jl/dev/config/)
or analyzer specific configurations can be given as the optional arguments `jetconfigs...` like this:
```julia
julia> cond = false

julia> function f(n)
            if cond           # `cond` is untyped, and will be reported by the sound analysis pass, while JET's default analysis pass will ignore it
                return sin(n)
            else
                return cos(n)
            end
       end;

julia> @test_call f(10)
Test Passed
  Expression: #= none:1 =# JET.@test_call f(10)

julia> @test_call mode=:sound f(10)
JET-test failed at none:1
  Expression: #= none:1 =# JET.@test_call mode = :sound f(10)
  ═════ 1 possible error found ═════
  ┌ @ none:2 goto %4 if not Main.cond
  │ non-boolean (Any) used in boolean context: goto %4 if not Main.cond
  └──────────

ERROR: There was an error during testing
```

`@test_call` is fully integrated with [`Test` standard library's unit-testing infrastructure](https://docs.julialang.org/en/v1/stdlib/Test/).
It means, the result of `@test_call` will be included in the final `@testset` summary,
it supports `skip` and `broken` annotations as like `@test` and its family:
```julia
julia> using JET, Test

julia> f(ref) = isa(ref[], Number) ? sin(ref[]) : nothing;      # Julia can't propagate the type constraint `ref[]::Number` to `sin(ref[])`, JET will report `NoMethodError`

julia> g(ref) = (x = ref[]; isa(x, Number) ? sin(x) : nothing); # we can make it type-stable if we extract `ref[]` into a local variable `x`

julia> @testset "check errors" begin
           ref = Ref{Union{Nothing,Int}}(0)
           @test_call f(ref)             # fail
           @test_call g(ref)             # fail
           @test_call broken=true f(ref) # annotated as broken, thus still "pass"
       end
check errors: JET-test failed at none:3
  Expression: #= none:3 =# JET.@test_call f(ref)
  ═════ 1 possible error found ═════
  ┌ @ none:1 Main.sin(Base.getindex(ref))
  │ for 1 of union split cases, no matching method found for call signatures (Tuple{typeof(sin), Nothing})): Main.sin(Base.getindex(ref::Base.RefValue{Union{Nothing, Int64}})::Union{Nothing, Int64})
  └──────────

Test Summary: | Pass  Fail  Broken  Total
check errors  |    1     1       1      3
ERROR: Some tests did not pass: 1 passed, 1 failed, 0 errored, 1 broken.
```
"""
macro test_call(ex0...)
    ex0 = collect(ex0)

    local broken = nothing
    local skip   = nothing
    idx = Int[]
    for (i,x) in enumerate(ex0)
        if iskwarg(x)
            key, val = x.args
            if key === :broken
                if !isnothing(broken)
                    error("invalid test macro call: cannot set `broken` keyword multiple times")
                end
                broken = esc(val)
                push!(idx, i)
            elseif key === :skip
                if !isnothing(skip)
                    error("invalid test macro call: cannot set `skip` keyword multiple times")
                end
                skip = esc(val)
                push!(idx, i)
            end
        end
    end
    if !isnothing(broken) && !isnothing(skip)
        error("invalid test macro call: cannot set both `skip` and `broken` keywords")
    end
    deleteat!(ex0, idx)

    testres, orig_expr = test_exs(ex0, __module__, __source__)

    return quote
        if $(!isnothing(skip) && skip)
            $record($get_testset(), $Broken(:skipped, $orig_expr))
        else
            testres = $testres
            if $(!isnothing(broken) && broken)
                if isa(testres, $JETTestFailure)
                    testres = $Broken(:test_call, $orig_expr)
                elseif isa(testres, $Pass)
                    testres = $Error(:test_unbroken, $orig_expr, nothing, nothing, $(QuoteNode(__source__)))
                end
            else
                isa(testres, $Pass) || ccall(:jl_breakpoint, $Cvoid, ($Any,), testres)
            end
            $record($get_testset(), testres)
        end
    end
end

get_exceptions() = @static if isdefined(Base, :current_exceptions)
    Base.current_exceptions()
else
    Base.catch_stack()
end
@static if !hasfield(Pass, :source)
    Pass(test_type::Symbol, orig_expr, data, thrown, source) = Pass(test_type, orig_expr, data, thrown)
end

function test_exs(ex0, m, source)
    analysis = gen_call_with_extracted_types_and_kwargs(m, :report_call, ex0)
    orig_expr = QuoteNode(
        Expr(:macrocall, GlobalRef(@__MODULE__, Symbol("@test_call")), source, ex0...))
    source = QuoteNode(source)
    testres = :(try
        result = $analysis
        if $length($get_reports(result.result)) == 0
            $Pass(:test_call, $orig_expr, nothing, nothing, $source)
        else
            $JETTestFailure($orig_expr, $source, result)
        end
    catch err
        isa(err, $InterruptException) && rethrow()
        $Error(:test_error, $orig_expr, err, $get_exceptions(), $source)
    end) |> Base.remove_linenums!
    return testres, orig_expr
end

"""
    test_call(f, types = Tuple{}; broken::Bool = false, skip::Bool = false, jetconfigs...)
    test_call(tt::Type{<:Tuple}; broken::Bool = false, skip::Bool = false, jetconfigs...)

Runs [`report_call(f, types; jetconfigs...`](@ref report_call) and tests that the generic
function call `f(args...)` is free from problems that `report_call` can detect.
Except that it takes a type signature rather than a call expression, this function works
in the same way as [`@test_call`](@ref).
"""
function test_call(@nospecialize(args...);
                   broken::Bool = false, skip::Bool = false,
                   jetconfigs...)
    source = LineNumberNode(@__LINE__, @__FILE__)
    kwargs = map(((k,v),)->Expr(:kw, k, v), collect(jetconfigs))
    orig_expr = :($test_call($(args...); $(kwargs...)))

    if skip
        record(get_testset(), Broken(:skipped, orig_expr))
    else
        testres = try
            result = report_call(args...; jetconfigs...)
            if length(get_reports(result.result)) == 0
                Pass(:test_call, orig_expr, nothing, nothing, source)
            else
                JETTestFailure(orig_expr, source, result)
            end
        catch err
            isa(err, InterruptException) && rethrow()
            Error(:test_error, orig_expr, err, get_exceptions(), source)
        end

        if broken
            if isa(testres, JETTestFailure)
                testres = Broken(:test_call, orig_expr)
            elseif isa(testres, Pass)
                testres = Error(:test_unbroken, orig_expr, nothing, nothing, source)
            end
        else
            isa(testres, Pass) || ccall(:jl_breakpoint, Cvoid, (Any,), testres)
        end
        record(get_testset(), testres)
    end
end

# NOTE we will just show abstract call strack, and won't show backtrace of actual test executions

struct JETTestFailure <: Result
    orig_expr::Expr
    source::LineNumberNode
    result::JETCallResult
end

const TEST_INDENTS = "  "

function Base.show(io::IO, t::JETTestFailure)
    printstyled(io, "JET-test failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    println(io, TEST_INDENTS, "Expression: ", t.orig_expr)
    # print abstract call stack, with appropriate indents
    _, ctx = Base.unwrapcontext(io)
    buf = IOBuffer()
    ioctx = IOContext(buf, ctx)
    show(ioctx, t.result)
    lines = replace(String(take!(buf)), '\n'=>string('\n',TEST_INDENTS))
    print(io, TEST_INDENTS, lines)
end

Base.show(io::IO, ::MIME"application/prs.juno.inline", t::JETTestFailure) =
    return t

function Test.record(::FallbackTestSet, t::JETTestFailure)
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end

function Test.record(ts::DefaultTestSet, t::JETTestFailure)
    if TESTSET_PRINT_ENABLE[]
        printstyled(ts.description, ": ", color=:white)
        print(t)
        println()
    end
    # HACK convert to `Fail` so that test summarization works correctly
    push!(ts.results, Fail(:test_call, t.orig_expr, nothing, nothing, t.source))
    return t
end
