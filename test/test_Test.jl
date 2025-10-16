module test_Test

using Test, JET

using Base.Meta: isexpr
using MacroTools: @capture, postwalk

# runs `f()` in an isolated testset, so that it doesn't influence the currently running test suite
function with_isolated_testset(f)
    ts = Test.DefaultTestSet("isolated")
    Test.push_testset(ts)
    try
        mktemp() do path, io
            redirect_stdout(io) do
                f()
            end
        end
    finally
        Test.pop_testset()
    end
    return ts
end

macro addfntest(macro_test)
    func_test = postwalk(macro_test) do x
        pat = :(@test_call(args__))
        if @capture(x, $pat)
            if isexpr(x, :macrocall)
                kwargs = map(x->Expr(:kw, x.args...), x.args[findall(x->isexpr(x,:(=)), x.args)])
                testex = x.args[findfirst(x->isexpr(x,:call), x.args)::Int]
                return :(test_call(Base.typesof($(testex.args...)); $(kwargs...)))
            end
        end
        return x
    end

    return quote
        $macro_test
        $func_test
    end
end

# positive case
goodf(var) = var
@addfntest let
    ts = with_isolated_testset() do
        @test_call goodf(10) # ok
    end
    @test ts.n_passed == 1
end

# negative case
# NOTE the following test is line-sensitive !
badf(var) = undefvar
let
    ts = with_isolated_testset() do
        @test_call badf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Fail)
    @test r.source === LineNumberNode((@__LINE__)-6, @__FILE__)
end

# actual error within `@test_call`
# NOTE the following test is line-sensitive !
let
    ts = with_isolated_testset() do
        @test_call undeff(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Error)
    @test r.source === LineNumberNode((@__LINE__)-6, @__FILE__)
    @test occursin("UndefVarError", r.value)
end

# supports `skip` keyword
@addfntest let
    ts = with_isolated_testset() do
        @test_call skip=true badf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :skipped

    ts = with_isolated_testset() do
        skip = true
        @test_call skip=skip badf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === :skipped
end

# supports `broken` keyword
@addfntest let
    ts = with_isolated_testset() do
        @test_call broken=true badf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === Symbol("@test_call") || r.test_type === :test_call

    ts = with_isolated_testset() do
        broken = true
        @test_call broken=broken badf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
    @test r.test_type === Symbol("@test_call") || r.test_type === :test_call

    ts = with_isolated_testset() do
        @test_call broken=true goodf(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Error)
    @test r.test_type === :test_unbroken
end

# supports JET's configurations
# synced with example used for the `@test_call` docstring
cond = false
function f(n)
    if cond           # `cond` is untyped, and will be reported by the sound analysis pass, while JET's default analysis pass will ignore it
        return sin(n)
    else
        return cos(n)
    end
end
@addfntest let
    ts = with_isolated_testset() do
        @test_call f(10) # ok
    end
    @test ts.n_passed == 1

    ts = with_isolated_testset() do
        @test_call f(10) # still be ok
    end
    @test ts.n_passed == 1

    ts = with_isolated_testset() do
        @test_call mode=:sound f(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Fail)

    ts = with_isolated_testset() do
        @test_call mode=:sound broken=true f(10)
    end
    @test ts.n_passed == 0
    @test length(ts.results) == 1
    r = ts.results[1]
    @test isa(r, Test.Broken)
end

# top-level entries
# https://github.com/aviatesk/JET.jl/issues/490
using Example
let ts = with_isolated_testset() do
        test_package(Example)
    end
    @test ts.n_passed == 1
end
let nonexistinclude = normpath(@__DIR__, "fixtures", "nonexistinclude.jl")
    let ts = with_isolated_testset() do
            test_file(nonexistinclude)
        end
        @test ts.n_passed == 0
        @test only(ts.results) isa Test.Fail
    end
    let ts = with_isolated_testset() do
            test_text(read(nonexistinclude, String), "nonexistinclude.jl")
        end
        @test ts.n_passed == 0
        @test only(ts.results) isa Test.Fail
    end
end

end # module test_Test
