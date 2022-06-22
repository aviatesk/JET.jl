# similar to ⊆, but respect the sequence, ignore char vs. string difference
function ⫇(a, b)
    normalize(@nospecialize(x)) = x
    normalize(c::AbstractChar)  = string(c)
    a = normalize.(a)
    b = normalize.(b)

    n = length(a)
    i = 1
    for bi in b
        if a[i] == bi
            i += 1
        else
            i = 1
        end
        i == n && return true
    end
    return false
end

@testset "error location signature" begin
    result = report_call((Char,Char)) do a, b
        a + b
    end
    @test length(get_reports_with_test(result)) == 1
    r = first(get_reports_with_test(result))
    @test isa(r, NoMethodErrorReport)
    @test Any['(', 'a', Char, ", ", 'b', Char, ')'] ⫇ r.sig._sig
end

@testset ":invoke signature" begin
    m = @fixturedef begin
        foo(s::AbstractString) = throw(ArgumentError(s))
    end
    result = report_call(m.foo, (String,))
    @test length(get_reports_with_test(result)) == 1
    r = first(get_reports_with_test(result))
    @test isa(r, UncaughtExceptionReport)
    @test Any['(', 's', String, ')', ArgumentError] ⫇ r.sig._sig
end

sparams1(::Type{T}) where T = zero(T)
sparams21(::Type{A}, ::Type{B}) where A where B = zero(A), zero(B)
sparams22(::Type{A}, ::Type{B}) where B where A = zero(A), zero(B)

@testset "static parameter name" begin
    let result = report_call() do
            sparams1(Char)
        end
        report = only(get_reports_with_test(result))
        @test "T" in report.sig
    end

    let result = report_call() do
            sparams21(Int, Char)
        end
        report = only(get_reports_with_test(result))
        @test "B" in report.sig
    end

    let result = report_call() do
            sparams22(Int, Char)
        end
        report = only(get_reports_with_test(result))
        @test "B" in report.sig
    end
end
