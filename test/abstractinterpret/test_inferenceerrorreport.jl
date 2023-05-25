# TODO set up a dedicated module context for this testset
# NOTE currently this testset relies on the fact that it is run in the `Main` module
# module test_inferenceerrorreport

include("../setup.jl")

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

onlystr(s::String) = length(s)

@testset "signature" begin
    let result = report_call((String,String)) do a, b
            sin(a, b)
        end
        r = only(get_reports_with_test(result))
        @test isa(r, MethodErrorReport)
        @test Any['(', 'a', String, ", ", 'b', String, ')'] ⫇ r.sig._sig
    end

    # nested
    let result = report_call((String,)) do s
            onlystr(onlystr(s))
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("onlystr(onlystr(s::String)::$Int)", s)
    end
end

@testset "binary signature" begin
    let result = report_call((String,String)) do a, b
            a + b
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("a::String + b::String", s)
    end

    # nested
    let result = report_call((Int, Int)) do a, b
            onlystr(a + b)
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("onlystr((a::$Int + b::$Int)::$Int)", s)
    end
end

invoke_error(s::AbstractString) = throw(ArgumentError(s))

@testset ":invoke signature" begin
    result = report_call(invoke_error, (String,))
    r = only(get_reports_with_test(result))
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

# TODO set up a dedicated module context for this testset
# end # module test_inferenceerrorreport
