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
        @test isa(r, NoMethodErrorReport)
        @test Any['(', 'a', String, ", ", 'b', String, ')'] ⫇ r.sig._sig
    end

    # nested
    let result = report_call((String,)) do s
            onlystr(onlystr(s))
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("onlystr(onlystr(s))", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
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
        @test occursin("a + b", s)
    end

    # nested
    let result = report_call((Int, Int)) do a, b
            onlystr(a + b)
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("onlystr(a + b)", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("onlystr((a::$Int + b::$Int)::$Int)", s)
    end
end

@testset "getproperty signature" begin
    let result = report_call((Regex,)) do r
            r.nonexist
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("r.nonexist", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("(r::Regex).nonexist", s)
    end

    # nested
    let result = report_call((Regex,)) do r
            Some(r).value.nonexist
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("Some(r).value.nonexist", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("((Some(r::Regex)::Some{Regex}).value::Regex).nonexist", s)
    end
end

@testset "setproperty! signature" begin
    let result = report_call((Base.RefValue{String},)) do r
            r.x = nothing
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("r.x = nothing", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("(r::Base.RefValue{String}).x = nothing", s)
    end
end

@testset "getindex signature" begin
    let result = report_call((String,)) do s
            sum(Ref(s)[])
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("sum(Ref(s)[])", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("sum((Ref(s::String)::Base.RefValue{String})[]::String)", s)
    end

    # nested
    let result = report_call((Regex,)) do r
            sum(Ref(Ref(r))[][])
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("sum(Ref(Ref(r))[][])", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("sum(((Ref(Ref(r::Regex)::Base.RefValue{Regex})::Base.RefValue{Base.RefValue{Regex}})[]::Base.RefValue{Regex})[]::Regex)", s)
    end
end

@testset "setindex! signature" begin
    let result = report_call((Base.RefValue{String},)) do r
            r[] = nothing
        end
        buf = IOBuffer()
        print_reports(buf, get_reports_with_test(result))
        s = String(take!(buf))
        @test occursin("r[] = nothing", s)
        print_reports(buf, get_reports_with_test(result); annotate_types=true)
        s = String(take!(buf))
        @test occursin("(r::Base.RefValue{String})[] = nothing", s)
    end
end

@testset "Core.apply_type signature" begin
    result = report_call() do
        NamedTuple{(:x,:y)}(1,2)
    end

    buf = IOBuffer()
    print_reports(buf, get_reports_with_test(result))
    s = String(take!(buf))
    @test !occursin("Core.apply_type", s)
    @test occursin("NamedTuple{(:x, :y)}", s)
end

@testset ":invoke signature" begin
    m = @fixturedef begin
        foo(s::AbstractString) = throw(ArgumentError(s))
    end
    result = report_call(m.foo, (String,))
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
