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
    interp, frame = analyze_call((Char,Char)) do a, b
        a + b
    end
    @test length(interp.reports) == 1
    r = first(interp.reports)
    @test isa(r, NoMethodErrorReport)
    @test Any['(', 'a', Char, ", ", 'b', Char, ')'] ⫇ r.sig
end

@testset ":invoke signature" begin
    m = @fixturedef begin
        foo(s::AbstractString) = throw(ArgumentError(s))
    end
    interp, frame = analyze_call(m.foo, (String,))
    @test length(interp.reports) == 1
    r = first(interp.reports)
    @test isa(r, UncaughtExceptionReport)
    @test Any['(', 's', String, ')', ArgumentError] ⫇ r.sig
end
