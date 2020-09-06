@testset "union-split method matching" begin
    # if there is no method matching case in union-split, it should be reported
    let
        # NOTE: we can't just wrap them into `let`, closures can't be inferred correctly
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            foo(a::Integer) = :Integer
            foo(a::AbstractString) = "AbstractString"

            $(profile_call)(a->foo(a), Union{Nothing,Int})
        end)

        @test !isempty(interp.reports)
        @test any(Fix2(isa, NoMethodErrorReport), interp.reports)
    end

    # constant propagation should limit false positive union-split no method reports
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            mutable struct P
                s::String
                i::Int
            end
            foo(p, v) = setproperty!(p, :i, v)

            # we don't want `for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)` report for this
            $(profile_call)(foo, P, Int)
        end)

        @test_broken isempty(interp.reports)
    end
end

@testset "inference with virtual global variable" begin
    let
        s = """
        s = "julia"
        sum(s)
        """

        vmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, vmod)

        @test widenconst(get_virtual_globalvar(interp, vmod, :s)) == String
        test_sum_over_string(res)
    end
end

@testset "report `throw` calls" begin
    # simplest case
    let
        interp, frame = profile_call(()->throw("foo"))
        @test !isempty(interp.reports)
        @test first(interp.reports) isa ExceptionReport
    end

    # throws in deep level
    let
        foo(a) = throw(a)
        interp, frame = profile_call(()->foo("foo"))
        @test !isempty(interp.reports)
        @test first(interp.reports) isa ExceptionReport
    end

    # don't report possibly false negative `throw`s
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        interp, frame = profile_call(foo, Int)
        @test isempty(interp.reports)
    end

    # constant propagation sometimes helps exclude false negatives
    let
        foo(a) = a ≤ 0 ? throw("a is $(a)") : a
        interp, frame = profile_call(()->foo(0))
        @test !isempty(interp.reports)
        @test first(interp.reports) isa ExceptionReport
    end

    # end to end
    let
        # this should report `throw(ArgumentError("Sampler for this object is not defined")`
        interp, frame = profile_call(rand, Char)
        @test !isempty(interp.reports)
        @test first(interp.reports) isa ExceptionReport

        # this should not report `throw(DomainError(x, "sin(x) is only defined for finite x."))`
        interp, frame = profile_call(sin, Int)
        @test isempty(interp.reports)

        # again, constant propagation sometimes can exclude false negatives
        interp, frame = profile_call(()->sin(Inf))
        @test !isempty(interp.reports)
        @test first(interp.reports) isa ExceptionReport
    end
end
