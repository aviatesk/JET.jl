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
