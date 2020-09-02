@testset "getfield with virtual global variable" begin
    # nested module access may not be resolved as `GlobalRef` and can be propagated into `getfield`
    let
        s = """
        module foo

        const bar = sum

        module baz

        using ..foo

        foo.bar("julia") # -> NoMethodErrorReports

        end # module bar

        end # module foo
        """

        res, interp = profile_toplevel!(s)

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end
