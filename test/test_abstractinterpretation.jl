@testset "inference with virtual global variable" begin
    let
        s = """
        s = "julia"
        sum(s)
        """

        virtualmod = gen_virtualmod()
        res, interp = profile_toplevel!(s, virtualmod)

        @test widenconst(getvirtualglobalvar(interp, virtualmod, :s)) == String
        test_sum_over_string(res)
    end
end
