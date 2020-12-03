@testset "force inference" begin
    let
        m = gen_virtual_module()

        interp, frame = Core.eval(m, quote
            $(profile_call)() do
                sum("julia")
            end
        end)
        test_sum_over_string(interp)

        interp, frame = Core.eval(m, quote
            sum′(s) = sum(s)
            $(profile_call)() do
                sum′("julia")
            end
        end)
        test_sum_over_string(interp)

        interp, frame = Core.eval(m, quote
            sum′′(s) = sum′(s)
            $(profile_call)() do
                sum′′("julia")
            end
        end)
        test_sum_over_string(interp)
    end

    m = gen_virtual_module()
    interp, frame = Core.eval(m, quote
        sum′(s) = sum(s)
        sum′′(s) = sum′(s)
        $(profile_call)() do
            sum′′("julia")
        end
    end)
    test_sum_over_string(interp)

    # invalidate seemingly erroneous code cache created by native compiler (not by JET profiling)
    interp, frame = profile_call((Nothing,)) do a
        a.field
    end
    @test length(interp.reports) === 1
end
