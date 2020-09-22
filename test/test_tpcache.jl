@testset "force inference" begin
    let
        m = gen_virtualmod()

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

    m = gen_virtualmod()
    interp, frame = Core.eval(m, quote
        sum′(s) = sum(s)
        sum′′(s) = sum′(s)
        $(profile_call)() do
            sum′′("julia")
        end
    end)
    test_sum_over_string(interp)
end
