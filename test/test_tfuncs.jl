@testset "report invalid builtin call" begin
    interp, frame = analyze_call((Int, Type{Int}, Any)) do a, b, c
        isa(a, b, c)
    end
    @test length(interp.reports) === 1
    report = first(interp.reports)
    @test report isa InvalidBuiltinCallErrorReport &&
        widenconst.(report.argtypes) == [Int, Type{Int}, Any]

    @testset "constant propagation" begin
        m = gen_virtual_module()
        Core.eval(m, quote
            struct T
                v
            end
            access_field(t, sym) = getfield(t, sym)
        end)

        interp, frame = Core.eval(m, quote
            $analyze_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(interp.reports)

        interp, frame = Core.eval(m, quote
            $analyze_call(t->access_field(t,:w), (T,))
        end)
        @test length(interp.reports) === 1
        er = first(interp.reports)
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w

        interp, frame = Core.eval(m, quote
            $analyze_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(interp.reports)
    end
end

@testset "malformed getfield" begin
    let
        # shouldn't error
        interp, frame = analyze_call((Any,)) do a
            getfield(a)
        end
        @test length(interp.reports) == 1
        @test first(interp.reports) isa InvalidBuiltinCallErrorReport
    end
end

@testset "getfield with abstract global variable" begin
    # nested module access will be resolved as a direct call of `getfield`
    let
        res = @analyze_toplevel begin
            module foo

            const bar = sum

            module baz

            using ..foo

            foo.bar("julia") # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end

    # this should work even if the accessed variable is not constant
    let
        res = @analyze_toplevel begin
            module foo

            bar = sum

            module baz

            using ..foo

            foo.bar("julia") # -> NoMethodErrorReports

            end # module bar

            end # module foo
        end

        @test isempty(res.toplevel_error_reports)
        test_sum_over_string(res)
    end
end

@testset "special case `return_type`" begin
    # don't report invalid method calls simulated in `return_type_tfunc`
    let
        interp, frame = analyze_call(()->CC.return_type(sum, Tuple{String}))
        @test isempty(interp.reports)
    end

    # report invalid call of `return_type` itself
    let
        interp, frame = analyze_call(()->CC.return_type(sum))
        @test !isempty(interp.reports)
        @test any(interp.reports) do report
            return report isa NoMethodErrorReport &&
                any(Base.Fix1(occursin, "return_type"), report.sig)
        end
    end

    # end to end
    let
        # this shouldn't report "no matching method found for call signature: Base.iterate(itr::DataType)"
        # , which otherwise will be caught in `abstract_cal` in `return_type_tfunc`
        interp, frame = analyze_call(()->Dict('a' => 1,
                                              :b => 2)
                                     )
        @test isempty(interp.reports)
    end
end
