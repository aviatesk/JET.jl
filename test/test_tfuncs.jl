@testset "report invalid builtin call" begin
    result = report_call((Int, Type{Int}, Any)) do a, b, c
        isa(a, b, c)
    end
    @test length(get_reports(result)) === 1
    report = first(get_reports(result))
    @test report isa UnimplementedBuiltinCallErrorReport &&
        widenconst.(report.argtypes) == [Int, Type{Int}, Any]

    @testset "constant propagation" begin
        m = gen_virtual_module()
        Core.eval(m, quote
            struct T
                v
            end
            access_field(t, sym) = getfield(t, sym)
        end)

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(get_reports(result))

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:w), (T,))
        end)
        @test length(get_reports(result)) === 1
        er = first(get_reports(result))
        @test er isa NoFieldErrorReport
        @test er.typ === m.T
        @test er.name === :w

        result = Core.eval(m, quote
            $report_call(t->access_field(t,:v), (T,))
        end)
        @test isempty(get_reports(result))
    end
end

@testset "malformed getfield" begin
    let
        # shouldn't error
        result = report_call((Any,)) do a
            getfield(a)
        end
        @test length(get_reports(result)) == 1
        @test first(get_reports(result)) isa UnimplementedBuiltinCallErrorReport
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
        result = report_call(()->CC.return_type(sum, Tuple{String}))
        @test isempty(get_reports(result))
    end

    # report invalid call of `return_type` itself
    let
        result = report_call(()->CC.return_type(sum))
        @test length(get_reports(result)) == 1
        @test isa(first(get_reports(result)), InvalidReturnTypeCall)
    end

    # end to end
    let
        # this shouldn't report "no matching method found for call signature: Base.iterate(itr::DataType)"
        # , which otherwise will be caught in `abstract_cal` in `return_type_tfunc`
        result = report_call(()->Dict('a' => 1,
                                              :b => 2)
                                     )
        @test isempty(get_reports(result))
    end
end
