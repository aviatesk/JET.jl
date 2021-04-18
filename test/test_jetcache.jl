@testset "cache per configuration" begin
    # cache key should be same for the same configurations
    let
        interp1 = JETInterpreter()
        k1 = JET.gen_cache_key(JET.JETAnalysisParams(interp1), CC.InferenceParams(interp1))

        interp2 = JETInterpreter()
        k2 = JET.gen_cache_key(JET.JETAnalysisParams(interp2), CC.InferenceParams(interp2))

        @test k1 == k2
    end

    # cache key should be different for different configurations
    let
        interp1 = JETInterpreter(; max_methods=3)
        k1 = JET.gen_cache_key(JET.JETAnalysisParams(interp1), CC.InferenceParams(interp1))

        interp2 = JETInterpreter(; max_methods=4)
        k2 = JET.gen_cache_key(JET.JETAnalysisParams(interp2), CC.InferenceParams(interp2))

        @test k1 ≠ k2
    end

    # configurations other than `JETAnalysisParams` and `InferenceParams` shouldn't affect
    # the cache key identity
    let
        interp1 = JETInterpreter(; toplevel_logger=nothing)
        k1 = JET.gen_cache_key(JET.JETAnalysisParams(interp1), CC.InferenceParams(interp1))

        interp2 = JETInterpreter(; toplevel_logger=IOBuffer())
        k2 = JET.gen_cache_key(JET.JETAnalysisParams(interp2), CC.InferenceParams(interp2))

        @test k1 == k2
    end

    # end to end test
    let
        m = Module()
        @eval m begin
            foo(a::Val{1}) = 1
            foo(a::Val{2}) = 2
            foo(a::Val{3}) = 3
            foo(a::Val{4}) = undefvar
        end

        # run first analysis and cache
        interp, frame = @eval m $analyze_call((Int,); max_methods=3) do a
            foo(Val(a))
        end
        @test isempty(interp.reports)

        # should use the cached result
        interp, frame = @eval m $analyze_call((Int,); max_methods=3) do a
            foo(Val(a))
        end
        @test isempty(interp.reports)

        # should re-run analysis, and should get a report
        interp, frame = @eval m $analyze_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(interp.reports) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end

        # should run the cached previous result
        interp, frame = @eval m $analyze_call((Int,); max_methods=4) do a
            foo(Val(a))
        end
        @test any(interp.reports) do r
            isa(r, GlobalUndefVarErrorReport) &&
            r.name === :undefvar
        end
    end
end

@testset "invalidate native code cache" begin
    # invalidate native code cache in a system image if it has not been analyzed by JET
    # yes this slows down anlaysis for sure, but otherwise JET will miss obvious errors like below
    let
        interp, frame = analyze_call((Nothing,)) do a
            a.field
        end
        @test length(interp.reports) === 1
    end

    # invalidation from deeper call site can refresh JET analysis
    let
        # NOTE: branching on https://github.com/JuliaLang/julia/pull/38830
        symarg = last(first(methods(Base.show_sym)).sig.parameters) === Symbol ?
                 :(sym::Symbol) :
                 :(sym)

        l1, l2, l3 = @freshexec begin
            # ensure we start with this "errorneous" `show_sym`
            @eval Base begin
                function show_sym(io::IO, $(symarg); allow_macroname=false)
                    if is_valid_identifier(sym)
                        print(io, sym)
                    elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
                        print(io, '@')
                        show_sym(io, sym_str[2:end]) # NOTE: `sym_str[2:end]` here is errorneous
                    else
                        print(io, "var", repr(string(sym)))
                    end
                end
            end

            # should have error reported
            interp1, = @analyze_call println(QuoteNode(nothing))

            # should invoke invalidation in the deeper call site of `println(::QuoteNode)`
            @eval Base begin
                function show_sym(io::IO, $(symarg); allow_macroname=false)
                    if is_valid_identifier(sym)
                        print(io, sym)
                    elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
                        print(io, '@')
                        show_sym(io, Symbol(sym_str[2:end]))
                    else
                        print(io, "var", repr(string(sym)))
                    end
                end
            end

            # now we shouldn't have reports
            interp2, = @analyze_call println(QuoteNode(nothing))

            # again, invoke invalidation
            @eval Base begin
                function show_sym(io::IO, $(symarg); allow_macroname=false)
                    if is_valid_identifier(sym)
                        print(io, sym)
                    elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
                        print(io, '@')
                        show_sym(io, sym_str[2:end])
                    else
                        print(io, "var", repr(string(sym)))
                    end
                end
            end

            # now we should have reports, again
            interp3, = @analyze_call println(QuoteNode(nothing))

            length(interp1.reports), length(interp2.reports), length(interp3.reports) # return
        end

        @test l1 > 0
        @test l2 == 0
        @test l3 == l1
    end
end

@testset "integrate with global code cache" begin
    # analysis for `sum(::String)` is already cached, `sum′` and `sum′′` should use it
    let
        m = gen_virtual_module()
        interp, frame = Core.eval(m, quote
            sum′(s) = sum(s)
            sum′′(s) = sum′(s)
            $analyze_call() do
                sum′′("julia")
            end
        end)
        test_sum_over_string(interp)
    end

    # incremental setup
    let
        m = gen_virtual_module()

        interp, frame = Core.eval(m, quote
            $analyze_call() do
                sum("julia")
            end
        end)
        test_sum_over_string(interp)

        interp, frame = Core.eval(m, quote
            sum′(s) = sum(s)
            $analyze_call() do
                sum′("julia")
            end
        end)
        test_sum_over_string(interp)

        interp, frame = Core.eval(m, quote
            sum′′(s) = sum′(s)
            $analyze_call() do
                sum′′("julia")
            end
        end)
        test_sum_over_string(interp)
    end

    # should not error for virtual stacktrace traversing with a frame for inner constructor
    # https://github.com/aviatesk/JET.jl/pull/69
    let
        res = @analyze_toplevel begin
            struct Foo end
            println(Foo())
        end
        @test isempty(res.inference_error_reports)
    end
end

@testset "integrate with local code cache" begin
    let
        m = gen_virtual_module()
        interp, frame = Core.eval(m, quote
            struct Foo{T}
                bar::T
            end
            $analyze_call((Foo{Int},)) do foo
                foo.baz # typo
            end
        end)

        @test !isempty(interp.reports)
        @test !isempty(interp.cache)
        @test any(interp.cache) do analysis_result
            analysis_result.argtypes==Any[CC.Const(getproperty),m.Foo{Int},CC.Const(:baz)]
        end
    end

    let
        m = gen_virtual_module()
        interp, frame = Core.eval(m, quote
            struct Foo{T}
                bar::T
            end
            getter(foo, prop) = getproperty(foo, prop)
            $analyze_call((Foo{Int}, Bool)) do foo, cond
                getter(foo, :bar)
                cond ? getter(foo, :baz) : getter(foo, :qux) # non-deterministic typos
            end
        end)

        # there should be local cache for each errorneous constant analysis
        @test !isempty(interp.reports)
        @test !isempty(interp.cache)
        @test any(interp.cache) do analysis_result
            analysis_result.argtypes==Any[CC.Const(m.getter),m.Foo{Int},CC.Const(:baz)]
        end
        @test any(interp.cache) do analysis_result
            analysis_result.argtypes==Any[CC.Const(m.getter),m.Foo{Int},CC.Const(:qux)]
        end
    end
end
