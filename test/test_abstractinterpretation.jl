@testset "report on union-split method matching" begin
    # if there is no method matching case in union-split, it should be reported
    let
        # NOTE: we can't just wrap them into `let`, closures can't be inferred correctly
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            foo(a::Integer) = :Integer
            foo(a::AbstractString) = "AbstractString"

            $(profile_call)(a->foo(a), Union{Nothing,Int})
        end)

        @test length(interp.reports) === 1
        report = first(interp.reports)
        @test report isa NoMethodErrorReport && report.atype === Tuple{typeof(m.foo), Union{Nothing,Int}}
    end

    # constant propagation should limit false positive union-split no method reports
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            foo(p, i) = p.i = i

            $(profile_call)(foo, P, Int)
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away
        @test isempty(interp.reports)

        # works for cache
        interp, frame = Core.eval(m, :($(profile_call)(foo, P, Int)))
        @test isempty(interp.reports)
    end

    # the false positive reports should be threw away from cache as well
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            foo(p, i) = p.i = i
            bar(args...) = foo(args...)

            $(profile_call)(bar, P, Int)
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away
        @test isempty(interp.reports)

        # works for cache (`abstract_call_gf_by_type` won't run on `foo` this time)
        interp, frame = Core.eval(m, :($(profile_call)(bar, P, Int)))
        @test isempty(interp.reports)
    end

    # false-positive punishing using constant propagation should only be applied those're
    # really revealed to be false positive
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            function foo(p, i, s)
                p.i = i
                p.s = s
            end

            $(profile_call)(foo, P, Int, #= invalid =# Int)
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away, while
        # "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int64)" should be kept
        @test length(interp.reports) === 1
        report = first(interp.reports)
        @test report isa NoMethodErrorReport && report.atype === Tuple{typeof(convert), Type{String}, Int}
    end

    # constant propagation should narrow down union-split no method error to single no method matching error
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            function foo(p, i, s)
                p.i = i
                p.s = s
            end

            $(profile_call)(foo, P, String, Int)
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::String)" should be narrowed down to "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{Int}, v::String)"
        @test !isempty(interp.reports)
        @test any(interp.reports) do report
            return report isa NoMethodErrorReport &&
                report.atype === Tuple{typeof(convert), Type{Int}, String}
        end
        # "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int)"
        # won't be reported since `typeinf` early escapes on `Bottom`-annotated statement
    end
end

@testset "report undefined slots" begin
    let
        interp, frame = profile_call(Bool) do b
            if b
                bar = rand(Int)
                return bar
            end
            return bar # undefined in this pass
        end
        @test length(interp.reports) === 1
        @test first(interp.reports) isa LocalUndefVarErrorReport
        @test first(interp.reports).name === :bar
    end

    # deeper level
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            function foo(b)
                if b
                    bar = rand(Int)
                    return bar
                end
                return bar # undefined in this pass
            end
            baz(a) = foo(a)
            $(profile_call)(baz, Bool)
        end)
        @test length(interp.reports) === 1
        @test first(interp.reports) isa LocalUndefVarErrorReport
        @test first(interp.reports).name === :bar

        # works when cached
        interp, frame = Core.eval(m, :($(profile_call)(baz, Bool)))
        @test length(interp.reports) === 1
        @test first(interp.reports) isa LocalUndefVarErrorReport
        @test first(interp.reports).name === :bar
    end

    # try to exclude false negatives as possible (by collecting reports in after-optimization pass)
    let
        interp, frame = profile_call(Bool) do b
            if b
                bar = rand()
            end

            return if b
                return bar # this shouldn't be reported
            else
                return nothing
            end
        end
        @test isempty(interp.reports)
    end

    let
        interp, frame = profile_call(Bool) do b
            if b
                bar = rand()
            end

            return if b
                return nothing
            else
                # ideally we want to have report for this pass, but tons of work will be
                # needed to report this pass
                return bar
            end
        end
        @test_broken length(interp.reports) === 1 &&
            first(interp.reports) isa LocalUndefVarErrorReport &&
            first(interp.reports).name === :bar
    end
end

@testset "report undefined (global) variables" begin
    let
        interp, frame = profile_call(()->foo)
        @test length(interp.reports) === 1
        @test first(interp.reports) isa GlobalUndefVarErrorReport
        @test first(interp.reports).name === :foo
    end

    # deeper level
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            foo(bar) = bar + baz
            qux(a) = foo(a)
            $(profile_call)(qux, Int)
        end)
        @test length(interp.reports) === 1
        @test first(interp.reports) isa GlobalUndefVarErrorReport
        @test first(interp.reports).name === :baz

        # works when cached
        interp, frame = Core.eval(m, :($(profile_call)(qux, Int)))
        @test length(interp.reports) === 1
        @test first(interp.reports) isa GlobalUndefVarErrorReport
        @test first(interp.reports).name === :baz
    end
end

@testset "report non-boolean condition error" begin
    let
        interp, frame = profile_call(Int) do a
            a ? a : nothing
        end
        @test length(interp.reports) === 1
        er = first(interp.reports)
        @test er isa NonBooleanCondErrorReport
        @test er.t === Int
    end

    let
        interp, frame = profile_call(Any) do a
            a ? a : nothing
        end
        @test isempty(interp.reports)
    end

    let
        interp, frame = profile_call() do
            anyary = Any[1,2,3]
            first(anyary) ? first(anyary) : nothing
        end
        @test isempty(interp.reports) # very untyped, we can't report on this ...
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

    # don't report if there the other crical error exist
    let
        m = gen_virtualmod()
        interp, frame = Core.eval(m, quote
            foo(a) = sum(a) # should be reported
            bar(a) = throw(a) # shouldn't be reported first
            $(profile_call)(Bool, String) do b, s
                b && foo(s)
                bar(s)
            end
        end)
        @test length(interp.reports) === 2
        test_sum_over_string(interp.reports)
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
