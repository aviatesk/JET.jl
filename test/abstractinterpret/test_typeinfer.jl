@testset "inference with abstract global variable" begin
    let
        vmod = gen_virtual_module()
        res = @analyze_toplevel context = vmod virtualize = false begin
            s = "julia"
            sum(s)
        end

        @test is_concrete(vmod, :s)
        test_sum_over_string(res)
    end

    @testset "union assignment" begin
        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel context = vmod virtualize = false begin
                global globalvar
                if rand(Bool)
                    globalvar = "String"
                else
                    globalvar = :Symbol
                end
            end

            @test is_abstract(vmod, :globalvar)
            @test vmod.globalvar.t === Union{String,Symbol}
        end

        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel context = vmod virtualize = false begin
                if rand(Bool)
                    globalvar = "String"
                else
                    globalvar = :Symbol
                end

                foo(s::AbstractString) = length(s)
                foo(globalvar) # union-split no method matching error should be reported
            end

            @test is_abstract(vmod, :globalvar)
            @test vmod.globalvar.t === Union{String,Symbol}
            @test length(res.inference_error_reports) === 1
            er = first(res.inference_error_reports)
            @test er isa NoMethodErrorReport
            @test isa(er.t, Vector) # should be true
        end

        # sequential
        let
            vmod = gen_virtual_module()
            res = @analyze_toplevel context = vmod virtualize = false begin
                if rand(Bool)
                    globalvar = "String"
                else
                    globalvar = :Symbol
                end

                foo(s::AbstractString) = length(s)
                foo(globalvar) # union-split no method matching error should be reported

                globalvar = 10
                foo(globalvar) # no method matching error should be reported
            end

            @test is_concrete(vmod, :globalvar)
            @test is_analyzed(vmod, :globalvar)
            @test isa_analyzed(vmod.globalvar, Int)
            @test length(res.inference_error_reports) === 2
            let er = first(res.inference_error_reports)
                @test er isa NoMethodErrorReport
                @test isa(er.t, Vector)
            end
            let er = last(res.inference_error_reports)
                @test er isa NoMethodErrorReport
                @test !isa(er.t, Vector)
            end
        end
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

@testset "cache separation from native execution" begin
    # we shouldn't use global code cache for native execution,
    # since it has really not been analyzed by JET

    let
        # # check if the sysimg contains the cache for `getproperty(::Nothing, ::Symbol)`
        # local found = false
        # for mi in only(methods(getproperty, (Nothing,Symbol))).specializations
        #     isnothing(mi) && continue
        #     if mi.specTypes === Tuple{typeof(getproperty),Nothing,Symbol}
        #         found |= true
        #     end
        # end
        # @assert found

        # we can still get error report from that frame
        result = report_call((Nothing,)) do a
            a.field
        end
        @test length(get_reports(result)) === 1
    end
end

@testset "simple invalidation" begin
    let # simple invalidation
        m = Module()

        # analyze the first definition
        @eval m foo(a, b) = (sum(a), b)
        result = @report_call m.foo([1,2,3], "julia")
        @test isempty(get_reports(result))

        # renew the definition and invalidate it
        @eval m foo(a, b) = (a, sum(b))
        result = @report_call m.foo([1,2,3], "julia")
        test_sum_over_string(result)
    end

    let # backedge invalidation
        m = Module()
        @eval m callf(f, args...) = f(args...)

        # analyze the first definition
        @eval m foo(a, b) = (sum(a), b)
        result = @report_call m.callf(m.foo, [1,2,3], "julia")
        @test isempty(get_reports(result))

        # renew the definition and invalidate it
        @eval m foo(a, b) = (a, sum(b))
        result = @report_call m.callf(m.foo, [1,2,3], "julia")
        test_sum_over_string(result)
    end
end

# COMBAK this test is very fragile, think about the alternate tests
# @testset "end to end invalidation" begin
#     # invalidation from deeper call site should still refresh JET analysis
#     let
#         # NOTE: branching on https://github.com/JuliaLang/julia/pull/38830
#         symarg = last(first(methods(Base.show_sym)).sig.parameters) === Symbol ?
#                  :(sym::Symbol) :
#                  :(sym)
#
#         l1, l2, l3 = @freshexec begin
#             # ensure we start with this "errorneous" `show_sym`
#             @eval Base begin
#                 function show_sym(io::IO, $(symarg); allow_macroname=false)
#                     if is_valid_identifier(sym)
#                         print(io, sym)
#                     elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
#                         print(io, '@')
#                         show_sym(io, sym_str[2:end]) # NOTE: `sym_str[2:end]` here is errorneous
#                     else
#                         print(io, "var", repr(string(sym)))
#                     end
#                 end
#             end
#
#             # should have error reported
#             result1 = @report_call println(QuoteNode(nothing))
#
#             # should invoke invalidation in the deeper call site of `println(::QuoteNode)`
#             @eval Base begin
#                 function show_sym(io::IO, $(symarg); allow_macroname=false)
#                     if is_valid_identifier(sym)
#                         print(io, sym)
#                     elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
#                         print(io, '@')
#                         show_sym(io, Symbol(sym_str[2:end]))
#                     else
#                         print(io, "var", repr(string(sym)))
#                     end
#                 end
#             end
#
#             # now we shouldn't have reports
#             result2 = @report_call println(QuoteNode(nothing))
#
#             # again, invoke invalidation
#             @eval Base begin
#                 function show_sym(io::IO, $(symarg); allow_macroname=false)
#                     if is_valid_identifier(sym)
#                         print(io, sym)
#                     elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
#                         print(io, '@')
#                         show_sym(io, sym_str[2:end])
#                     else
#                         print(io, "var", repr(string(sym)))
#                     end
#                 end
#             end
#
#             # now we should have reports, again
#             result3 = @report_call println(QuoteNode(nothing))
#
#             (length ∘ JET.get_reports).((result1, result2, result3)) # return
#         end
#
#         @test l1 > 0
#         @test l2 == 0
#         @test l3 == l1
#     end
# end

@testset "integration with global code cache" begin
    # analysis for `sum(::String)` is already cached, `sum′` and `sum′′` should use it
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            sum′(s) = sum(s)
            sum′′(s) = sum′(s)
            $report_call() do
                sum′′("julia")
            end
        end)
        test_sum_over_string(get_reports(result))
    end

    # incremental setup
    let
        m = gen_virtual_module()

        result = Core.eval(m, quote
            $report_call() do
                sum("julia")
            end
        end)
        test_sum_over_string(get_reports(result))

        result = Core.eval(m, quote
            sum′(s) = sum(s)
            $report_call() do
                sum′("julia")
            end
        end)
        test_sum_over_string(get_reports(result))

        result = Core.eval(m, quote
            sum′′(s) = sum′(s)
            $report_call() do
                sum′′("julia")
            end
        end)
        test_sum_over_string(get_reports(result))
    end

    # should not error for virtual stacktrace traversing with a frame for inner constructor
    # https://github.com/aviatesk/JET.jl/pull/69
    let # FIXME https://github.com/JuliaLang/julia/pull/41885
        res = @analyze_toplevel begin
            struct Foo end
            println(Foo())
        end
        @test isempty(res.inference_error_reports)
    end
end

@testset "integration with local code cache" begin
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            struct Foo{T}
                bar::T
            end
            $report_call((Foo{Int},)) do foo
                foo.baz # typo
            end
        end)

        @test !isempty(get_reports(result))
        @test !isempty(get_cache(result.analyzer))
        @test any(get_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[CC.Const(getproperty),m.Foo{Int},CC.Const(:baz)]
        end
    end

    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            struct Foo{T}
                bar::T
            end
            getter(foo, prop) = getproperty(foo, prop)
            $report_call((Foo{Int}, Bool)) do foo, cond
                getter(foo, :bar)
                cond ? getter(foo, :baz) : getter(foo, :qux) # non-deterministic typos
            end
        end)

        # there should be local cache for each errorneous constant analysis
        @test !isempty(get_reports(result))
        @test !isempty(get_cache(result.analyzer))
        @test any(get_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[CC.Const(m.getter),m.Foo{Int},CC.Const(:baz)]
        end
        @test any(get_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[CC.Const(m.getter),m.Foo{Int},CC.Const(:qux)]
        end
    end
end

@testset "constant analysis" begin
    # constant prop should limit false positive union-split no method reports
    let
        m = @fixturedef begin
            mutable struct P
                i::Int
                s::String
            end
            foo(p, i) = p.i = i
        end

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away
        result = Core.eval(m, :($report_call(foo, (P, Int))))
        @test isempty(get_reports(result))

        # works for cache
        result = Core.eval(m, :($report_call(foo, (P, Int))))
        @test isempty(get_reports(result))
    end

    # more cache test, constant prop should re-run in deeper level
    let
        m = @fixturedef begin
            mutable struct P
                i::Int
                s::String
            end
            foo(p, i) = p.i = i
            bar(args...) = foo(args...)
        end

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away
        result = Core.eval(m, :($report_call(bar, (P, Int))))
        @test isempty(get_reports(result))

        # works for cache
        result = Core.eval(m, :($report_call(bar, (P, Int))))
        @test isempty(get_reports(result))
    end

    # constant prop should not exclude those are not related
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            function foo(p, i, s)
                p.i = i
                p.s = s
            end

            $report_call(foo, (P, Int, #= invalid =# Int))
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)" should be threw away, while
        # "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int64)" should be kept
        @test length(get_reports(result)) === 1
        er = first(get_reports(result))
        @test er isa NoMethodErrorReport
        @test er.t === Tuple{typeof(convert), Type{String}, Int}
    end

    # constant prop should narrow down union-split no method error to single no method matching error
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            mutable struct P
                i::Int
                s::String
            end
            function foo(p, i, s)
                p.i = i
                p.s = s
            end

            $report_call(foo, (P, String, Int))
        end)

        # "for one of the union split cases, no matching method found for signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::String)" should be narrowed down to "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{Int}, v::String)"
        @test !isempty(get_reports(result))
        @test any(get_reports(result)) do report
            report isa NoMethodErrorReport &&
            report.t === Tuple{typeof(convert), Type{Int}, String}
        end
        # "no matching method found for call signature: Base.convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int)"
        # won't be reported since `typeinf` early escapes on `Bottom`-annotated statement
    end

    # report-throw away with constant analysis shouldn't throw away reports from the same
    # frame but with the other constants
    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            foo(a) = a<0 ? a+string(a) : a
            bar() = foo(-1), foo(1) # constant analysis on `foo(1)` shouldn't throw away reports from `foo(-1)`
            $report_call(bar)
        end)
        @test !isempty(get_reports(result))
        @test any(r->isa(r,NoMethodErrorReport), get_reports(result))
    end

    let
        m = gen_virtual_module()
        result = Core.eval(m, quote
            foo(a) = a<0 ? a+string(a) : a
            function bar(b)
                a = b ? foo(-1) : foo(1)
                b = foo(-1)
                return a, b
            end
            $report_call(bar, (Bool,))
        end)
        @test !isempty(get_reports(result))
        # FIXME our report uniquify logic might be wrong and it wrongly singlifies the different reports here
        @test_broken count(isa(report, NoMethodErrorReport) for report in get_reports(result)) == 2
    end

    @testset "constant analysis throws away false positive reports" begin
        let
            m = @fixturedef begin
                foo(a) = a > 0 ? a : "minus"
                bar(a) = foo(a) + 1
            end

            # constant propagation can reveal the error pass can't happen
            result = Core.eval(m, :($report_call(()->bar(10))))
            @test isempty(get_reports(result))

            # for this case, no constant prop' doesn't happen, we can't throw away error pass
            result = Core.eval(m, :($report_call(bar, (Int,))))
            @test length(get_reports(result)) === 1
            er = first(get_reports(result))
            @test er isa NoMethodErrorReport
            @test er.t == [Tuple{typeof(+),String,Int}]

            # if we run constant prop' that leads to the error pass, we should get the reports
            result = Core.eval(m, :($report_call(()->bar(0))))
            @test length(get_reports(result)) === 1
            er = first(get_reports(result))
            @test er isa NoMethodErrorReport
            @test er.t === Tuple{typeof(+),String,Int}
        end

        # we should throw-away reports collected from frames that are revealed as "unreachable"
        # by constant prop'
        let
            m = @fixturedef begin
                foo(a) = bar(a)
                function bar(a)
                    return if a < 1
                        baz1(a, "0")
                    else
                        baz2(a, a)
                    end
                end
                baz1(a, b) = a ? b : b
                baz2(a, b) = a + b
            end

            # no constant prop, just report everything
            result = Core.eval(m, :($report_call(foo, (Int,))))
            @test length(get_reports(result)) === 1
            er = first(get_reports(result))
            @test er isa NonBooleanCondErrorReport &&
                er.t === Int

            # constant prop should throw away the non-boolean condition report from `baz1`
            result = Core.eval(m, quote
                $report_call() do
                    foo(1)
                end
            end)
            @test isempty(get_reports(result))

            # constant prop'ed, still we want to have the non-boolean condition report from `baz1`
            result = Core.eval(m, quote
                $report_call() do
                    foo(0)
                end
            end)
            @test length(get_reports(result)) === 1
            er = first(get_reports(result))
            @test er isa NonBooleanCondErrorReport &&
                er.t === Int

            # so `Bool` is good for `foo` after all
            result = Core.eval(m, :($report_call(foo, (Bool,))))
            @test isempty(get_reports(result))
        end

        # end to end
        let
            res = @analyze_toplevel begin
                function foo(n)
                    if n < 10
                        return n
                    else
                        return "over 10"
                    end
                end

                function bar(n)
                    if n < 10
                        return foo(n) + 1
                    else
                        return foo(n) * "+1"
                    end
                end

                bar(1)
                bar(10)
            end

            @test isempty(res.inference_error_reports)
        end
    end
end

@testset "additional analysis pass for task parallelism code" begin
    # general case with `schedule(::Task)` pattern
    result = report_call() do
        t = Task() do
            sum("julia")
        end
        schedule(t)
        fetch(t)
    end
    test_sum_over_string(get_reports(result))

    # handle `Threads.@spawn` (https://github.com/aviatesk/JET.jl/issues/114)
    result = report_call() do
        fetch(Threads.@spawn 1 + "foo")
    end
    @test length(get_reports(result)) == 1
    let r = first(get_reports(result))
        @test isa(r, NoMethodErrorReport)
        @test r.t === Tuple{typeof(+), Int, String}
    end

    # handle `Threads.@threads`
    result = report_call((Int,)) do n
        a = String[]
        Threads.@threads for i in 1:n
            push!(a, i)
        end
        return a
    end
    @test !isempty(get_reports(result))
    @test any(get_reports(result)) do r
        isa(r, NoMethodErrorReport) &&
        r.t === Tuple{typeof(convert), Type{String}, Int}
    end

    # multiple tasks in the same frame
    result = report_call() do
        t1 = Threads.@spawn 1 + "foo"
        t2 = Threads.@spawn "foo" + 1
        fetch(t1), fetch(t2)
    end
    @test length(get_reports(result)) == 2
    let r = get_reports(result)[1]
        @test isa(r, NoMethodErrorReport)
        @test r.t === Tuple{typeof(+), Int, String}
    end
    let r = get_reports(result)[2]
        @test isa(r, NoMethodErrorReport)
        @test r.t === Tuple{typeof(+), String, Int}
    end

    # nested tasks
    result = report_call() do
        t0 = Task() do
            t = Threads.@spawn sum("julia")
            fetch(t)
        end
        schedule(t0)
        fetch(t0)
    end
    test_sum_over_string(get_reports(result))

    # when `schedule` call is separated from `Task` definition
    make_task(s) = Task() do
        sum(s)
    end
    function run_task(t)
        schedule(t)
        fetch(t)
    end
    result = report_call() do
        t = make_task("julia")

        run_task(t)
    end
    test_sum_over_string(get_reports(result))
    let r = first(get_reports(result))
        # we want report to come from `run_task`, but currently we invoke JET analysis on `Task` construction
        @test_broken any(r.vst) do vf
            vf.linfo.def.name === :run_task
        end
    end

    # report uncaught exception happened in a task
    # TODO currently uncaught exceptions are erased by return type check at caller `Task(::Function)`
    result = report_call() do
        fetch(Threads.@spawn throw("foo"))
    end
    @test_broken length(get_reports(result)) == 1
    @test_broken isa(first(get_reports(result)), UncaughtExceptionReport)

    # don't fail into infinite loop (rather, don't spoil inference termination)
    m = @fixturedef begin
        # adapated from https://julialang.org/blog/2019/07/multithreading/
        import Base.Threads.@spawn

        # sort the elements of `v` in place, from indices `lo` to `hi` inclusive
        function psort!(v, lo::Int=1, hi::Int=length(v))
            if lo >= hi                       # 1 or 0 elements; nothing to do
                return v
            end
            if hi - lo < 100000               # below some cutoff, run in serial
                sort!(view(v, lo:hi), alg = MergeSort)
                return v
            end

            mid = (lo+hi)>>>1                 # find the midpoint

            half = @spawn psort!(v, lo, mid)  # task to sort the lower half; will run
            psort!(v, mid+1, hi)              # in parallel with the current call sorting
                                              # the upper half
            wait(half)                        # wait for the lower half to finish

            temp = v[lo:mid]                  # workspace for merging

            i, k, j = 1, lo, mid+1            # merge the two sorted sub-arrays
            @inbounds while k < j <= hi
                if v[j] < temp[i]
                    v[k] = v[j]
                    j += 1
                else
                    v[k] = temp[i]
                    i += 1
                end
                k += 1
            end
            @inbounds while k < j
                v[k] = temp[i]
                k += 1
                i += 1
            end

            return v
        end
    end
    result = report_call(m.psort!, (Vector{Int},))
    @test true
end

@testset "opaque closure" begin
    # can cache const prop' result with varargs
    function oc_varargs_constprop()
        oc = Base.Experimental.@opaque (args...)->args[1]+args[2]+arg[3] # typo on `arg[3]`
        return Val{oc(1,2,3)}()
    end
    result = @report_call oc_varargs_constprop()
    @test !isempty(get_cache(result.analyzer))
end

@testset "https://github.com/aviatesk/JET.jl/issues/133" begin
    res = @analyze_toplevel begin
        @ccall strlen("foo"::Cstring)::Csize_t
    end
    @test isempty(res.inference_error_reports)
end
