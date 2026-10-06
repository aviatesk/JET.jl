module test_typeinfer

include("../setup.jl")

_badgetpropertycall(x) = x.field
badgetpropertycall() = _badgetpropertycall(nothing)

@testset "cache separation from native execution" begin
    # the native execution will generated the cache for `_badgetpropertycall(::Nothing)`
    @test_throws FieldError(Nothing, :field) badgetpropertycall()

    # but we shouldn't use the global code cache for the native execution,
    # and we should still be able to get a report below
    result = @report_call badgetpropertycall()
    @test only(get_reports_with_test(result)) isa BuiltinErrorReport
end

# this call is concrete-eval eligible (`:foldable`) and always throws at runtime,
# while its generic return type stays non-`Bottom` (inference does not fold the loop)
Base.@assume_effects :foldable function always_throwing_foldable()
    for i = 1:3
        i == 3 && return sin("42")
    end
    return nothing
end

@testset "reports survive erroring concretization" begin
    # the report from the generic edge must not be thrown away on the erroring
    # concretization: the constant propagation fallback that would re-derive it is
    # refused here since the argument list carries no constant information
    result = report_call() do
        always_throwing_foldable()
    end
    @test only(get_reports_with_test(result)) isa MethodErrorReport
end

@testset "invalidation" begin; let M = Module()
    # renew a definition and re-analyze it
    @eval M foo(a, b) = (sum(a), b)
    @test isempty(get_reports_with_test(@report_call M.foo([1,2,3], "julia")))
    @eval M foo(a, b) = (a, sum(b))
    test_sum_over_string(@report_call M.foo([1,2,3], "julia"))

    # backedge invalidation
    @eval M callf(f, args...) = f(args...)
    @eval M bar(a, b) = (sum(a), b)
    @test isempty(get_reports_with_test(@report_call M.callf(M.bar, [1,2,3], "julia")))
    @eval M bar(a, b) = (a, sum(b))
    test_sum_over_string(@report_call M.callf(M.foo, [1,2,3], "julia"))

    # `invoke`-backedge invalidation
    @eval M baz(a, b) = sum(a), b
    @eval M qux(a, b) = invoke(baz, Tuple{Any,Any}, a, b)
    @test isempty(get_reports_with_test(@report_call M.qux([1,2,3], "julia")))
    @eval M baz(a, b) = sum(b), a
    test_sum_over_string(@report_call M.qux([1,2,3], "julia"))
end; end

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
#             # ensure we start with this "erroneous" `show_sym`
#             @eval Base begin
#                 function show_sym(io::IO, $(symarg); allow_macroname=false)
#                     if is_valid_identifier(sym)
#                         print(io, sym)
#                     elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
#                         print(io, '@')
#                         show_sym(io, sym_str[2:end]) # NOTE: `sym_str[2:end]` here is erroneous
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
    test_sum_over_string(@report_call sum("julia"))

    # analysis for `sum(::String)` is already cached, `sum′` and `sum′′` should use it
    Core.eval(Module(), quote
        sum′(s) = sum(s)
        sum′′(s) = sum′(s)
        $report_call() do
            sum′′("julia")
        end
    end) |> test_sum_over_string

    # incremental setup
    let m = Module()

        Core.eval(m, quote
            $report_call() do
                sum("julia")
            end
        end) |> test_sum_over_string

        Core.eval(m, quote
            sum′(s) = sum(s)
            $report_call() do
                sum′("julia")
            end
        end) |> test_sum_over_string

        Core.eval(m, quote
            sum′′(s) = sum′(s)
            $report_call() do
                sum′′("julia")
            end
        end) |> test_sum_over_string
    end

    # should not error for virtual stacktrace traversing with a frame for inner constructor
    # https://github.com/aviatesk/JET.jl/pull/69
    let # FIXME https://github.com/JuliaLang/julia/pull/41885
        res = @analyze_toplevel begin
            struct Foo end
            println(Foo())
        end
        @test isnothing(res.res.toplevel_error_report)
        @test isempty(res.res.inference_error_reports)
    end
end

@testset "integration with local code cache" begin
    let m = Module()
        result = Core.eval(m, quote
            struct Foo{T}
                bar::T
            end
            $report_call((Foo{Int},)) do foo
                foo.baz # typo
            end
        end)

        @test !isempty(get_reports_with_test(result))
        @test !isempty(JET.get_inf_cache(result.analyzer))
        @test any(JET.get_inf_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[Const(getproperty),m.Foo{Int},Const(:baz)]
        end
    end

    let m = Module()
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

        # there should be local cache for each erroneous constant analysis
        @test !isempty(get_reports_with_test(result))
        @test any(JET.get_inf_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[Const(m.getter),m.Foo{Int},Const(:baz)]
        end
        @test any(JET.get_inf_cache(result.analyzer)) do analysis_result
            analysis_result.argtypes==Any[Const(m.getter),m.Foo{Int},Const(:qux)]
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

        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)`
        # should be threw away
        result = Core.eval(m, :($report_call(foo, (P, Int))))
        @test isempty(get_reports_with_test(result))

        # works for cache
        result = Core.eval(m, :($report_call(foo, (P, Int))))
        @test isempty(get_reports_with_test(result))
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

        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)`
        # should be threw away
        result = Core.eval(m, :($report_call(bar, (P, Int))))
        @test isempty(get_reports_with_test(result))

        # works for cache
        result = Core.eval(m, :($report_call(bar, (P, Int))))
        @test isempty(get_reports_with_test(result))
    end

    # constant prop should not exclude those are not related
    let result = Core.eval(Module(), quote
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

        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::Int64)`
        # should be threw away, while
        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int64)`
        # should be kept
        @test length(get_reports_with_test(result)) === 1
        er = first(get_reports_with_test(result))
        @test er isa MethodErrorReport
        @test er.t === Tuple{typeof(convert), Type{String}, Int}
    end

    # constant prop should narrow down union-split no method error to single no method matching error
    let result = Core.eval(Module(), quote
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

        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Union{Type{Int64}, Type{String}}, v::String)`
        # should be narrowed down to
        # `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{Int}, v::String)`
        @test !isempty(get_reports_with_test(result))
        @test any(get_reports_with_test(result)) do report
            report isa MethodErrorReport &&
            report.t === Tuple{typeof(convert), Type{Int}, String}
        end
        # NOTE:
        # report for `convert(Base.fieldtype(Base.typeof(x::P)::Type{P}, f::Symbol)::Type{String}, v::Int)`
        # won't be reported since `typeinf` early escapes on `Bottom`-annotated statement
    end

    # report-throw away with constant analysis shouldn't throw away reports from the same
    # frame but with the other constants
    let result = Core.eval(Module(), quote
            foo(a) = a<0 ? a+string(a) : a
            bar() = foo(-1), foo(1) # constant analysis on `foo(1)` shouldn't throw away reports from `foo(-1)`
            $report_call(bar)
        end)
        @test !isempty(get_reports_with_test(result))
        @test any(r->isa(r,MethodErrorReport), get_reports_with_test(result))
    end

    # The `foo(x)` report should survive const-prop filtering for `foo(1)`.
    # Filtering only by `linfo` would discard reports from both callsites.
    let result = Core.eval(Module(), quote
            foo(a) = a < 0 ? a + string(a) : a
            function bar(x)
                r1 = foo(x)
                r2 = foo(1)
                return r1, r2
            end
            $report_call(bar, (Int,))
        end)
        reports = get_reports_with_test(result)
        @test length(reports) === 1
        report = only(reports)
        @test report isa MethodErrorReport
        @test report.t === Tuple{typeof(+), Int, String}
    end

    # FIXME Same-line callsites of the same callee can't be distinguished by the
    # (file, line, linfo) key: const-prop filtering for `foo(1)` still discards
    # the report from `foo(x)` written on the same line.
    let result = Core.eval(Module(), quote
            foo(a) = a < 0 ? a + string(a) : a
            bar(x) = (foo(x), foo(1))
            $report_call(bar, (Int,))
        end)
        @test_broken length(get_reports_with_test(result)) === 1
    end

    let result = Core.eval(Module(), quote
            foo(a) = a<0 ? a+string(a) : a
            function bar(b)
                a = b ? foo(-1) : foo(1)
                b = foo(-1)
                return a, b
            end
            $report_call(bar, (Bool,))
        end)
        @test !isempty(get_reports_with_test(result))
        # FIXME our report uniquify logic might be wrong and it wrongly singlifies the different reports here
        @test_broken count(isa(report, MethodErrorReport) for report in get_reports_with_test(result)) == 2
    end

    @testset "constant analysis throws away false positive reports" begin
        let
            m = @fixturedef begin
                foo(a) = a > 0 ? a : "minus"
                bar(a) = foo(a) + 1
            end

            # constant propagation can reveal the error pass can't happen
            result = Core.eval(m, :($report_call(()->bar(10))))
            @test isempty(get_reports_with_test(result))

            # for this case, no constant prop' doesn't happen, we can't throw away error pass
            result = Core.eval(m, :($report_call(bar, (Int,))))
            @test length(get_reports_with_test(result)) === 1
            er = first(get_reports_with_test(result))
            @test er isa MethodErrorReport
            @test er.t == [Tuple{typeof(+),String,Int}]

            # if we run constant prop' that leads to the error pass, we should get the reports
            result = Core.eval(m, :($report_call(()->bar(0))))
            @test length(get_reports_with_test(result)) === 1
            er = first(get_reports_with_test(result))
            @test er isa MethodErrorReport
            @test er.t === Tuple{typeof(+),String,Int}
        end

        # we should throw-away reports collected from frames that are revealed as "unreachable"
        # by constant prop'
        let m = @fixturedef begin
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
            @test length(get_reports_with_test(result)) === 1
            er = first(get_reports_with_test(result))
            @test er isa NonBooleanCondErrorReport &&
                er.t === Int

            # constant prop should throw away the non-boolean condition report from `baz1`
            result = Core.eval(m, quote
                $report_call() do
                    foo(1)
                end
            end)
            @test isempty(get_reports_with_test(result))

            # constant prop'ed, still we want to have the non-boolean condition report from `baz1`
            result = Core.eval(m, quote
                $report_call() do
                    foo(0)
                end
            end)
            @test length(get_reports_with_test(result)) === 1
            er = first(get_reports_with_test(result))
            @test er isa NonBooleanCondErrorReport &&
                er.t === Int

            # so `Bool` is good for `foo` after all
            result = Core.eval(m, :($report_call(foo, (Bool,))))
            @test isempty(get_reports_with_test(result))
        end

        # end to end
        let res = @analyze_toplevel begin
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

            @test isempty(get_reports_with_test(res))
        end
    end
end

cycle_c1(x::Int) = x > 0 ? cycle_c2(x) : 0
cycle_c2(x::Int) = cycle_c3(x)
cycle_c3(x::Int) = (x > 10 && sin(x, x); cycle_c1(x - 1))
cycle_entry(x) = cycle_c1(x)
cycle_cached_entry(x) = cycle_c1(x)

@inline cycle_h1(x::Int, s::Symbol) = s === :bad ? cycle_h2(x) : 0
cycle_h2(x::Int) = (x > 10 && sin(x, x); cycle_h1(x - 1, :bad))
cycle_constprop_entry(x) = cycle_h1(x, :good)

cycle_p(x::Int) = x > 0 ? cycle_m(x, :good) : 0
@inline cycle_m(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_p(x - 1)) : 0
cycle_inner_constprop_entry(x) = cycle_p(x)

cycle_splat_p(x::Int) = x > 0 ? cycle_splat_m((x, :good)...) : 0
@inline cycle_splat_m(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_splat_p(x - 1)) : 0
cycle_splat_entry(x) = cycle_splat_p(x)

cycle_invoke_p(x::Int) = x > 0 ? invoke(cycle_invoke_m, Tuple{Int,Symbol}, x, :good) : 0
@inline cycle_invoke_m(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_invoke_p(x - 1)) : 0
cycle_invoke_entry(x) = cycle_invoke_p(x)

cycle_equal_p(x::Int, callback) = x > 0 ? cycle_equal_m(x, callback, :good) : callback()
@inline cycle_equal_m(x::Int, callback, s::Symbol) =
    s === :bad ? (x > 10 && sin(x, x); cycle_equal_p(x - 1, callback)) : callback()
cycle_equal_entry(x, callback) = cycle_equal_p(x, callback)

cycle_rt_a(x::Int) = x > 0 ? (cycle_rt_b(x, :good); cycle_rt_c(x)) : 0
@inline cycle_rt_b(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_rt_a(x - 1)) : 0
cycle_rt_c(x::Int) = Core.Compiler.return_type(cycle_rt_b, Tuple{Int,Symbol})
cycle_rt_entry(x) = cycle_rt_a(x)

cycle_q(x::Int) = x > 0 ? cycle_n(x, :good) : 0
@inline cycle_n(x::Int, s::Symbol) = (x > 10 && sin(x, x); s === :bad ? 0 : cycle_q(x - 1))
cycle_bailout_entry(x) = cycle_q(x)

function cycle_r(x::Int)
    s = :good
    while x > 0
        x = cycle_w(x, s)
        s = cycle_sym(x)
    end
    return x
end
@inline cycle_w(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_r(x - 1)) : x - 1
cycle_sym(x::Int) = x > 5 ? :good : :bad
cycle_revisit_entry(x) = cycle_r(x)

function cycle_sites_p(x::Int)
    x > 0 || return 0
    cycle_sites_m(x, :good)
    return cycle_sites_m(x, :bad)
end
const CYCLE_SITES_BAD_LINE = (@__LINE__) - 2
@inline cycle_sites_m(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_sites_p(x - 1)) : 0
cycle_sites_entry(x) = cycle_sites_p(x)

cycle_later_p(x::Int, s::Symbol) = x > 0 ? (cycle_later_m(x, :good); cycle_later_k(x, s)) : 0
@inline cycle_later_m(x::Int, s::Symbol) = s === :bad ? (x > 10 && sin(x, x); cycle_later_p(x - 1, s)) : 0
cycle_later_k(x::Int, s::Symbol) = cycle_later_m(x, s)
cycle_later_entry(x, s) = cycle_later_p(x, s)

cycle_upper_p(x::Int) = x > 0 ? (cycle_upper_k(x, :good); cycle_upper_m(x)) : 0
@inline cycle_upper_k(x::Int, s::Symbol) = s === :bad ? cycle_upper_m(x) : 0
cycle_upper_m(x::Int) = (x > 10 && sin(x, x); cycle_upper_p(x - 1))
cycle_upper_entry(x) = cycle_upper_p(x)

cycle_t1(x::Int) = x > 0 ? cycle_t2(x) : 0
cycle_t2(x::Int) = x > 10 ? throw(ArgumentError("x")) : cycle_t1(x - 1)
cycle_throw_entry(x) = cycle_t1(x)

function cycle_opt1(x::Int, y::Base.RefValue{Any})
    x > 0 || return 0
    z = cycle_optinl(x)
    return cycle_opt2(z, y)
end
const CYCLE_OPT1_CALLSITE_LINE = (@__LINE__) - 2
@inline cycle_optinl(x) = x + 1
cycle_opt2(x::Int, y::Base.RefValue{Any}) = (y[] + 1; cycle_opt1(x - 2, y))
cycle_opt_entry(x, y) = cycle_opt1(x, y)

cycle_reuse1_a(x::Int) = x > 0 ? (cycle_reuse1_b(x); cycle_reuse1_c(x)) : 0
cycle_reuse1_b(x::Int) = cycle_reuse1_c(x)
cycle_reuse1_c(x::Int) = (x > 10 && sin(x, x); cycle_reuse1_a(x - 1))
cycle_reuse1_entry_a(x) = cycle_reuse1_a(x)
cycle_reuse1_entry_b(x) = cycle_reuse1_b(x)

cycle_reuse2_a(x::Int) = x > 0 ? (x > 10 && sin(x, x); cycle_reuse2_b(x)) : 0
cycle_reuse2_b(x::Int) = cycle_reuse2_c(x)
cycle_reuse2_c(x::Int) = cycle_reuse2_a(x - 1)
cycle_reuse2_entry_a(x) = cycle_reuse2_a(x)
cycle_reuse2_entry_b(x) = cycle_reuse2_b(x)

@testset "reports within call cycles" begin
    # reports of a cycle member are attributed to the call site that entered it,
    # not to the caller of the whole cycle
    let result = report_call(cycle_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_entry, :cycle_c1, :cycle_c2, :cycle_c3]
    end

    # the cycle top caches the reports from the whole cycle
    let result = report_call(cycle_cached_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_cached_entry, :cycle_c1, :cycle_c2, :cycle_c3]
    end

    # constant propagation throws away the reports from the non-constant cycle
    let result = report_call(cycle_constprop_entry, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    # the same holds when constant propagation happens inside the cycle
    let result = report_call(cycle_inner_constprop_entry, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    let result = report_call(cycle_splat_entry, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    let result = report_call(cycle_invoke_entry, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    # The unknown callback keeps return/exception types and effects unchanged, but
    # Compiler still retains the constant-propagation result that removes the error.
    let result = report_call(cycle_equal_entry, (Int, Any))
        @test isempty(get_reports_with_test(result))
    end

    # the reports don't reach the cycle top through the simulated call of `return_type`
    let result = report_call(cycle_rt_entry, (Int,))
        @test isempty(get_reports_with_test(result))
    end

    # but constant propagation that hits the cycle bails out, and the reports from the
    # non-constant result are kept
    let result = report_call(cycle_bailout_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_bailout_entry, :cycle_q, :cycle_n]
    end

    # the reports are kept as well when the cycle revisits the call site with arguments
    # that are no longer constant, since the latest evaluation uses the non-constant result
    let result = report_call(cycle_revisit_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_revisit_entry, :cycle_r, :cycle_w]
    end

    # constant propagation at one call site does not supersede the reports for another call
    # site that uses the non-constant result
    let result = report_call(cycle_sites_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_sites_entry, :cycle_sites_p, :cycle_sites_m]
        @test r.vst[2].line == CYCLE_SITES_BAD_LINE
    end

    # the reports are handed over through a call site in a cycle member that is entered after
    # the callee
    let result = report_call(cycle_later_entry, (Int, Symbol))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_later_entry, :cycle_later_p, :cycle_later_k, :cycle_later_m]
    end

    # the reports are handed over through another call site when constant propagation
    # supersedes them on the way to the cycle top
    let result = report_call(cycle_upper_entry, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_upper_entry, :cycle_upper_p, :cycle_upper_m]
    end

    # reports that analyzers add in their `CC.finish!` overloads are handed over as well
    let result = report_call(cycle_throw_entry, (Int,); mode=:sound)
        r = only(get_reports_with_test(result))
        @test r isa UncaughtExceptionReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_throw_entry, :cycle_t1, :cycle_t2]
    end

    # the call sites are resolved before analyzers that optimize transform the sources
    let result = report_opt(cycle_opt_entry, (Int, Base.RefValue{Any}))
        r = only(get_reports_with_test(result))
        @test r isa RuntimeDispatchReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_opt_entry, :cycle_opt1, :cycle_opt2]
        @test r.vst[2].line == CYCLE_OPT1_CALLSITE_LINE
    end

    # the cycle members are not cached, so a later analysis entering the cycle through a
    # member infers it again and gets the reports reachable from there
    let result = report_call(cycle_reuse1_entry_a, (Int,))
        @test only(get_reports_with_test(result)) isa MethodErrorReport
    end
    let result = report_call(cycle_reuse1_entry_b, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_reuse1_entry_b, :cycle_reuse1_b, :cycle_reuse1_c]
    end

    # including the reports reachable only through the call back into the cycle top
    let result = report_call(cycle_reuse2_entry_a, (Int,))
        @test only(get_reports_with_test(result)) isa MethodErrorReport
    end
    let result = report_call(cycle_reuse2_entry_b, (Int,))
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test [vf.linfo.def.name for vf in r.vst] == [:cycle_reuse2_entry_b, :cycle_reuse2_b, :cycle_reuse2_c, :cycle_reuse2_a]
    end
end

mutable struct ConstPropCycleOnce{F}
    const f::F
    x::Int
end
function (once::ConstPropCycleOnce)()
    once.x == 0 && (once.x = once.f())
    return once.x
end
constprop_cycle_wait() = CONSTPROP_CYCLE_ONCE()
constprop_cycle_entry() = constprop_cycle_wait()
const CONSTPROP_CYCLE_ONCE = ConstPropCycleOnce(constprop_cycle_wait, 0)

mutable struct ConstPropCycleErrorOnce{F}
    const f::F
    x::Int
end
function (once::ConstPropCycleErrorOnce)()
    once.x == 0 && (once.x = once.f())
    once.x > 10 && sin(once.x, once.x)
    return once.x
end
constprop_cycle_error_wait() = CONSTPROP_CYCLE_ERROR_ONCE()
constprop_cycle_error_entry() = constprop_cycle_error_wait()
const CONSTPROP_CYCLE_ERROR_ONCE = ConstPropCycleErrorOnce(constprop_cycle_error_wait, 0)

constprop_cycle_cached_wait() = CONSTPROP_CYCLE_CACHED_ONCE()
constprop_cycle_cached_entry() = constprop_cycle_cached_wait()
const CONSTPROP_CYCLE_CACHED_ONCE = ConstPropCycleErrorOnce(constprop_cycle_cached_wait, 0)

# aviatesk/JET.jl#868
@testset "reports from constant propagation discarded by call cycles" begin
    let result = report_opt(constprop_cycle_entry, ())
        @test isempty(get_reports_with_test(result))
    end

    let result = report_call(constprop_cycle_error_entry, ())
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test r.t === Tuple{typeof(sin), Int, Int}
    end

    # A cached generic report must survive a failed constant-propagation attempt,
    # rather than being replaced by a leaked report with the wrong call stack.
    let result = report_call(CONSTPROP_CYCLE_CACHED_ONCE, ())
        @test only(get_reports_with_test(result)) isa MethodErrorReport
    end
    let result = report_call(constprop_cycle_cached_entry, ())
        r = only(get_reports_with_test(result))
        @test r isa MethodErrorReport
        @test r.t === Tuple{typeof(sin), Int, Int}
        @test length(r.vst) == 3
        @test r.vst[2].linfo.def.name === :constprop_cycle_cached_wait
    end
end

@testset "additional analysis pass for task parallelism code" begin
    # general case with `schedule(::Task)` pattern
    report_call() do
        t = Task() do
            sum("julia")
        end
        schedule(t)
        fetch(t)
    end |> test_sum_over_string

    # handle `Threads.@spawn` (https://github.com/aviatesk/JET.jl/issues/114)
    result = report_call() do
        fetch(Threads.@spawn 1 + "foo")
    end
    let r = only(get_reports_with_test(result))
        @test isa(r, MethodErrorReport)
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
    @test !isempty(get_reports_with_test(result))
    @test any(get_reports_with_test(result)) do r
        isa(r, MethodErrorReport) &&
        r.t === Tuple{typeof(convert), Type{String}, Int}
    end

    # multiple tasks in the same frame
    result = report_call() do
        t1 = Threads.@spawn 1 + "foo"
        t2 = Threads.@spawn "foo" + 1
        fetch(t1), fetch(t2)
    end
    @test length(get_reports_with_test(result)) == 2
    let r = get_reports_with_test(result)[1]
        @test isa(r, MethodErrorReport)
        @test r.t === Tuple{typeof(+), Int, String}
    end
    let r = get_reports_with_test(result)[2]
        @test isa(r, MethodErrorReport)
        @test r.t === Tuple{typeof(+), String, Int}
    end

    # nested tasks
    report_call() do
        t0 = Task() do
            t = Threads.@spawn sum("julia")
            fetch(t)
        end
        schedule(t0)
        fetch(t0)
    end |> test_sum_over_string

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
    test_sum_over_string(result)
    let r = first(get_reports_with_test(result))
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
    @test_broken length(get_reports_with_test(result)) == 1
    @test_broken isa(first(get_reports_with_test(result)), UncaughtExceptionReport)

    # don't fail into infinite loop (rather, don't spoil inference termination)
    m = @fixturedef begin
        # adapted from https://julialang.org/blog/2019/07/multithreading/
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
    @test !isempty(JET.get_inf_cache(result.analyzer))
end

# FIXME Remove `virtualize=false`` A bug within `resolve_toplevel_symbols!`
# UndefVarError: `Csize_t` not defined in `Main.var"##JETVirtualModule#340"`
@testset "https://github.com/aviatesk/JET.jl/issues/133" begin
    res = @analyze_toplevel virtualize=false begin
        @ccall strlen("foo"::Cstring)::Csize_t
    end
    @test isempty(get_reports_with_test(res))
end

# filter_lineages! for concrete_eval_call
Base.@assume_effects :foldable function filter_unopt_call(call::Bool, f, args...)
    if call
        f(args...)
    else
        typocall(args...) # can't be optimized
    end
end
let res = report_opt() do
        filter_unopt_call(true, sin, 42)
    end
    @test isempty(get_reports_with_test(res))
end

module BasicFiltering
    function foo(a)
        r1 = sum(a)
        r2 = undefsum(a)
        return r1, r2
    end
end

module SubmoduleFiltering
    module SubMod
        function sub_func(x::Int)
            return undefined_var
        end
    end

    function parent_func(x::String)
        return SubMod.sub_func(parse(Int, x))
    end
end

@testset "report filtering" begin
    @testset "basic filtering" begin
        let result = @report_call BasicFiltering.foo("julia")
            test_sum_over_string(result)
            @test any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
        end

        let result = @report_call target_modules=(BasicFiltering,) BasicFiltering.foo("julia")
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :undefsum)
        end

        let result = @report_call target_modules=(AnyFrameModule(BasicFiltering),) BasicFiltering.foo("julia")
            test_sum_over_string(result)
            @test any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
        end

        let result = @report_call ignored_modules=(Base,) BasicFiltering.foo("julia")
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :undefsum)
        end

        let result = @report_call ignored_modules=(BasicFiltering,) BasicFiltering.foo("julia")
            test_sum_over_string(result)
            @test !any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
        end
    end

    @testset "submodule filtering" begin
        let result = @report_call SubmoduleFiltering.parent_func("not_a_number")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
        end

        let result = @report_call target_modules=(SubmoduleFiltering,) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        let result = @report_call target_modules=(LastFrameModuleExact(SubmoduleFiltering),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test isempty(reports)
        end

        let result = @report_call target_modules=(LastFrameModuleExact(SubmoduleFiltering.SubMod),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        let result = @report_call target_modules=(AnyFrameModule(SubmoduleFiltering.SubMod),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        let result = @report_call target_modules=(AnyFrameModuleExact(SubmoduleFiltering),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
        end

        let result = @report_call ignored_modules=(SubmoduleFiltering,) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test isempty(reports)
        end

        let result = @report_call ignored_modules=(LastFrameModuleExact(SubmoduleFiltering),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end
    end

    @testset "namespace root filtering" begin
        # `target_modules = (Main,)` should cover interactively-defined code
        # without letting reports from `Base` through
        m = Core.eval(Main, :(module $(gensym(:NamespaceRootFiltering))
            foo(a) = (sum(a), undefsum(a))
        end))
        let result = report_call(m.foo, (String,); target_modules=(Main,))
            r = only(get_reports_with_test(result))
            @test is_global_undef_var(r, :undefsum)
        end
        let result = report_call(m.foo, (String,); ignored_modules=(Main,))
            test_sum_over_string(result)
            @test !any(r->is_global_undef_var(r, :undefsum), get_reports_with_test(result))
        end
    end

    @testset "Symbol-based filtering" begin
        let result = @report_call target_modules=(:BasicFiltering,) BasicFiltering.foo([1,2,3])
            reports = get_reports_with_test(result)
            @test length(reports) == 1
            @test any(r->is_global_undef_var(r, :undefsum), reports)
        end

        let result = @report_call ignored_modules=(:Base,) BasicFiltering.foo([1,2,3])
            reports = get_reports_with_test(result)
            @test length(reports) == 1
            @test any(r->is_global_undef_var(r, :undefsum), reports)
        end

        let result = @report_call target_modules=(LastFrameModule(:SubmoduleFiltering),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        let result = @report_call target_modules=(LastFrameModuleExact(:SubMod),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test length(reports) == 1
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        let result = @report_call ignored_modules=(LastFrameModuleExact(:SubmoduleFiltering),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end
    end

    @testset "method-based filtering" begin
        # LastFrameMethod with Symbol: last frame is `sub_func`
        let result = @report_call target_modules=(LastFrameMethod(:sub_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        # LastFrameMethod with Symbol: last frame is NOT `parent_func`
        let result = @report_call target_modules=(LastFrameMethod(:parent_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test isempty(reports)
        end

        # AnyFrameMethod with Symbol: `parent_func` is in the stack
        let result = @report_call target_modules=(AnyFrameMethod(:parent_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        # AnyFrameMethod with Symbol: `sub_func` is also in the stack
        let result = @report_call target_modules=(AnyFrameMethod(:sub_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        # LastFrameMethod with Function
        let result = @report_call target_modules=(LastFrameMethod(SubmoduleFiltering.SubMod.sub_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        # AnyFrameMethod with Function
        let result = @report_call target_modules=(AnyFrameMethod(SubmoduleFiltering.parent_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test !isempty(reports)
            @test any(r->is_global_undef_var(r, :undefined_var), reports)
        end

        # ignored_modules with LastFrameMethod
        let result = @report_call ignored_modules=(LastFrameMethod(:sub_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test isempty(reports)
        end

        # ignored_modules with AnyFrameMethod
        let result = @report_call ignored_modules=(AnyFrameMethod(:parent_func),) SubmoduleFiltering.parent_func("42")
            reports = get_reports_with_test(result)
            @test isempty(reports)
        end
    end
end

end # module test_typeinfer
