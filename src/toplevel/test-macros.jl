const TEST_MACRO_NAMES = Set{Symbol}((
    Symbol("@test"),
    Symbol("@test_broken"),
    Symbol("@test_skip"),
    Symbol("@test_throws"),
    Symbol("@test_logs"),
    Symbol("@test_warn"),
    Symbol("@test_nowarn"),
    Symbol("@test_deprecated"),
    Symbol("@inferred"),
    Symbol("@testset"),
))

function test_macro_name(mod::Module, @nospecialize(mac))
    if mac isa GlobalRef
        mac.mod === Test || return nothing
        return mac.name
    elseif mac isa Symbol
        @invokelatest(isdefinedglobal(mod, mac)) || return nothing
        isdefinedglobal(Test, mac) || return nothing
        @invokelatest(getglobal(mod, mac)) === getglobal(Test, mac) || return nothing
        return mac
    elseif isexpr(mac, :., 2)
        modname, name = mac.args
        name isa QuoteNode || return nothing
        name = name.value
        name isa Symbol || return nothing
        modname isa Symbol || return nothing
        resolve_latest_global(GlobalRef(mod, modname)) === Test || return nothing
        return name
    end
    return nothing
end

# Prevent the synthetic result of a Test macro replacement from affecting the
# surrounding inference result while preserving its runtime value.
hide_test_replacement_value(@nospecialize(value)) =
    Expr(:call, GlobalRef(Base, :inferencebarrier), value)

function is_anonymous_test_function(@nospecialize(ex))
    isexpr(ex, :->, 2) && return true
    isexpr(ex, :function, 2) || return false
    sig = first(ex.args)
    while isexpr(sig, (:(::), :where)) && !isempty(sig.args)
        sig = first(sig.args)
    end
    return isexpr(sig, :tuple)
end

# Calls and access syntax preserve their operands through ordinary closure capture.
const THUNK_SAFE_TEST_CALL_HEADS = (:call, :ref, :., :parameters, :kw, :...)

# These heads do not introduce bindings by themselves and are safe when all children are safe.
const THUNK_SAFE_TEST_CONTROL_HEADS = (:block, :&&, :||, :if, :comparison)

# Literal construction does not observe or modify the enclosing lexical scope.
const THUNK_SAFE_TEST_LITERAL_HEADS = (
    :tuple, :vect, :vcat, :hcat, :ncat, :typed_vcat, :typed_hcat, :typed_ncat,
    :row, :nrow, :string,
)

# `where` binds type variables only within its type expression; these heads do not define
# names in the surrounding scope.
const THUNK_SAFE_TEST_TYPE_HEADS = (:curly, :(::), :where)

# Introducing a function scope is only safe for expression forms whose enclosing-scope
# behavior is implemented by closure capture. In particular, bindings, named definitions,
# control transfers, and unknown macro syntax must remain inline, so this is intentionally a
# positive list of reads, calls, expression-level control flow, and anonymous functions.
function is_test_expression_thunk_safe(@nospecialize(ex))
    ex isa Expr || return true
    isexpr(ex, (:quote, :inert)) && return true
    is_anonymous_test_function(ex) && return true
    if isexpr(ex, :do, 2)
        call, closure = ex.args
        return is_test_expression_thunk_safe(call) &&
            is_anonymous_test_function(closure)
    end
    (ex.head ∈ THUNK_SAFE_TEST_CALL_HEADS ||
     ex.head ∈ THUNK_SAFE_TEST_CONTROL_HEADS ||
     ex.head ∈ THUNK_SAFE_TEST_LITERAL_HEADS ||
     ex.head ∈ THUNK_SAFE_TEST_TYPE_HEADS) || return false
    return all(is_test_expression_thunk_safe, ex.args)
end

# Unconditional anonymous functions do not force unrelated control flow to be concretized,
# so only detect definitions nested under short-circuit or branch semantics.
function has_conditionally_defined_test_function(
        @nospecialize(ex), conditional::Bool = false)
    ex isa Expr || return false
    isexpr(ex, (:quote, :inert)) && return false
    is_anonymous_test_function(ex) && return conditional
    if isexpr(ex, :do, 2)
        call, closure = ex.args
        return has_conditionally_defined_test_function(call, conditional) ||
            conditional && is_anonymous_test_function(closure)
    elseif isexpr(ex, (:&&, :||), 2)
        return has_conditionally_defined_test_function(first(ex.args), conditional) ||
            has_conditionally_defined_test_function(last(ex.args), true)
    elseif isexpr(ex, :if)
        isempty(ex.args) && return false
        has_conditionally_defined_test_function(first(ex.args), conditional) && return true
        return any(ex.args[2:end]) do @nospecialize(branch)
            has_conditionally_defined_test_function(branch, true)
        end
    elseif isexpr(ex, :comparison)
        for (i, arg) in enumerate(ex.args)
            has_conditionally_defined_test_function(arg, conditional || i ≥ 5) &&
                return true
        end
        return false
    end
    return any(arg -> has_conditionally_defined_test_function(arg, conditional), ex.args)
end

# `conditionally_evaluated` accounts for control flow introduced by the replacement itself,
# such as the `if skip` surrounding the tested expression for `@test skip=...`.
function should_wrap_test_expression_in_thunk(
        @nospecialize(ex); conditionally_evaluated::Bool = false)
    return is_test_expression_thunk_safe(ex) &&
        has_conditionally_defined_test_function(ex, conditionally_evaluated)
end

# The replacement templates use `@q` instead of `Base.remove_linenums!`, which would also
# strip the line numbers of the interpolated user code and make reports in it point to the
# line of the top-level statement. `@q` strips only the template's own line numbers.
function wrap_test_expression_in_thunk(@nospecialize(ex))
    thunk = gensym(:test_expression)
    return @q begin
        let $thunk = () -> $ex
            $thunk()
        end
    end
end

"""
    maybe_wrap_test_expression_in_thunk(ex)

Outline the test expression `ex` into a zero-argument closure if it defines an anonymous
function in a conditional branch. For example, `isnothing(x) || all(y -> y > 0, x)` becomes:
```julia
let thunk = () -> isnothing(x) || all(y -> y > 0, x)
    thunk()
end
```

Virtual processing concretizes top-level closure definitions so that later inference can
use their methods. When a closure is defined in a conditional branch of a test expression,
realizing that definition also selects the preceding condition for concrete execution.
Outlining the test expression into a zero-argument function instead leaves its nested
closures inside an ordinary method body, where inference can analyze the control flow
without concretely executing the condition.
"""
function maybe_wrap_test_expression_in_thunk(@nospecialize(ex))
    should_wrap_test_expression_in_thunk(ex) || return ex
    return wrap_test_expression_in_thunk(ex)
end

function discard_test_expression_value(@nospecialize(ex))
    return @q begin
        try
            $ex
        catch
        end
        $(hide_test_replacement_value(nothing))
    end
end

function preserve_test_expression_value(@nospecialize(exs...))
    value = gensym(:test_value)
    return @q begin
        let $value = nothing
            try
                $(exs[1:end-1]...)
                $value = $(last(exs))
            catch
            end
            $(hide_test_replacement_value(value))
        end
    end
end

function scope_test_expression(@nospecialize(exs...))
    return @q begin
        let
            $(exs...)
        end
    end
end

const TEST_THROWS_ACCEPTS_PATTERN =
    hasmethod(Test.var"@test_throws", Tuple{LineNumberNode,Module,Any,Any,Any})

"""
    check_test_value(value)

Do nothing when executed. `JETAnalyzer` reports a call to this function in top-level code
as a test that always fails if `value`, the value of a test expression, is inferred to be
`false` or of a non-boolean type (see `FailingTestReport`).
"""
check_test_value(@nospecialize(_)) = nothing

checked_test_value(@nospecialize(value)) =
    Expr(:call, GlobalRef(@__MODULE__, :check_test_value), value)

"""
    enter_test_exception()

Do nothing when executed. The replacement of a `@test_throws` call evaluates its test
expression in a `try` block that starts with a call to this function, which marks the
statements in the block as expected to throw (see `is_test_exception_statement`).
"""
enter_test_exception() = nothing

struct NoTestException end

"""
    NO_TEST_EXCEPTION

The value that the replacement of a `@test_throws` call passes to `check_test_exception`
when its test expression does not throw.
"""
const NO_TEST_EXCEPTION = NoTestException()

"""
    check_test_exception(exception, extype, [pattern])

Do nothing when executed. The replacement of a `@test_throws` call passes `exception`, the
exception caught from its test expression or `NO_TEST_EXCEPTION`, to this function, along
with `extype` and `pattern`, the expected exception and the message pattern of the call.
`JETAnalyzer` reports a call to this function in top-level code as a test that always fails
if `exception` is inferred to be only of exception types that do not match `extype` (see
`FailingTestReport`).
"""
check_test_exception(@nospecialize(exception), @nospecialize(extype),
                     @nospecialize(pattern...)) = nothing

"""
    is_test_exception_statement(frame::InferenceState, pc::Int) -> Bool

Return whether the statement at `pc` in `frame` evaluates the test expression of a
`@test_throws` call, i.e. whether it is in a `try` block that starts with a call to
`enter_test_exception`. Since the test expression is expected to throw, the reports of
`frame` whose first `VirtualFrame` points to such a statement, i.e. the errors detected at
the statement and the reports collected from its callees, are discarded when `frame` is
finished.
"""
function is_test_exception_statement(frame::InferenceState, pc::Int)
    handler_info = frame.handler_info
    handler_info === nothing && return false
    (; handlers, handler_at) = handler_info
    checkbounds(Bool, handler_at, pc) || return false
    code = frame.src.code
    handler_idx = first(handler_at[pc])
    while handler_idx != 0
        enter_idx = handlers[handler_idx].enter_idx
        is_enter_test_exception(code[enter_idx+1]) && return true
        handler_idx = first(handler_at[enter_idx])
    end
    return false
end

# Lowering may load the callee in a separate statement before the call.
function is_enter_test_exception(@nospecialize(stmt))
    isexpr(stmt, :call) && (stmt = first(stmt.args))
    return stmt isa GlobalRef && stmt.mod === (@__MODULE__) &&
        stmt.name === :enter_test_exception
end

# The exceptions that inference does not model, e.g. the stack overflow of a deep recursion.
const UNINFERRED_EXCEPTION_TYPE =
    Union{StackOverflowError,OutOfMemoryError,InterruptException,ReadOnlyMemoryError}

"""
    matching_test_exception_type(extype) -> Union{Nothing,Type}

Return the type of the exceptions that match the expected exception of a `@test_throws`
call, given `extype`, the inferred type of the `extype` argument of `check_test_exception`.
Return `nothing` if the type cannot be determined, e.g. for a message pattern, or if it
includes exceptions that inference does not model.
"""
function matching_test_exception_type(@nospecialize(extype))
    extype isa Const || return nothing
    expected = extype.val
    if expected isa Type
        # Test.jl passes `@test_throws LoadError macroexpand(...)` for any exception, and
        # `@test_throws ErrorException ex` for a `FieldError`, both of which are deprecated.
        expected === LoadError && return nothing
        T = expected === ErrorException ? Union{ErrorException,FieldError} : expected
    elseif expected isa Exception
        # Test.jl compares the thrown exception with the expected one, except for a
        # deprecated `LoadError` wrapping the expected exception.
        expected isa LoadError && return nothing
        T = typeof(expected)
    else
        return nothing
    end
    hasintersect(T, UNINFERRED_EXCEPTION_TYPE) && return nothing
    return T
end

# A failing or erroring test aborts a fail-fast test set, so the code after the test runs
# only when the test expression evaluates to `true`. Expressing this as a branch lets
# inference narrow the types in the subsequent code with the tested condition, which is
# preserved through the `let` binding.
function assert_test_expression_value(@nospecialize(ex))
    value = gensym(:test_value)
    return @q begin
        let $value = $ex
            $(checked_test_value(value))
            $(GlobalRef(Core, :(===)))($value, true) ||
                $(GlobalRef(Core, :throw))($(GlobalRef(Test, :FailFastError))())
        end
        $(hide_test_replacement_value(nothing))
    end
end

macro_argument_value(@nospecialize(ex)) = isexpr(ex, :(=), 2) ? last(ex.args) : ex

"""
    testset_failfast(args::Vector{Any}, failfast::Union{Nothing,Bool})

Return the fail-fast mode of the `@testset` call with the macro call arguments `args`,
nested in a test set of the fail-fast mode `failfast`.

The fail-fast mode is `true` if a failing test aborts the test set, `false` if it does not
or that cannot be determined, and `nothing` for a custom test set type, whose behavior on
test failures is unknown. As in Test.jl, a test set inherits the mode of the enclosing test
set unless it specifies `failfast`, and a test set without an explicit type inherits the
type of the enclosing test set. Only a literal `failfast=true` enables the fail-fast mode.
"""
function testset_failfast(args::Vector{Any}, failfast::Union{Nothing,Bool})
    options = args[3:end-1]
    any(@nospecialize(arg) -> arg isa Symbol || isexpr(arg, :.), options) && return nothing
    failfast === nothing && return nothing
    for arg in options
        if isexpr(arg, :(=), 2) && arg.args[1] === :failfast
            failfast = arg.args[2] === true
        end
    end
    return failfast
end

function replace_test(@nospecialize(ex), kws::Vector{Any}, failfast::Bool)
    broken = Any[kw.args[2] for kw in kws if isexpr(kw, :(=), 2) && kw.args[1] === :broken]
    skip = Any[kw.args[2] for kw in kws if isexpr(kw, :(=), 2) && kw.args[1] === :skip]
    kws = Any[kw for kw in kws if !(isexpr(kw, :(=), 2) && kw.args[1] ∈ (:skip, :broken))]
    length(broken) ≤ 1 || return nothing
    length(skip) ≤ 1 || return nothing
    isempty(skip) || isempty(broken) || return nothing
    try
        Test.test_expr!("@test", ex, kws...)
    catch
        return nothing
    end
    if !isempty(skip)
        skip = only(skip)
        if should_wrap_test_expression_in_thunk(ex; conditionally_evaluated = true)
            thunk = gensym(:test_expression)
            test_expr = discard_test_expression_value(checked_test_value(Expr(:call, thunk)))
            ret = @q begin
                let $thunk = () -> $ex
                    if $skip
                        $(hide_test_replacement_value(nothing))
                    else
                        $test_expr
                    end
                end
            end
        else
            test_expr = discard_test_expression_value(checked_test_value(ex))
            ret = @q begin
                if $skip
                    $(hide_test_replacement_value(nothing))
                else
                    $test_expr
                end
            end
        end
        return ret
    end
    ex = maybe_wrap_test_expression_in_thunk(ex)
    if !isempty(broken)
        return Expr(:block, only(broken), discard_test_expression_value(ex))
    elseif failfast
        return assert_test_expression_value(ex)
    end
    return discard_test_expression_value(checked_test_value(ex))
end

# As in Test.jl, the test expression is evaluated inline rather than in a closure, which
# would box the captured local variables reassigned in the surrounding code, and the other
# arguments are evaluated after it in the original scope. The line of the macro call
# `source` set in the `catch` block applies to the code after the `try` block, so that a
# test that always fails is reported there.
function replace_test_throws(@nospecialize(source), exs::Vector{Any})
    exception = gensym(:test_exception)
    caught = @q begin
        try
            $(GlobalRef(@__MODULE__, :enter_test_exception))()
            $(maybe_wrap_test_expression_in_thunk(last(exs)))
            $(GlobalRef(@__MODULE__, :NO_TEST_EXCEPTION))
        catch $exception
            $source
            $exception
        end
    end
    return @q begin
        $(GlobalRef(@__MODULE__, :check_test_exception))($caught, $(exs[1:end-1]...))
        $(hide_test_replacement_value(nothing))
    end
end

"""
    replace_test_macrocall(name::Symbol, args::Vector{Any}, failfast::Bool)

Return the replacement for the Test.jl macro call `name` with the macro call arguments
`args`, or `nothing` if the call is malformed. `failfast` tells whether the call is in a
test set that a failing test aborts (see `testset_failfast`).

The replacements evaluate the arguments of the macro call, catching exceptions where Test.jl
does. The values of the replacements that catch exceptions pass through
`Base.inferencebarrier` so that the synthetic values do not affect the inference of the
surrounding code: macros that return a test result return `nothing`, while macros that
return the value of the test expression keep that value. The checks performed by Test.jl,
such as the message matching of `@test_throws` and the return type check of `@inferred`,
are omitted, except that `JETAnalyzer` reports a `@test` or `@test_throws` call that always
fails (see `FailingTestReport`).

The replacements look like the following, where gensym'ed names are shortened, and the
line numbers of the user code and blocks that wrap a single expression are omitted.

`@test f(x) == 1` becomes the following, where `JET.check_test_value` lets `JETAnalyzer`
report the test if it always fails (see `FailingTestReport`):
```julia
begin
    try
        JET.check_test_value(f(x) == 1)
    catch
    end
    Base.inferencebarrier(nothing)
end
```
Keyword arguments other than `broken` and `skip` are passed to the tested call as Test.jl
does, so `@test a ≈ b atol=0.1` tests `≈(a, b, atol = 0.1)`. The values of the tests
expected to fail are not checked, so `@test_broken f(x) == 1` becomes:
```julia
begin
    try
        f(x) == 1
    catch
    end
    Base.inferencebarrier(nothing)
end
```

`@test f(x) broken=Sys.iswindows()` becomes:
```julia
begin
    Sys.iswindows()
    begin
        try
            f(x)
        catch
        end
        Base.inferencebarrier(nothing)
    end
end
```

`@test f(x) skip=Sys.iswindows()` becomes:
```julia
if Sys.iswindows()
    Base.inferencebarrier(nothing)
else
    begin
        try
            JET.check_test_value(f(x))
        catch
        end
        Base.inferencebarrier(nothing)
    end
end
```

In a fail-fast test set, the code after `@test ex` runs only when `ex` evaluates to `true`,
and an exception thrown by `ex` also aborts the test set. So `@test x isa Int` becomes the
following, which lets inference narrow `x` to `Int` in the subsequent code:
```julia
begin
    let value = x isa Int
        JET.check_test_value(value)
        value === true || throw(Test.FailFastError())
    end
    Base.inferencebarrier(nothing)
end
```

`@test_skip f(x)` does not evaluate `f(x)` and becomes:
```julia
Base.inferencebarrier(nothing)
```

`@test_throws ArgumentError f(-1)` becomes the following, where the errors in the test
expression are not reported since `JET.enter_test_exception` marks it as expected to throw
(see `is_test_exception_statement`), and `JET.check_test_exception` lets `JETAnalyzer`
report the test if the caught exception never matches the expected one (see
`FailingTestReport`):
```julia
begin
    JET.check_test_exception(
        try
            JET.enter_test_exception()
            f(-1)
            JET.NO_TEST_EXCEPTION
        catch exception
            exception
        end,
        ArgumentError)
    Base.inferencebarrier(nothing)
end
```
As in Test.jl, the expected exception is evaluated after the test expression, and so is the
message pattern of `@test_throws ArgumentError "msg" f(-1)`, which Test.jl accepts in
Julia 1.13 and later and is passed as the third argument of `JET.check_test_exception`.

`@test_logs (:warn, "msg") min_level=Logging.Warn f(x)` becomes the following, where the
keyword arguments are reduced to their values:
```julia
let value = nothing
    try
        (:warn, "msg")
        Logging.Warn
        value = f(x)
    catch
    end
    Base.inferencebarrier(value)
end
```
`@test_deprecated` is replaced in the same way, as Test.jl catches exceptions in it when
deprecation warnings are enabled, e.g. by `Pkg.test`.

`@test_warn`, `@test_nowarn`, and `@inferred` evaluate the arguments in a `let` scope
without catching exceptions, as Test.jl does. For example, `@inferred Float64 f(x)` becomes:
```julia
let
    Float64
    f(x)
end
```

The following `@testset`:
```julia
@testset "name" verbose=true begin
    y = f(x)
    @test y == 1
end
```
becomes:
```julia
let
    try
        "name"
        true
        begin
            y = f(x)
            begin
                try
                    y == 1
                catch
                end
                Base.inferencebarrier(nothing)
            end
        end
    catch
    end
    Base.inferencebarrier(nothing)
end
```
The arguments of a `@testset` loop are moved into the loop body since they may refer to
the loop variables. For example, the following `@testset`:
```julia
@testset "i = \$i" for i in 1:3
    @test f(i) > 0
end
```
becomes:
```julia
let
    try
        for i = 1:3
            "i = \$i"
            begin
                try
                    f(i) > 0
                catch
                end
                Base.inferencebarrier(nothing)
            end
        end
    catch
    end
    Base.inferencebarrier(nothing)
end
```
A `@testset` with a `let` argument, e.g. `@testset let x = f() ... end`, is replaced with
the `let` expression itself, since Test.jl does not catch exceptions in it.

When the test expression defines an anonymous function that would be evaluated
conditionally, including within the `if` introduced by `skip`, the test expression is
outlined into a closure (see `maybe_wrap_test_expression_in_thunk`). For example,
`@test all(y -> y > 0, x) skip=cond` becomes:
```julia
let thunk = () -> all(y -> y > 0, x)
    if cond
        Base.inferencebarrier(nothing)
    else
        begin
            try
                JET.check_test_value(thunk())
            catch
            end
            Base.inferencebarrier(nothing)
        end
    end
end
```
"""
function replace_test_macrocall(name::Symbol, args::Vector{Any}, failfast::Bool)
    exs = args[3:end]
    if name === Symbol("@test")
        isempty(exs) && return nothing
        return replace_test(first(exs), Any[exs[2:end]...], failfast)
    elseif name === Symbol("@test_broken")
        isempty(exs) && return nothing
        ex, kws = first(exs), exs[2:end]
        try
            Test.test_expr!("@test_broken", ex, kws...)
        catch
            return nothing
        end
        return discard_test_expression_value(maybe_wrap_test_expression_in_thunk(ex))
    elseif name === Symbol("@test_skip")
        isempty(exs) && return nothing
        ex, kws = first(exs), exs[2:end]
        try
            Test.test_expr!("@test_skip", ex, kws...)
        catch
            return nothing
        end
        return hide_test_replacement_value(nothing)
    elseif name === Symbol("@test_throws")
        length(exs) == 2 || (TEST_THROWS_ACCEPTS_PATTERN && length(exs) == 3) ||
            return nothing
        # Keyword arguments are not supported here. Test.jl in Julia 1.14 and later takes
        # the last argument as a keyword argument rather than the test expression if it
        # follows two positional arguments and assigns `broken`, `skip`, or `context`.
        if length(exs) == 3
            kw = last(exs)
            if isexpr(kw, :(=), 2)
                kwname = first(kw.args)
                (kwname === :broken || kwname === :skip || kwname === :context) &&
                    return nothing
            end
        end
        return replace_test_throws(args[2], exs)
    elseif name === Symbol("@test_logs")
        isempty(exs) && return nothing
        values = Any[macro_argument_value(ex) for ex in exs[1:end-1]]
        push!(values, maybe_wrap_test_expression_in_thunk(last(exs)))
        return preserve_test_expression_value(values...)
    elseif name === Symbol("@test_warn")
        length(exs) == 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return scope_test_expression(exs...)
    elseif name === Symbol("@test_nowarn")
        length(exs) == 1 || return nothing
        return scope_test_expression(maybe_wrap_test_expression_in_thunk(only(exs)))
    elseif name === Symbol("@test_deprecated")
        1 ≤ length(exs) ≤ 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return preserve_test_expression_value(exs...)
    elseif name === Symbol("@inferred")
        1 ≤ length(exs) ≤ 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return scope_test_expression(exs...)
    elseif name === Symbol("@testset")
        isempty(exs) && return nothing
        values = Any[macro_argument_value(ex) for ex in exs[1:end-1]]
        body = last(exs)
        if isexpr(body, :let)
            isempty(values) || return nothing
            bindings = first(body.args)
            isexpr(bindings, :(=)) ||
                isexpr(bindings, :block) && all(ex -> isexpr(ex, :(=)), bindings.args) ||
                return nothing
            return body
        elseif isexpr(body, :for, 2)
            body = copy(body)
            body.args[2] = Expr(:block, values..., body.args[2])
            empty!(values)
        end
        return @q begin
            let
                try
                    $(values...)
                    $body
                catch
                end
                $(hide_test_replacement_value(nothing))
            end
        end
    end
    return nothing
end

"""
    replace_test_macrocalls(x, mod::Module, failfast::Union{Nothing,Bool} = false,
                            failfast_modes::Union{Nothing,Dict} = nothing)

Replace the Test.jl macro calls in the top-level expression `x` with lightweight code that
evaluates the test expressions but omits the construction and recording of test results.
Virtual processing applies this replacement to each top-level statement before macro
expansion, so that inference analyzes user code instead of Test.jl's reporting machinery.

A macro call is replaced only when its name is in `TEST_MACRO_NAMES` and it resolves to the
macro defined in `Test` within `mod` (see `test_macro_name`), e.g. `@test`, `Test.@test`,
or `T.@test` with `const T = Test`. Nested Test.jl macro calls are replaced from the inside
out, while quoted code and the arguments of other macros are left as is. A malformed call
is kept with its arguments replaced, so that the ordinary macro expansion reports the
error.

`failfast` is the fail-fast mode of the test set enclosing `x` (see `testset_failfast`).
It applies to all code lexically within a `@testset` call, including the bodies of the
functions defined there. If `failfast_modes` is given, the fail-fast mode at each line of
`x` is recorded in it, so that an `include` call at the line can pass the mode on to the
included file.

See `replace_test_macrocall` for the replacement of each macro.
"""
function replace_test_macrocalls(
        @nospecialize(x), mod::Module, failfast::Union{Nothing,Bool} = false,
        failfast_modes::Union{Nothing,Dict{Int,Union{Nothing,Bool}}} = nothing)
    if x isa LineNumberNode
        failfast_modes === nothing || (failfast_modes[x.line] = failfast)
        return x
    end
    x isa Expr || return x
    isexpr(x, (:quote, :inert)) && return x
    if isexpr(x, :macrocall)
        name = test_macro_name(mod, first(x.args))
        name ∈ TEST_MACRO_NAMES || return x
        inner_failfast =
            name === Symbol("@testset") ? testset_failfast(x.args, failfast) : failfast
        source = x.args[2]
        if failfast_modes !== nothing && source isa LineNumberNode
            # The body of a one-line `@testset`, e.g. `@testset "x" include("x.jl")`, has no
            # line number of its own but shares the line of the macro call.
            failfast_modes[source.line] = inner_failfast
        end
        args = Any[replace_test_macrocalls(arg, mod, inner_failfast, failfast_modes)
                   for arg in x.args[3:end]]
        replaced = Expr(:macrocall, x.args[1], source, args...)
        replacement = replace_test_macrocall(name, replaced.args, failfast === true)
        return @something replacement replaced
    end
    args = Any[replace_test_macrocalls(arg, mod, failfast, failfast_modes)
               for arg in x.args]
    return Expr(x.head, args...)
end
