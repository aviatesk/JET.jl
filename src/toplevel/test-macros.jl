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
        @invokelatest(isdefinedglobal(mod, modname)) || return nothing
        @invokelatest(getglobal(mod, modname)) === Test || return nothing
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

macro_argument_value(@nospecialize(ex)) = isexpr(ex, :(=), 2) ? last(ex.args) : ex

function replace_test(@nospecialize(ex), kws::Vector{Any})
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
            test_expr = discard_test_expression_value(Expr(:call, thunk))
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
            test_expr = discard_test_expression_value(ex)
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
    test_expr = discard_test_expression_value(maybe_wrap_test_expression_in_thunk(ex))
    if !isempty(broken)
        return Expr(:block, only(broken), test_expr)
    end
    return test_expr
end

"""
    replace_test_macrocall(name::Symbol, args::Vector{Any}) -> Union{Expr,Nothing}

Return the replacement for the Test.jl macro call `name` with the macro call arguments
`args`, or `nothing` if the call is malformed.

The replacements evaluate the arguments of the macro call, catching exceptions as Test.jl
does. Their values pass through `Base.inferencebarrier` so that the synthetic values do not
affect the inference of the surrounding code: macros that return a test result return
`nothing`, while macros that return the value of the test expression keep that value.
The checks performed by Test.jl, such as the exception type matching of `@test_throws` and
the return type check of `@inferred`, are omitted.

The replacements look like the following, where gensym'ed names are shortened, and the
line numbers of the user code and blocks that wrap a single expression are omitted.

`@test f(x) == 1` and `@test_broken f(x) == 1` become:
```julia
begin
    try
        f(x) == 1
    catch
    end
    Base.inferencebarrier(nothing)
end
```
Keyword arguments other than `broken` and `skip` are passed to the tested call as Test.jl
does, so `@test a ≈ b atol=0.1` tests `≈(a, b, atol = 0.1)`.

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
            f(x)
        catch
        end
        Base.inferencebarrier(nothing)
    end
end
```

`@test_skip f(x)` does not evaluate `f(x)` and becomes:
```julia
Base.inferencebarrier(nothing)
```

`@test_throws ArgumentError f(-1)` becomes:
```julia
begin
    try
        begin
            ArgumentError
            f(-1)
        end
    catch
    end
    Base.inferencebarrier(nothing)
end
```

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
`@test_warn`, `@test_nowarn`, `@test_deprecated`, and `@inferred` are replaced in the same
way. For example, `@inferred Float64 f(x)` becomes:
```julia
let value = nothing
    try
        Float64
        value = f(x)
    catch
    end
    Base.inferencebarrier(value)
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
                thunk()
            catch
            end
            Base.inferencebarrier(nothing)
        end
    end
end
```
"""
function replace_test_macrocall(name::Symbol, args::Vector{Any})
    exs = args[3:end]
    if name === Symbol("@test")
        isempty(exs) && return nothing
        return replace_test(first(exs), Any[exs[2:end]...])
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
        length(exs) == 2 || return nothing
        ex = maybe_wrap_test_expression_in_thunk(Expr(:block, exs...))
        return discard_test_expression_value(ex)
    elseif name === Symbol("@test_logs")
        isempty(exs) && return nothing
        values = Any[macro_argument_value(ex) for ex in exs[1:end-1]]
        push!(values, maybe_wrap_test_expression_in_thunk(last(exs)))
        return preserve_test_expression_value(values...)
    elseif name === Symbol("@test_warn")
        length(exs) == 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return preserve_test_expression_value(exs...)
    elseif name === Symbol("@test_nowarn")
        length(exs) == 1 || return nothing
        return preserve_test_expression_value(
            maybe_wrap_test_expression_in_thunk(only(exs)))
    elseif name === Symbol("@test_deprecated")
        1 ≤ length(exs) ≤ 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return preserve_test_expression_value(exs...)
    elseif name === Symbol("@inferred")
        1 ≤ length(exs) ≤ 2 || return nothing
        exs = Any[exs...]
        exs[end] = maybe_wrap_test_expression_in_thunk(last(exs))
        return preserve_test_expression_value(exs...)
    elseif name === Symbol("@testset")
        isempty(exs) && return nothing
        values = Any[macro_argument_value(ex) for ex in exs[1:end-1]]
        body = last(exs)
        if isexpr(body, :for, 2)
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
    replace_test_macrocalls(x, mod::Module)

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

See `replace_test_macrocall` for the replacement of each macro.
"""
function replace_test_macrocalls(@nospecialize(x), mod::Module)
    x isa Expr || return x
    isexpr(x, (:quote, :inert)) && return x
    if isexpr(x, :macrocall)
        name = test_macro_name(mod, first(x.args))
        name ∈ TEST_MACRO_NAMES || return x
        replaced = Expr(:macrocall, x.args[1], x.args[2],
            map(@nospecialize(arg) -> replace_test_macrocalls(arg, mod), x.args[3:end])...)
        replacement = replace_test_macrocall(name, replaced.args)
        return @something replacement replaced
    end
    return Expr(x.head,
        map(@nospecialize(arg) -> replace_test_macrocalls(arg, mod), x.args)...)
end
