# TODO:
# - profiling on unloaded package
#   * support `module`
#   * special case `__init__` calls
# - respect evaluation order; currently `parse_and_transform` evaluates "toplevel definitions"
#   in batch but it misses errors because of "not-yet-defined" definitions

const Transformed = let
    sym2typ = (
        :filename    => String,
        :transformed => Expr,
        :reports     => Vector{ToplevelErrorReport}
    )
    NamedTuple{first.(sym2typ), to_tuple_type(last.(sym2typ))}
end

"""
    parse_and_transform(actualmod::Module,
                        virtualmod::Module,
                        s::AbstractString,
                        filename::AbstractString,
                        ret::Vector{Transformed} = Transformed[],
                        ) -> Vector{Transformed}

Parses `s` into a toplevel expression and transforms the resulting expression so that the
  output expression can be wrapped into a virtual function to be profiled in `virtualmod`.

Returns `Vector{Transformed}` where `Transformed`-element keeps the following information:
- `filename`: file that has been parsed
- `transformed`: transformed `:toplevel` expression, or `:empty` expression if nothing has
  been parsed
- `reports`: errors found during the text parsing and AST transformation

The AST transformation includes:
- try to extract toplevel "defintions" and directly evaluate them in a context of
  `virtualmod`; toplevel "defintions" include:
  * toplevel `function` definition
  * `macro` definition
  * `struct`, `abstract` and `primitive` type definition
  * `import`/`using` statements
  * handle `include`
- try to expand macros in a context of `virtualmod`
- fix self-referring dot accessors (i.e. those referring to `actualmod`) so that it can be
  constant-propagated in abstract interpretation (otherwise they will be annotated as `Any`
  because they'will actually be resolved in a context of `virtualmod`)
- remove `const` annotations (they are not allowed in a function body)
"""
function parse_and_transform(actualmod::Module,
                             virtualmod::Module,
                             s::AbstractString,
                             filename::AbstractString,
                             ret::Vector{Transformed} = Transformed[],
                             )::Vector{Transformed}
    ex = parse_input_line(s; filename)

    # if there's any syntax error, try to identify all the syntax error location
    if isexpr(ex, (:error, :incomplete))
        reports = collect_syntax_errors(s, filename)
        push!(ret, (; filename, transformed = ex, reports))
        return ret
    elseif isnothing(ex)
        push!(ret, (; filename, transformed = Expr(:empty), reports = []))
        return ret
    end

    @assert isexpr(ex, :toplevel)

    reports::Vector{ToplevelErrorReport} = ToplevelErrorReport[]
    line::Int = 1
    filename::String = filename
    function macroexpand_err_handler(err, st)
        # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
        st = crop_stacktrace(st, 4)
        push!(reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    macroexpand_with_err_handling(mod, x) = return with_err_handling(macroexpand_err_handler) do
        macroexpand(mod, x)
    end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    eval_with_err_handling(mod, x) = return with_err_handling(eval_err_handler) do
        Core.eval(mod, x)
    end

    # define constant self-referring variable so that global references using it can be
    # constant propagated
    actualmodsym = Symbol(actualmod)
    constmodsym  = gensym(actualmodsym)
    Core.eval(virtualmod, :(const $(constmodsym) = $(actualmodsym)))

    function walker(x, scope)
        # update line info
        if x isa LineNumberNode
            line = x.line
            return x
        end

        # expand macro
        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(virtualmod, x)
        end

        # always escape inside expression
        if :quote in scope
            return x
        end

        # evaluate these toplevel expressions only when not in function scope:
        # we need this because otherwise these invalid expressions can be "extracted" and
        # evaled wrongly while they actually cause syntax errors
        if isexpr(x, (:macro, :abstract, :struct, :primitive))
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(reports, report)
                nothing
            end
        end

        # TODO: enable profiling on module (i.e. without actual loading, combined with `include` support)
        if isexpr(x, (:import, :using))
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(reports, report)
                nothing
            end
        end

        # hoist toplevel function definitions
        if !islocalscope(scope) && isfuncdef(x)
            return eval_with_err_handling(virtualmod, x)
        end

        # remove `const` annotation
        if isexpr(x, :const)
            return first(x.args)
        end

        # handle `include` call
        if isinclude(x)
            # can't correctly eval expressions with accessing global variables, etc
            include_file = eval_with_err_handling(virtualmod, last(x.args))

            # error happened when evaling
            isnothing(include_file) && return nothing

            include_file = normpath(dirname(filename), include_file)
            if isfile(include_file)
                include_text = read(include_file, String)

                parse_and_transform(actualmod, virtualmod, include_text, include_file, ret)

                # try to get last expression
                include_transformed = last(ret).transformed
                isexpr(include_transformed, :toplevel) || return nothing
                return last(include_transformed.args)
            else
                error("implement error report for this pass") # TODO
            end
        end

        # fix self-referring global references
        if isexpr(x, :.) && first(x.args) === actualmodsym
            x.args[1] = constmodsym
            return x
        end

        return x
    end

    transformed = walk_and_transform!(walker, ex, Symbol[])
    push!(ret, (; filename, transformed, reports))
    return ret
end

function collect_syntax_errors(s, filename)
    reports = SyntaxErrorReport[]
    index = line = 1
    while begin
            ex, nextindex = _parse_string(s, filename, index, :statement)
            !isnothing(ex)
        end
        line += count(==('\n'), s[index:nextindex-1])
        report = if isexpr(ex, :error)
            SyntaxErrorReport(string("syntax: ", first(ex.args)), filename, line)
        elseif isexpr(ex, :incomplete)
            SyntaxErrorReport(first(ex.args), filename, line)
        else
            nothing
        end
        isnothing(report) || push!(reports, report)
        index = nextindex
    end
    return reports
end

function walk_and_transform!(f, x, scope)
    x = f(x, scope)
    x isa Expr || return x
    push!(scope, x.head)
    for (i, ex) in enumerate(x.args)
        x.args[i] = walk_and_transform!(f, ex, scope)
    end
    pop!(scope)
    return x
end

function islocalscope(scope)
    for s in scope
        s in (:let, :quote, :if, :try, :for, :while) && return true
    end

    return false
end

function isfuncdef(ex)
    isexpr(ex, :function) && return true

    # short form
    if isexpr(ex, :(=))
        farg = first(ex.args)
        isexpr(farg, :call) && return true
        isexpr(farg, :where) && isexpr(first(farg.args), :call) && return true
    end

    return false
end

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

# don't inline this so we can find it in the stacktrace
@noinline function with_err_handling(f, err_handler)
    return try
        f()
    catch err
        bt = catch_backtrace()
        st = stacktrace(bt)
        err_handler(err, st)
    end
end

function crop_stacktrace(st, offset)
    i = find_frame_index(st, @__FILE__, with_err_handling)
    return st[1:(isnothing(i) ? end : i - offset)]
end

function find_frame_index(st, file, func)
    return findfirst(st) do frame
        return frame.file === Symbol(file) && frame.func === Symbol(func)
    end
end
