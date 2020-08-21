# TODO:
# - `module`
# - `using`, `import`, `__init__`
# - special case `include` call

"""
    parse_and_transform(actualmod::Module,
                        virtualmod::Module,
                        s::AbstractString,
                        filename::AbstractString,
                        ) -> Union{Expr,Vector{<:ToplevelErrorReport}}

Parses `s` into a toplevel expression and transforms the resulting expression so that the
  final output expression can be wrapped into a virtual function to be profiled in
  `virtualmod`.
Returns `Vector{<:ToplevelErrorReport}` if there are any error found during the text parsing
 and AST transformation.

The AST transformation includes:
- try to extract toplevel "defintions" and directly evaluate them in a context of
  `virtualmod`; toplevel "defintions" include:
  * toplevel function definition
  * macro definition
  * struct, abstract and primitive type definition
- try to expand macros in a context of `virtualmod`
- fix self-referring dot accessors (i.e. those referring to `actualmod`) so that it can be
  constant-propagated in abstract interpretation (otherwise they will be annotated as `Any`
  because they'will actually be resolved in a context of `virtualmod`)
- remove `const` annotations (`const` annotations are not allowed in a function body)
"""
function parse_and_transform(actualmod::Module,
                             virtualmod::Module,
                             s::AbstractString,
                             filename::AbstractString,
                             )::Union{Expr,Vector{<:ToplevelErrorReport}}
    ex = parse_input_line(s; filename)

    # if there's any syntax error, try to identify all the syntax error location
    isexpr(ex, (:error, :incomplete)) && return collect_syntax_errors(s, filename)

    @assert isexpr(ex, :toplevel)

    reports::Vector{ToplevelErrorReport} = ToplevelErrorReport[]
    line::Int = 1
    file::String = filename
    function macroexpand_err_handler(err, st)
        # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
        st = crop_stacktrace(st, 4)
        push!(reports, ActualErrorWrapped(err, st, file, line))
        return nothing
    end
    macroexpand_with_err_handling(mod, x) = return with_err_handling(macroexpand_err_handler) do
        macroexpand(mod, x)
    end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(reports, ActualErrorWrapped(err, st, file, line))
        return nothing
    end
    eval_with_err_handling(mod, x) = return with_err_handling(eval_err_handler) do
        Core.eval(mod, x)
    end

    # defined constant self-referring variable so that global references using it can be
    # constant propagated
    actualmodsym = Symbol(actualmod)
    constmodsym  = gensym(actualmodsym)
    Core.eval(virtualmod, :(const $(constmodsym) = $(actualmodsym)))

    ret = walk_and_transform!(ex, Symbol[]) do x, scope
        # update file/line info
        if x isa LineNumberNode
            line = x.line
            # file = x.file # NOTE: will be needed when this function handles `include` calls
            return x
        end

        # expand macro
        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(virtualmod, x)
        end

        # always escape inside expression
        if :quote in scope
            x

        # evaluate these toplevel expressions only when not in function scope:
        # owe need this because otherwise these invalid expressions can be "extracted" and
        # evaled wrongly while they actually cause syntax errors
        elseif isexpr(x, (:macro, :abstract, :struct, :primitive))
            leftover = if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", file, line)
                push!(reports, report)
                nothing
            end
            :($(leftover))

        # hoist toplevel function definitions
        elseif !islocalscope(scope) && isfuncdef(x)
            leftover = eval_with_err_handling(virtualmod, x)
            :($(leftover))

        # remove `const` annotation
        elseif isexpr(x, :const)
            first(x.args)

        # fix self-referring global references
        elseif isexpr(x, :.) && first(x.args) === actualmodsym
            x.args[1] = constmodsym
            x

        else
            x
        end
    end

    return if isempty(reports)
        ret
    else
        reports # non-empty `reports` means critical errors happened
    end
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
