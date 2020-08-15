# TODO:
# - `module`
# - `using`, `import`, `__init__`
# - special case `include` call

"""
    parse_and_transform(mod::Module, s::AbstractString, filename::AbstractString) ->
      Union{Expr,Vector{<:ToplevelErrorReport}}

Parses `s` into a toplevel expression and transforms the resulting expression so that the
  final output expression can be wrapped into a virtual function to be profiled in
  `mod::Module`.

Returns `Vector{<:ToplevelErrorReport}` if there are any error found during the text parsing
 and AST transformation.
The AST transformation includes:
- extract toplevel "defintions" and directly evaluate them in a given `mod::Module`
- expand macros
- remove `const` annotations
"""
function parse_and_transform(mod::Module,
                             s::AbstractString,
                             filename::AbstractString
                             )::Union{Expr,Vector{<:ToplevelErrorReport}}
    ex = parse_input_line(s; filename)

    # if there's any syntax error, try to identify all the syntax error location
    isexpr(ex, (:error, :incomplete)) && return collect_syntax_errors(s, filename)

    @assert isexpr(ex, :toplevel)

    reports = ToplevelErrorReport[]
    line = 1
    file = filename

    ret = walk_and_transform!(ex, Symbol[]) do x, scope
        if x isa LineNumberNode
            line = x.line # update
            # file = x.file # NOTE: will be needed when this function handles `include` calls
            return x
        end

        if isexpr(x, :macrocall)
            x = try
                macroexpand(mod, x)
            catch err
                bt = catch_backtrace()
                push!(reports, ActualErrorWrapped(err, bt, file, line))
                nothing
            end
        end

        if :quote in scope
            # always escape inside expression
            x

        elseif isexpr(x, (:macro, :abstract, :struct, :primitive))
            # toplevel expressions other than functions
            leftover = if :function ∉ scope
                try
                    Core.eval(mod, x)
                catch err
                    bt = catch_backtrace()
                    push!(reports, ActualErrorWrapped(err, bt, file, line))
                    nothing
                end
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", file, line)
                push!(reports, report)
                nothing
            end
            :($(leftover))

        elseif !islocalscope(scope) && isfuncdef(x)
            # hoist function
            leftover = try
                Core.eval(mod, x)
            catch err
                bt = catch_backtrace()
                push!(reports, ActualErrorWrapped(err, bt, file, line))
                nothing
            end
            :($(leftover))

        elseif isexpr(x, :const)
            # remove `const` annotation
            first(x.args)

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
    foreach(enumerate(x.args)) do (i, ex)
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
