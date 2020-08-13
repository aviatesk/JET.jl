function parse_to_toplevel(s, filename)
    ex = parse_input_line(s; filename)

    isexpr(ex, (:error, :incomplete)) || return ex

    # if there's any syntax error, try to identify all the syntax error location
    reports = ToplevelErrorReport[]
    index = 1
    line = 1
    while begin
            ex, nextindex = _parse_string(s, filename, index, :statement)
            !isnothing(ex)
        end
        line += count(==('\n'), s[index:nextindex-1])
        report = if isexpr(ex, :error)
            report = SyntaxErrorReport(string("syntax: ", first(ex.args)), line)
        elseif isexpr(ex, :incomplete)
            report = SyntaxErrorReport(first(ex.args), line)
        else
            nothing
        end
        isnothing(report) || push!(reports, report)
        index = nextindex
    end
    return reports
end

# TODO:
# - `module`
# - `using`, `import`, `__init__`
# - special case `include` call

"""
    transform_for_profiling!(mod::Module, toplevelex::Expr)

Transform the given toplevel `toplevelex::Expr` so that final output can be wrapped into a virtual
  function and profiled:
- extract toplevel "defintions" and directly evaluate them in a given `mod::Module`
- expand macros
- remove `const` annotations
"""
function transform_for_profiling!(mod::Module, toplevelex::Expr)
    @assert isexpr(toplevelex, :toplevel) "toplevel expression should be given"

    reports = ToplevelErrorReport[]
    line = 1

    ret = walk_and_transform!(toplevelex, Symbol[]) do x, scope
        if x isa LineNumberNode
            line = x.line # updatet
            return x
        end

        # TODO: report errors in macro expansion
        if isexpr(x, :macrocall)
            x = try
                macroexpand(mod, x)
            catch err
                push!(reports, ActualErrorWrapped(err, line))
                nothing
            end
        end


        if :quote in scope
            # always escape inside expression
            x

        elseif isexpr(x, TOPLEVEL_EXS)
            # toplevel expressions other than functions
            leftover = if :function ∉ scope
                try
                    Core.eval(mod, x)
                catch err
                    push!(reports, ActualErrorWrapped(err, line))
                    nothing
                end
            else
                report = SyntaxErrorReport("syntax: \"$(toplevelex.head)\" expression not at top level", line)
                push!(reports, report)
                nothing
            end
            :($(leftover))

        elseif !islocalscope(scope) && isfuncdef(x)
            # hoist function
            leftover = try
                Core.eval(mod, x)
            catch err
                push!(reports, ActualErrorWrapped(err, line))
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

const TOPLEVEL_EXS = (:macro, :abstract, :struct, :primitive)

const LOCAL_SCOPES = (:let, :quote, :if, :try, :for, :while)
function islocalscope(scope)
    for s in scope
        s in LOCAL_SCOPES && return true
    end

    return false
end

function isfuncdef(ex)
    isexpr(ex, :function) && return true
    isexpr(ex, :(=)) && isexpr(first(ex.args), :call) && return true # short form

    return false
end
