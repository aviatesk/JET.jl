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

    reports::Vector{ToplevelErrorReport} = ToplevelErrorReport[]
    line::Int = 1
    file::String = filename
    function macroexpand_with_err_handling(mod, x)
        function err_handler(err)
            bt = crop_backtrace(catch_backtrace(), #=XXX=# 3)
            push!(reports, ActualErrorWrapped(err, bt, file, line))
            return nothing
        end
        with_err_handling(err_handler) do
            macroexpand(mod, x)
        end
    end
    function eval_with_err_handling(mod, x)
        function err_handler(err)
            bt = crop_backtrace(catch_backtrace(), #=XXX=# 4)
            push!(reports, ActualErrorWrapped(err, bt, file, line))
            return nothing
        end
        with_err_handling(err_handler) do
            return Core.eval(mod, x)
        end
    end

    ret = walk_and_transform!(ex, Symbol[]) do x, scope
        if x isa LineNumberNode
            line = x.line # update
            # file = x.file # NOTE: will be needed when this function handles `include` calls
            return x
        end

        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(mod, x)
        end

        if :quote in scope
            # always escape inside expression
            x

        elseif isexpr(x, (:macro, :abstract, :struct, :primitive))
            # toplevel expressions other than functions
            leftover = if :function ∉ scope
                eval_with_err_handling(mod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", file, line)
                push!(reports, report)
                nothing
            end
            :($(leftover))

        elseif !islocalscope(scope) && isfuncdef(x)
            # hoist functiono
            leftover = eval_with_err_handling(mod, x)
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

# don't inline this so we can find it in the stacktrace
@noinline function with_err_handling(f, err_handler)
    return try
        f()
    catch err
        err_handler(err)
    end
end

function crop_backtrace(bt, offset)
    i = find_frame_index(bt, @__FILE__, with_err_handling)
    return bt[1:(isnothing(i) ? end : i - offset)]
end

find_frame_index(st::Vector{Base.StackTraces.StackFrame}, file, func) =
    return findfirst(frame -> frame.file === Symbol(file) && frame.func === Symbol(func), st)
function find_frame_index(bt::Vector{<:Union{Base.InterpreterIP,Ptr{Cvoid}}}, file, func)
    for (i, ip) in enumerate(bt)
        st = Base.StackTraces.lookup(ip)
        ind = find_frame_index(st, file, func)
        isnothing(ind) || return i
    end
    return
end
