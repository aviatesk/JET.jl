# TODO:
# - handle toplevel `if/else` correctly
# - profiling on unloaded package
#   * support `module`
#   * special case `__init__` calls

const VirtualProcessResult = @NamedTuple begin
    included_files::Set{String}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
end

generate_virtual_process_result() = return (; included_files = Set{String}(),
                                              toplevel_error_reports = ToplevelErrorReport[],
                                              inference_error_reports = InferenceErrorReport[],
                                              )::VirtualProcessResult

"""
    virtual_process!(interp::TPInterpreter,
                     actualmod::Module,
                     virtualmod::Module,
                     s::AbstractString,
                     filename::AbstractString,
                     ret::VirtualProcessResult = generate_virtual_process_result(),
                     )::VirtualProcessResult

simulates execution of `s` and profiles error reports, and returns `VirtualProcessResult`,
which keeps the following information:
- `included_files::Set{String}`: files that has been profiled
- `toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
   text parsing and AST transformation; these reports are "critical" and should have
   precedence over `inference_error_reports`
- `inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found by
  `TPInterpreter`

this function first parses `s` and then iterate following steps on each code block
1. if it is "toplevel defititions", e.g. definitions of `macro`, `struct`, `function`, etc.,
   just _evaluates_ it in a context of `virtualmod`
2. if not, _transforms_ it so that it can be profiled with a context of `virtualmod`

the _transform_ includes:
- hoist "toplevel defintions" into toplevel and extract them from local blocks, so that
  remaining code block can be wrapped into a virtual function, which will be profiled;
  "toplevel defintions" include:
  * toplevel `function` definition
  * `macro` definition
  * `struct`, `abstract` and `primitive` type definition
  * `import`/`using` statements
- try to expand macros in a context of `virtualmod`
- handle `include` by recursively calling this function on the `include`d file
- fix self-referring dot accessors (i.e. those referring to `actualmod`) so that it can be
  constant-propagated in profiling i.e. abstract interpretation (otherwise they will be
  annotated as `Any` because they'will actually be resolved in a context of `virtualmod`)
- remove `const` annotations so that remaining code block can be wrapped into a virtual
  function (they are not allowed in a function body)

!!! warning
    this approach involves following limitations:
    - if code is directly evaluated but it has an access to global objects, it will just
      result in error since global objects don't have actual values
    - if hoisted "toplevel definitions" have access to objects in a local scope, the hoisting
      will yield error
"""
function virtual_process!(interp::TPInterpreter,
                          actualmod::Module,
                          virtualmod::Module,
                          s::AbstractString,
                          filename::AbstractString,
                          ret::VirtualProcessResult = generate_virtual_process_result(),
                          )::VirtualProcessResult
    filename in ret.included_files && error("recursive `include` call found") # TODO: report instead of error
    push!(ret.included_files, filename)

    toplevelex = parse_input_line(s; filename)

    # if there's any syntax error, try to identify all the syntax error location
    if isexpr(toplevelex, (:error, :incomplete))
        append!(ret.toplevel_error_reports, collect_syntax_errors(s, filename))
        return ret
    # just return if there is nothing to profile
    elseif isnothing(toplevelex)
        return ret
    end

    @assert isexpr(toplevelex, :toplevel)

    line::Int = 1
    filename::String = filename
    function macroexpand_err_handler(err, st)
        # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
        st = crop_stacktrace(st, 4)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    macroexpand_with_err_handling(mod, x) = return with_err_handling(macroexpand_err_handler) do
        macroexpand(mod, x)
    end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, line))
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

    function transform!(x, scope)
        # always escape inside expression
        :quote in scope && return x

        # expand macro
        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(virtualmod, x)
        end

        # evaluate these toplevel expressions only when not in function scope, otherwise
        # these invalid expressions can be "extracted" and evaled wrongly while they actually
        # cause syntax errors
        # TODO: :export
        if isexpr(x, (:macro, :abstract, :struct, :primitive, :import, :using))
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(reports, report)
                nothing
            end
        end

        # evaluate toplevel function definitions
        !islocalscope(scope) && isfuncdef(x) && return eval_with_err_handling(virtualmod, x)

        # remove `const` annotation
        # NOTE:
        # needs to be handled here, otherwise invalid `const` annotations can propagate into
        # `generate_virtual_lambda` (e.g. within `let` block)
        if isexpr(x, :const)
            return if !islocalscope(scope)
                first(x.args)
            else
                report = SyntaxErrorReport("unsupported `const` declaration on local variable", filename, line)
                push!(reports, report)
                nothing
            end
        end

        # handle `include` call
        if isinclude(x)
            # TODO: maybe find a way to handle two args `include` calls
            include_file = eval_with_err_handling(virtualmod, last(x.args))

            isnothing(include_file) && return nothing # error happened when evaling `include` args

            include_file = normpath(dirname(filename), include_file)
            read_ex      = :(read($(include_file), String))
            include_text = eval_with_err_handling(virtualmod, read_ex)

            isnothing(include_text) && return nothing # typically no file error

            virtual_process!(interp, actualmod, virtualmod, include_text, include_file, ret)

            # TODO: actually, here we need to try to get the last profiling result of the `virtual_process!` call above
            return nothing
        end

        # fix self-referring global references
        if isexpr(x, :.) && first(x.args) === actualmodsym
            x.args[1] = constmodsym
            return x
        end

        return x
    end

    # transform, and then profile sequentially
    for x in toplevelex.args
        # update line info
        if islnn(x)
            line = x.line
            continue
        end

        x = walk_and_transform!(transform!, x, Symbol[])

        shouldprofile(x) || continue

        λ = generate_virtual_lambda(virtualmod, LineNumberNode(line, filename), x)

        interp = TPInterpreter(; # world age gets updated to take in `λ`
                               inf_params            = InferenceParams(interp),
                               opt_params            = OptimizationParams(interp),
                               optimize              = may_optimize(interp),
                               compress              = may_compress(interp),
                               discard_trees         = may_discard_trees(interp),
                               istoplevel            = !isa(x, Symbol) && !islocalscope(x), # disable virtual global variable assignment when profiling non-toplevel blocks
                               virtualglobalvartable = interp.virtualglobalvartable, # pass on virtual global variable table
                               filter_native_remarks = interp.filter_native_remarks,
                               )

        profile_call_gf!(interp, Tuple{typeof(λ)})

        append!(ret.inference_error_reports, interp.reports) # correct error reports
    end

    return ret
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

islocalscope(scopes)        = any(islocalscope, scopes)
islocalscope(scope::Expr)   = islocalscope(scope.head)
islocalscope(scope::Symbol) = scope in (:quote, :let, :try, :for, :while)

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

islnn(x) = x isa LineNumberNode

shouldprofile(@nospecialize(_)) = false
shouldprofile(::Expr)           = true
shouldprofile(::Symbol)         = true

generate_virtual_module(actualmod) =
    return Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))::Module

function generate_virtual_lambda(mod, lnn, x)
    funcbody = Expr(:block, lnn, x)
    funcex   = Expr(:function, #=nullary lambda=# Expr(:tuple), funcbody)
    return Core.eval(mod, funcex)::Function
end
