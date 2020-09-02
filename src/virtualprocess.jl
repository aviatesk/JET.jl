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

const TYPEPROFILERJL_SELF_REFERENCE_SYM = :TYPEPROFILERJL_SELF_REFERENCE_SYM

"""
    virtual_process!(s::AbstractString,
                     filename::AbstractString,
                     interp::TPInterpreter,
                     actualmodsym::Symbol,
                     virtualmod::Module,
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
- replace self-reference of `actualmodsym` with that of `virtualmod`
- remove `const` annotations so that remaining code block can be wrapped into a virtual
  function (they are not allowed in a function body)

!!! warning
    this approach involves following limitations:
    - if code is directly evaluated but it also includes an evaluation of global objects,
      it will just result in error since global objects don't have actual values
    - if hoisted "toplevel definitions" have access to objects in a local scope, the
      hoisting will just be wrong
    - dynamic `include`, `using`, `import`, `module`, etc. can't be resolved correctly
"""
function virtual_process!(s::AbstractString,
                          filename::AbstractString,
                          actualmodsym::Symbol,
                          virtualmod::Module,
                          interp::TPInterpreter,
                          ret::VirtualProcessResult = generate_virtual_process_result(),
                          )::VirtualProcessResult
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

    return virtual_process!(toplevelex, filename, actualmodsym, virtualmod, interp, ret)
end

function virtual_process!(toplevelex, filename, actualmodsym, virtualmod, interp, ret)
    @assert isexpr(toplevelex, :toplevel)

    # define constant self-referring variable of `virtualmod` so that we can replace
    # self-reference of the original `actualmodsym` with it
    # NOTE: this needs to be "ordinal" identifier for `import`/`using`, etc
    Core.eval(virtualmod, :(const $(TYPEPROFILERJL_SELF_REFERENCE_SYM) = $(virtualmod)))

    # postwalk and do transformations that should be done for all atoms
    # fix self-reference of `actualmodsym` with that of `virtualmod`;
    # this needs to be done for all atoms in advance of the following transformations
    toplevelex = postwalk_and_transform!(toplevelex, Symbol[:toplevel]) do x, scope
        # TODO: this doesn't work when `actualmodsym` is supposed to be a local variable, find a workaround
        x === actualmodsym && return TYPEPROFILERJL_SELF_REFERENCE_SYM

        return x
    end

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

    # prewalk, and some transformations assume that it doesn't happen in local scopes
    function transform!(x, scope)
        # always escape inside expression
        :quote in scope && return x

        # expand macro
        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(virtualmod, x)
        end

        # handle `module` definitions
        if isexpr(x, :module)
            if !(isone(length(scope)) && first(scope) === :toplevel)
                report = SyntaxErrorReport("syntax: \"module\" expression not at top level", filename, line)
                push!(ret.toplevel_error_reports, report)
                return nothing
            end

            newblk = x.args[3]
            @assert isexpr(newblk, :block)
            newtoplevelex = Expr(:toplevel, newblk.args...)

            x.args[3] = Expr(:block) # empty module's code body
            newvirtualmod = eval_with_err_handling(virtualmod, x)

            isnothing(newvirtualmod) && return nothing # error happened, e.g. duplicated naming

            virtual_process!(newtoplevelex, filename, actualmodsym, newvirtualmod, interp, ret)

            return newvirtualmod
        end

        # hoist and evaluate these toplevel expressions and handle module usage
        #
        # these shouldn't happen when in function scope, since then they can be wrongly
        # hoisted and evaluated when they're wrapped in closures, while they actually cause
        # syntax errors; if they're in other "toplevel definition"s, they will just cause
        # error when the defition gets evaluated

        if istopleveldef(x)
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(ret.toplevel_error_reports, report)
                nothing
            end
        end

        # TODO: support package loading
        if ismoduleusage(x)
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(ret.toplevel_error_reports, report)
                nothing
            end
        end

        # evaluate toplevel function definitions
        !islocalscope(scope) && isfuncdef(x) && return eval_with_err_handling(virtualmod, x)

        # remove `const` annotation
        if isexpr(x, :const)
            return if !islocalscope(scope)
                first(x.args)
            else
                report = SyntaxErrorReport("unsupported `const` declaration on local variable", filename, line)
                push!(ret.toplevel_error_reports, report)
                nothing
            end
        end

        # handle static `include` call
        if isinclude(x)
            # TODO: maybe find a way to handle two args `include` calls
            include_file = eval_with_err_handling(virtualmod, last(x.args))

            isnothing(include_file) && return nothing # error happened when evaling `include` args

            include_file = normpath(dirname(filename), include_file)

            # handle recursive `include` calls
            if include_file in ret.included_files
                report = RecursiveIncludeErrorReport(include_file, ret.included_files, filename, line)
                push!(ret.toplevel_error_reports, report)
                return nothing
            end

            read_ex      = :(read($(include_file), String))
            include_text = eval_with_err_handling(virtualmod, read_ex)

            isnothing(include_text) && return nothing # typically no file error

            virtual_process!(include_text, include_file, actualmodsym, virtualmod, interp, ret)

            # TODO: actually, here we need to try to get the last profiling result of the `virtual_process!` call above
            return nothing
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

        x = prewalk_and_transform!(transform!, x, Symbol[:toplevel])

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

function walk_and_transform!(pre, f, x, scope)
    x = pre ? f(x, scope) : x
    x isa Expr || return f(x, scope)
    push!(scope, x.head)
    for (i, ex) in enumerate(x.args)
        x.args[i] = walk_and_transform!(pre, f, ex, scope)
    end
    pop!(scope)
    x = pre ? x : f(x, scope)
    return x
end
prewalk_and_transform!(args...) = walk_and_transform!(true, args...)
postwalk_and_transform!(args...) = walk_and_transform!(false, args...)

istopleveldef(x) = isexpr(x, (:macro, :abstract, :struct, :primitive))

ismoduleusage(x) = isexpr(x, (:import, :using, :export))

islocalscope(scopes)        = any(islocalscope, scopes)
islocalscope(scope::Expr)   = islocalscope(scope.head)
islocalscope(scope::Symbol) = scope in (:quote, :let, :try, :for, :while, :macro, :abstract, :struct, :primitive, :function)

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
