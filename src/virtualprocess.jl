# TODO:
# - handle toplevel `if/else` correctly
# - profiling on unloaded package
#   * support `module`
#   * special case `__init__` calls
#
# FIXME: this is too hacky, find an alternative way for simulating toplevel execution, make me more robust at least

const VirtualProcessResult = @NamedTuple begin
    included_files::Set{String}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
end

gen_virtual_process_result() = (; included_files = Set{String}(),
                                  toplevel_error_reports = ToplevelErrorReport[],
                                  inference_error_reports = InferenceErrorReport[],
                                  )::VirtualProcessResult

const VM_SELF_REFERRING_SYM = :VM_SELF_REFERRING_SYM

"""
    virtual_process!(s::AbstractString,
                     filename::AbstractString,
                     interp::TPInterpreter,
                     actualmodsym::Symbol,
                     virtualmod::Module,
                     )::Tuple{VirtualProcessResult,TPInterpreter}

simulates execution of `s` and profiles error reports, and returns `VirtualProcessResult`,
which keeps the following information:
- `included_files::Set{String}`: files that have been profiled
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
- extract "toplevel defintions" from local blocks and hoist them into toplevel, so that
  remaining code block can be wrapped into a virtual function, which `TPInterpreter` will
  later profile on; "toplevel defintions" includes:
  * toplevel `function` definition
  * `macro` definition
  * `struct`, `abstract type` and `primitive type` definition
  * `import`/`using` statements
- try to expand macros in a context of `virtualmod`
- handle `include` calls by recursively calling this function on the included file
- replace self-reference of `actualmodsym` with that of `virtualmod` to help type inference
- tweak toplevel assignments
  * remove `const` annotations so that remaining code block can be wrapped into a virtual
    function (they are not allowed in a function body)
  * annotate regular assignments with `global`, on which `TPInterpreter` will do virtual
    global variable assignment during abstract interpretation
    (see [`typeinf_local(interp::TPInterpreter, frame::InferenceState)`](@ref))

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
                          ret::VirtualProcessResult = gen_virtual_process_result(),
                          )::Tuple{VirtualProcessResult,TPInterpreter}
    push!(ret.included_files, filename)

    toplevelex = parse_input_line(s; filename)

    # if there's any syntax error, try to identify all the syntax error location
    if isexpr(toplevelex, (:error, :incomplete))
        append!(ret.toplevel_error_reports, collect_syntax_errors(s, filename))
        return ret, interp
    # just return if there is nothing to profile
    elseif isnothing(toplevelex)
        return ret, interp
    end

    return virtual_process!(toplevelex, filename, actualmodsym, virtualmod, interp, ret)
end

function virtual_process!(toplevelex, filename, actualmodsym, virtualmod, interp, ret)
    @assert isexpr(toplevelex, :toplevel)

    # define a constant that self-refers to `virtualmod` so that we can replace
    # self-references of the original `actualmodsym` with it
    # NOTE: this identifier can be `gensym`ed, `import/using` will fail otherwise
    Core.eval(virtualmod, :(const $(VM_SELF_REFERRING_SYM) = $(virtualmod)))

    # first transformation pass, do transformations that should be applied for all atoms:
    # 1. replace self-reference of `actualmodsym` with that of `virtualmod`;
    toplevelex = postwalk_and_transform!(toplevelex, Symbol[:toplevel]) do x, scope
        # TODO: this cause wrong program semantics when `actualmodsym` is actually used as a
        # local variable, find a workaround
        x === actualmodsym && return VM_SELF_REFERRING_SYM

        return x
    end

    line::Int = 1
    filename::String = filename
    interp::TPInterpreter = interp

    function macroexpand_err_handler(err, st)
        # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
        st = crop_stacktrace(st, 4)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    macroexpand_with_err_handling(mod, x) = with_err_handling(macroexpand_err_handler) do
        return macroexpand(mod, x)
    end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    eval_with_err_handling(mod, x) = with_err_handling(eval_err_handler) do
        return Core.eval(mod, x)
    end
    usemodule_with_err_handling(interp, mod, x) = with_err_handling(eval_err_handler) do
        # TODO: handle imports of virtual global variables
        # if the importing of this global variable failed (because they don't not actually
        # get evaluated and thus not exist in `virtualmod`), but we can continue profiling
        # rather than throwing a toplevel error as far as we know its type (,which is kept
        # in `interp.virtual_globalvar_table`).
        # To handle this, we need to restore `Module` object from arbitrary module usage
        # expressions, which can be a bit complicated

        return Core.eval(mod, x)
    end

    # prewalk, and some transformations assume that it doesn't happen in local scopes
    function transform!(x, scope)
        # always escape inside expression
        :quote in scope && return x

        # expand macro
        # ------------

        if isexpr(x, :macrocall)
            x = macroexpand_with_err_handling(virtualmod, x)
        end

        # hoist and evaluate toplevel expressions
        # ---------------------------------------

        # these shouldn't happen when in function scope, since then they can be wrongly
        # hoisted and evaluated when they're wrapped in closures, while they actually cause
        # syntax errors; if they're in other "toplevel definition"s, the expression should
        # have been reported already on the evaluation of its parent "toplevel definition"
        if istopleveldef(x)
            return if :function ∉ scope
                eval_with_err_handling(virtualmod, x)
            else
                report = SyntaxErrorReport("syntax: \"$(x.head)\" expression not at top level", filename, line)
                push!(ret.toplevel_error_reports, report)
                nothing
            end
        end

        # evaluate toplevel function definitions
        # --------------------------------------

        !islocalscope(scope) && isfuncdef(x) && return eval_with_err_handling(virtualmod, x)

        # tweak assignments
        # -----------------

        # remove `const` annotation, annotate `global` instead
        if isexpr(x, :const)
            return if !islocalscope(scope)
                Expr(:global, first(x.args))
            else
                report = SyntaxErrorReport("unsupported `const` declaration on local variable", filename, line)
                push!(ret.toplevel_error_reports, report)
                nothing
            end
        end

        # annotate `global`s for regular assignments in global scope
        !(islocalscope(scope) || is_already_scoped(scope)) && is_assignment(x) && return Expr(:global, x)

        # handle static `include` calls
        # -----------------------------

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

    lnn = LineNumberNode(0, filename)

    # transform, and then profile sequentially
    exs = reverse(toplevelex.args)
    while !isempty(exs)
        x = pop!(exs)

        # update line info
        if islnn(x)
            lnn = x::LineNumberNode
            continue
        end

        # flatten container expression
        if isexpr(x, :toplevel)
            append!(exs, reverse(x.args))
            continue
        end

        # handle `:module` definition and module usage;
        # should happen here because modules need to be loaded sequentially while
        # "toplevel definitions" inside of the loaded modules shouldn't be evaluated in a
        # context of `virtualmod`

        # handle `:module` definition; they should always happen in toplevel
        if isexpr(x, :module)
            newblk = x.args[3]
            @assert isexpr(newblk, :block)
            newtoplevelex = Expr(:toplevel, newblk.args...)

            x.args[3] = Expr(:block) # empty module's code body
            newvirtualmod = eval_with_err_handling(virtualmod, x)

            isnothing(newvirtualmod) && continue # error happened, e.g. duplicated naming

            virtual_process!(newtoplevelex, filename, actualmodsym, newvirtualmod, interp, ret)

            continue
        end

        # handle module usage
        # TODO: support package loading
        if ismoduleusage(x)
            for ex in to_single_usages(x)
                usemodule_with_err_handling(interp, virtualmod, ex)
            end

            continue
        elseif isexport(x)
            eval_with_err_handling(virtualmod, x)

            continue
        end

        x = prewalk_and_transform!(transform!, x, Symbol[:toplevel])

        shouldprofile(x) || continue

        λ = gen_virtual_lambda(virtualmod, lnn, x)

        interp = TPInterpreter(; # world age gets updated to take in `λ`
                               inf_params              = InferenceParams(interp),
                               opt_params              = OptimizationParams(interp),
                               optimize                = may_optimize(interp),
                               compress                = may_compress(interp),
                               discard_trees           = may_discard_trees(interp),
                               virtual_globalvar_table = interp.virtual_globalvar_table, # pass on virtual global variable table
                               filter_native_remarks   = interp.filter_native_remarks,
                               )

        profile_call_gf!(interp, Tuple{typeof(λ)})

        append!(ret.inference_error_reports, interp.reports) # correct error reports
    end

    return ret, interp
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

function walk_and_transform!(pre, @nospecialize(f), x, scope)
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

ismoduleusage(x) = isexpr(x, (:import, :using))
isexport(x) = isexpr(x, :export)

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

is_already_scoped(scope) = last(scope) in (:local, :global)
function is_assignment(x)
    isexpr(x, :(=)) || return false
    lhs = first(x.args)
    isa(lhs, Symbol) || return false
    Base.isidentifier(lhs)
end

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

shouldprofile(@nospecialize(_)) = false
shouldprofile(::Expr)           = true
shouldprofile(::Symbol)         = true

gen_virtual_module(actualmod) =
    Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))::Module

function gen_virtual_lambda(mod, lnn, x)
    funcbody = Expr(:block, lnn, x)
    funcex   = Expr(:function, #=nullary lambda=# Expr(:tuple), funcbody)
    return Core.eval(mod, funcex)::Function
end

function to_single_usages(x)
    if length(x.args) != 1
        # using A, B
        return Expr.(x.head, x.args)
    else
        arg = x.args[1]
        if arg.head === :.
            # using A
            return [x]
        elseif arg.head === :(:)
            # using A: sym1, sym2, ...
            args = Expr.(arg.head, Ref(first(arg.args)), arg.args[2:end])
            return Expr.(x.head, args)
        end
    end
end
