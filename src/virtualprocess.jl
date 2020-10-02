# FIXME: I'm too hacky, find an alternative way for simulating toplevel execution, make me more robust at least

const VirtualProcessResult = @NamedTuple begin
    included_files::Set{String}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
end

gen_virtual_process_result() = (; included_files = Set{String}(),
                                  toplevel_error_reports = ToplevelErrorReport[],
                                  inference_error_reports = InferenceErrorReport[],
                                  )::VirtualProcessResult

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

this function first parses `s` and then iterate following steps on each code block:
- first of all, tries to expand macros in a context of `virtualmod`
- replace self-reference of `actualmodsym` with that of `virtualmod` to help type inference
  in the later step
- if it contains "toplevel defintions", e.g. definitions of `macro`, `struct`, `function`,
  etc. _within toplevel scope_, just directly evaluates it in a context of `virtualmod`
- handle `module` expression by recursively calling this function with an newly generated
  virtual module
- handle (static) `include` calls by recursively calling this function on the `include`d file
- tweak toplevel assignments
  * remove `const` annotations so that remaining code block can be wrapped into a virtual
    function (they are not allowed in a function body)
  * annotate regular assignments with `global`, on which `TPInterpreter` will do virtual
    global variable assignment during abstract interpretation
    (see [`typeinf_local(interp::TPInterpreter, frame::InferenceState)`](@ref))

once all the transformation has been done, each code block will be wrapped into a virtual
  (nullary) lambda function, and then type inference will be run on it

!!! warning
    obviously this approach is only a poor model of Julia's actual execution, and has (maybe)
      lots of limitations, like:
    - if a direct evaluation needs access to global objects that have not been actually
      _evaluated_ (just because they have been _profiled_ instead), it just results in error
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
    Core.eval(virtualmod, :(const $(:VM_SELF_REFERRING_SYM) = $(virtualmod)))

    # first pass, do transformations that should be applied for all atoms:
    # 1. replace self-reference of `actualmodsym` with that of `virtualmod`;
    toplevelex = postwalk_and_transform!(toplevelex) do x, scope
        # TODO: this cause wrong program semantics when `actualmodsym` is actually used as a
        # local variable, find a workaround
        x === actualmodsym && return :VM_SELF_REFERRING_SYM

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
        # NOTE: usages of virtual global variables also work here, since they supposed to
        # be actually evaluated in `mod` (as `VirtualGlobalVariable` object)

        # TODO: add some error report pass here

        return Core.eval(mod, x)
    end

    # transform, and then profile sequentially
    exs = reverse(toplevelex.args)
    lnn = LineNumberNode(0, filename)
    while !isempty(exs)
        x = pop!(exs)

        # update line info
        if islnn(x)
            lnn = x
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
        # TODO: support package profiling
        if ismoduleusage(x)
            for ex in to_single_usages(x)
                usemodule_with_err_handling(interp, virtualmod, ex)
            end

            continue
        end

        # second pass, do transformations that need error handling
        x = prewalk_and_transform!(x) do x, scope
            # always escape inside expression
            :quote in scope && return x

            # expand macro
            # ------------

            if isexpr(x, :macrocall)
                x = macroexpand_with_err_handling(virtualmod, x)
            end

            # handle static `include` calls
            # -----------------------------
            # TODO: do something same as toplevel definitions

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

        if hastopleveldef(x)
            # this expression contains "toplevel definition(s)", we don't want to abstract it
            # away, let's just evaluate it (hoping "heavy" computation doesn't happen in this block ...)
            eval_with_err_handling(virtualmod, x)

            continue
        end

        # at this point `x` is supposed not to contain any "toplevel definition" but only
        # toplevel code like call sites and variable assignment;
        # in this pass we transform the toplevel code so that it can be wrapped into a virtual (nullary) lambda
        local locally_scoped_vars = Set{Symbol}()
        x = prewalk_and_transform!(x) do x, scope
            # always escape inside expression
            :quote in scope && return x

            # replace `const` with `global`
            # TODO: add `const` properties for this virtual global variable
            if isexpr(x, :const)
                return if !islocalscope(scope)
                    Expr(:global, first(x.args))
                else
                    report = SyntaxErrorReport("unsupported `const` declaration on local variable", filename, line)
                    push!(ret.toplevel_error_reports, report)
                    nothing
                end
            end

            if isexpr(x, :local)
                x′ = first(x.args)
                # `parse_input_line` will parse "local a, b = sincos(1)" into `:(local (a, b) = sincos(1))`
                if is_assignment(x′)
                    # local a, b = sincos(1)
                    vars = get_lhs(x′)
                    push!(locally_scoped_vars, vars...)
                else
                    # local a, b ...
                    push!(locally_scoped_vars, x.args...)
                end
                return x
            end

            # annotate `global`s for regular assignments in global scope
            if !(islocalscope(scope) || is_already_scoped(scope))
                # `parse_input_line` will parse "a, b = sincos(1)" into `:((a, b) = sincos(1))`
                if is_assignment(x)
                    # FIXME: this can't handle following case
                    # begin
                    #     local l
                    #     l, g = sincos(1) # won't be `global` annotated
                    # end
                    vars = get_lhs(x)
                    any(in(locally_scoped_vars), vars) || return Expr(:global, x)
                end
            end

            return x
        end

        shouldprofile(x) || continue

        λ = gen_virtual_lambda(virtualmod, lnn, x)

        interp = TPInterpreter(; # world age gets updated to take in `λ`
                               inf_params              = InferenceParams(interp),
                               opt_params              = OptimizationParams(interp),
                               optimize                = may_optimize(interp),
                               compress                = may_compress(interp),
                               discard_trees           = may_discard_trees(interp),
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

function walk_and_transform!(pre, @nospecialize(f), x, scope = Symbol[])
    x = pre ? f(x, scope) : x
    if isa(x, Expr)
        push!(scope, x.head)
        for (i, arg) in enumerate(x.args)
            x.args[i] = walk_and_transform!(pre, f, arg, scope)
        end
        pop!(scope)
    end
    return pre ? x : f(x, scope)
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

is_already_scoped(scope) = !isempty(scope) && last(scope) in (:local, :global)

# XXX: this will miss hygine variables (`isidentifier` check)
# but hygine variables are (usually) supposed to be used only within a macro expanded block
# and so it might be okay not to annotate them as virtual global variables
function is_assignment(x)
    isexpr(x, :(=)) || return false
    lhs = first(x.args)
    isa(lhs, Symbol) && return isidentifier(lhs)
    isexpr(lhs, :tuple) && return all(var -> isa(var, Symbol) && isidentifier(var), lhs.args)
    return false
end

function get_lhs(eq)::Vector{Symbol}
    # @assert is_assignment(eq)
    lhs = first(eq.args)
    return (isa(lhs, Symbol) ? [lhs] : lhs.args)
end

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

hastopleveldef(@nospecialize(x)) = false
function hastopleveldef(ex::Expr)
    local found::Bool = false
    prewalk_and_transform!(ex) do x, scope
        # TODO: escape quoted expression unless within `eval` call

        if isexpr(x, (:macro, :abstract, :struct, :primitive))
            found = true
        elseif isfuncdef(x)
            # let will introduce closure, it shouldn't be defined in toplevel
            # i = findlast(s->s===:quote, scope)
            # scope = scope[(isnothing(i) ? 1 : i):end]
            if !any(in((:let, :call)), scope)
                found = true
            end
        end

        return x
    end
    return found
end

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
        # using A, B, export a, b
        return Expr.(x.head, x.args)
    else
        arg = first(x.args)
        if isa(arg, Symbol)
            # export a
            return [x]
        elseif arg.head === :.
            # using A
            return [x]
        elseif arg.head === :(:)
            # using A: sym1, sym2, ...
            args = Expr.(arg.head, Ref(first(arg.args)), arg.args[2:end])
            return Expr.(x.head, args)
        end
    end
end
