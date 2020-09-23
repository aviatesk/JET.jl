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

"""
    virtual_process!(s::AbstractString,
                     filename::AbstractString,
                     interp::TPInterpreter,
                     actualmodsym::Symbol,
                     virtualmod::Module,
                     )::Tuple{VirtualProcessResult,TPInterpreter}

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

    return virtual_process!(toplevelex, filename, virtualmod, interp, ret)
end

function virtual_process!(toplevelex, filename, virtualmod, interp, ret)
    @assert isexpr(toplevelex, :toplevel)

    line::Int = 1
    filename::String = filename
    interp::TPInterpreter = interp

    function lower_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `lower`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, line))
        return nothing
    end
    lower_with_err_handling(mod, x) = with_err_handling(lower_err_handler) do
        lwr = lower(mod, x)

        # here we should capture syntax errors found during lowering
        if lwr.head === :error
            msg = first(lwr.args)
            push!(ret.toplevel_error_reports, SyntaxErrorReport("syntax: $(msg)", filename, line))
            return nothing
        end

        return lwr
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
        # TODO: handle imports/exports of virtual global variables
        # if the importing of this global variable failed (because they don't not actually
        # get evaluated and thus not exist in `virtualmod`), but we can continue profiling
        # rather than throwing a toplevel error as far as we know its type (,which is kept
        # in `interp.virtual_globalvar_table`).
        # To handle this, we need to restore `Module` object from arbitrary module usage
        # expressions, which can be a bit complicated

        return Core.eval(mod, x)
    end

    expr_splitter = ExprSplitter(virtualmod, toplevelex)
    for (mod, x) in expr_splitter
        if !isexpr(x, :block)
            if isexpr(x, :global, 1)
                # global assignment can be lowered, but global declaration can't
                Core.eval(mod, x)
                continue
            end
            throw("handle this $(x)")
        end

        blk = x::Expr
        lnn = first(blk.args)::LineNumberNode
        ex = last(blk.args)::Expr

        line = lnn.line

        lwr = lower_with_err_handling(mod, blk) # macro within `ex` will be expanded here
        isnothing(lwr) && continue # error handled

        # TODO: handle toplevel definitions in local scopes
        if istopleveldef(ex)
            eval_with_err_handling(mod, lwr)
            continue
        end
        if ismoduleusage(ex)
            usemodule_with_err_handling(interp, mod, lwr)
            continue
        end

        interp = TPInterpreter(; # world age gets updated to take in `λ`
                               inf_params              = InferenceParams(interp),
                               opt_params              = OptimizationParams(interp),
                               optimize                = may_optimize(interp),
                               compress                = may_compress(interp),
                               discard_trees           = may_discard_trees(interp),
                               virtual_globalvar_table = interp.virtual_globalvar_table, # pass on virtual global variable table
                               filter_native_remarks   = interp.filter_native_remarks,
                               )

        src = first(lwr.args)::CodeInfo
        interp, frame = profile_toplevel!(interp, mod, src)

        # correct error reports found by inference on this toplevel `src`
        append!(ret.inference_error_reports, interp.reports)
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

# function walk_and_transform!(pre, @nospecialize(f), x, scope)
#     x = pre ? f(x, scope) : x
#     x isa Expr || return f(x, scope)
#     push!(scope, x.head)
#     for (i, ex) in enumerate(x.args)
#         x.args[i] = walk_and_transform!(pre, f, ex, scope)
#     end
#     pop!(scope)
#     x = pre ? x : f(x, scope)
#     return x
# end
# prewalk_and_transform!(args...) = walk_and_transform!(true, args...)
# postwalk_and_transform!(args...) = walk_and_transform!(false, args...)

istopleveldef(x) = isfuncdef(x) || isexpr(x, (:macro, :abstract, :struct, :primitive))

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

ismoduleusage(x) = isexpr(x, (:import, :using, :export))

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

shouldprofile(@nospecialize(_)) = false
shouldprofile(::Expr)           = true
shouldprofile(::Symbol)         = true

gen_virtual_module(actualmod) =
    Core.eval(actualmod, :(module $(gensym(:TypeProfilerVirtualModule)) end))::Module

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
