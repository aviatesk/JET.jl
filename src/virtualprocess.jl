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
   text parsing or partial (actual) interpretation; these reports are "critical" and should
   have precedence over `inference_error_reports`
- `inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found by
  `TPInterpreter`

this function first parses `s` into `toplevelex::Expr` and then iterate the following steps
  on each code block (`blk`) (after replacing self-reference of `actualmodsym` with that of `virtualmod`):
1. if `blk` is a `:module` expression, recusively call `virtual_process!` with an newly defined
     virtual module
2. if the current code block is a namespace expression (i.e. `:using`, `:import`, and `:export`)
     just evaluate it and `continue`
3. `lower`s `blk` into `lwr` (including macro expansion)
4. partially interpret `lwr`'s statements so that we don't abstract away e.g. `:method` definitions
5. finally, profile the remaining abstract statements by abstract interpretation

!!! warning
    the current approach splits entire code into code blocks and we're not tracking
      inter-code-block level dependencies, and so a partial interpretation of toplevle
      definitions will fail if it needs an access to global variables that are defined
      in the other code block
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

function virtual_process!(toplevelex::Expr,
                          filename::AbstractString,
                          actualmodsym::Symbol,
                          virtualmod::Module,
                          interp::TPInterpreter,
                          ret::VirtualProcessResult = gen_virtual_process_result(),
                          )::Tuple{VirtualProcessResult,TPInterpreter}
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

    filename::String = filename
    lnn::LineNumberNode = LineNumberNode(0, filename)
    interp::TPInterpreter = interp

    function lower_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `lower`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    lower_with_err_handling(mod, x) = with_err_handling(lower_err_handler) do
        lwr = lower(mod, x)

        # here we should capture syntax errors found during lowering
        if isexpr(lwr, :error)
            msg = first(lwr.args)
            push!(ret.toplevel_error_reports, SyntaxErrorReport("syntax: $(msg)", filename, lnn.line))
            return nothing
        end

        return lwr
    end
    function macroexpand_err_handler(err, st)
        # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
        st = crop_stacktrace(st, 4)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    macroexpand_with_err_handling(mod, x) = with_err_handling(macroexpand_err_handler) do
        return macroexpand(mod, x)
    end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
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

        blk = Expr(:block, lnn, x) # attach line number info
        lwr = lower_with_err_handling(virtualmod, blk)

        isnothing(lwr) && continue # error happened during lowering
        isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        interp′ = ActualInterpreter(filename,
                                    lnn,
                                    eval_with_err_handling,
                                    actualmodsym,
                                    virtualmod,
                                    interp,
                                    ret
                                    )
        not_abstracted = partial_interpret!(interp′, virtualmod, src)

        # bail out if nothing to profile (just a performance optimization)
        if all(is_return(last(src.code)) ? not_abstracted[begin:end-1] : not_abstracted)
            continue
        end

        interp = TPInterpreter(; # world age gets updated to take in newly added methods defined by `ActualInterpreter`
                               inf_params            = InferenceParams(interp),
                               opt_params            = OptimizationParams(interp),
                               optimize              = may_optimize(interp),
                               compress              = may_compress(interp),
                               discard_trees         = may_discard_trees(interp),
                               filter_native_remarks = interp.filter_native_remarks,
                               not_abstracted        = not_abstracted, # or construct partial `CodeInfo` from remaining abstract statements
                               )

        profile_toplevel!(interp, virtualmod, src)

        append!(ret.inference_error_reports, interp.reports) # correct error reports
    end

    return ret, interp
end

# trait to inject code into JuliaInterpreter's actual interpretation process
struct ActualInterpreter
    filename::String
    lnn::LineNumberNode
    eval_with_err_handling::Function
    actualmodsym::Symbol
    virtualmod::Module
    interp::TPInterpreter
    ret::VirtualProcessResult
end

# TODO: needs to overload `JuliaInterpreter.handle_err` against `ActualInterpreter`
function partial_interpret!(interp, mod, src)
    selected = select_statements(src)

    # LoweredCodeUtils.print_with_code(stdout, src, selected)

    selective_eval_fromstart!(interp, Frame(mod, src), selected, #= istoplevel =# true)

    return selected
end

# select statements that should be not abstracted away, but rather actually interpreted
function select_statements(src)
    stmts = src.code

    not_abstracted = map(stmts) do stmt
        # special case `include` calls
        isinclude(stmt) && return true

        # interpret method definitions
        ismethod(stmt) && return true

        # interpret type definitions
        istypedef(stmt) && return true
        # add more statements necessary for the first time interpretation of a type definition
        # TODO: maybe upstream these into LoweredCodeUtils.jl ?
        if isexpr(stmt, :(=))
            stmt = stmt.args[2] # rhs
        end
        if isexpr(stmt, :call)
            f = stmt.args[1]
            if isa(f, GlobalRef)
                f.mod === Core && f.name in (:_setsuper!, :_equiv_typedef, :_typebody!) && return true
            end
            if isa(f, QuoteNode)
                f.value in (Core._setsuper!, Core._equiv_typedef, Core._typebody!) && return true
            end
        end

        return false
    end

    lines_required!(not_abstracted, src, CodeEdges(src))

    return not_abstracted
end

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/2f5f80034bc287a60fe77c4e3b5a49a087e38f8b/src/interpret.jl#L188-L199
# works as `JuliaInterpreter.evaluate_call_compiled!`, but special cases for TypeProfiler.jl added
function evaluate_call_recurse!(interp::ActualInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)
    # @assert !enter_generated
    pc = frame.pc
    ret = bypass_builtins(frame, call_expr, pc)
    isa(ret, Some{Any}) && return ret.value
    ret = maybe_evaluate_builtin(frame, call_expr, false)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    f = fargs[1]
    popfirst!(fargs)  # now it's really just `args`

    if isinclude(f)
        return handle_include(fargs, interp)
    else
        ret = f(fargs...)
        @debug "actual interpret: " f fargs ret
        return ret
    end
end

isinclude(@nospecialize(f::Function)) = nameof(f) === :include

function handle_include(fargs, interp)
    filename = interp.filename
    ret = interp.ret
    lnn = interp.lnn
    eval_with_err_handling = interp.eval_with_err_handling
    virtualmod = interp.virtualmod

    # TODO: maybe find a way to handle two args `include` calls
    include_file = normpath(dirname(filename), first(fargs))

    # handle recursive `include`s
    if include_file in ret.included_files
        report = RecursiveIncludeErrorReport(include_file, ret.included_files, filename, lnn.line)
        push!(ret.toplevel_error_reports, report)
        return nothing
    end

    read_ex      = :(read($(include_file), String))
    include_text = eval_with_err_handling(virtualmod, read_ex)

    isnothing(include_text) && return nothing # typically no file error

    virtual_process!(include_text, include_file, interp.actualmodsym, interp.virtualmod, interp.interp, interp.ret)

    # TODO: actually, here we need to try to get the last profiling result of the `virtual_process!` call above
    return nothing
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

ismoduleusage(x) = isexpr(x, (:import, :using, :export))

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true

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
