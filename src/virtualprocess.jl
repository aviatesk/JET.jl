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
                     actualmodsym::Symbol,
                     virtualmod::Module,
                     interp::TPInterpreter,
                     )::Tuple{VirtualProcessResult,TPInterpreter}
    virtual_process!(toplevelex::Expr,
                     filename::AbstractString,
                     actualmodsym::Symbol,
                     virtualmod::Module,
                     interp::TPInterpreter,
                     )::Tuple{VirtualProcessResult,TPInterpreter}

simulates Julia's toplevel execution and profiles error reports, and returns
`ret::VirtualProcessResult`, which keeps the following information:
- `ret.included_files::Set{String}`: files that have been profiled
- `ret.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `re.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `TPInterpreter`

this function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
  following steps on each code block (`blk`) of `toplevelex` (after replacing self-reference
  of `actualmodsym` with that of `virtualmod`):
1. if `blk` is a `:module` expression, recusively call `virtual_process!` with an newly defined
     virtual module
2. if the current code block is a namespace expression (i.e. `:using`, `:import`, and `:export`)
     just evaluate it and `continue`
3. `lower`s `blk` into `lwr` (including macro expansion)
4. `ConcreteInterpreter` partially interprets some of `lwr`'s statements that should not be
     abstracted away (e.g. a `:method` definition)
5. finally, `TPInterpreter` profiles the remaining statements by abstract interpretation

!!! warning
    the current approach splits entire code into code blocks and we're not tracking
      inter-code-block level dependencies, and so a partial interpretation of toplevle
      definitions will fail if it needs an access to global variables that are defined
      in the other code block
"""
function virtual_process! end

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

        interp′ = ConcreteInterpreter(filename,
                                      lnn,
                                      eval_with_err_handling,
                                      actualmodsym,
                                      virtualmod,
                                      interp,
                                      ret
                                      )
        concretized = partial_interpret!(interp′, virtualmod, src)

        # bail out if nothing to profile (just a performance optimization)
        if all(is_return(last(src.code)) ? concretized[begin:end-1] : concretized)
            continue
        end

        interp = TPInterpreter(; # world age gets updated to take in newly added methods defined by `ConcreteInterpreter`
                               inf_params            = InferenceParams(interp),
                               opt_params            = OptimizationParams(interp),
                               optimize              = may_optimize(interp),
                               compress              = may_compress(interp),
                               discard_trees         = may_discard_trees(interp),
                               filter_native_remarks = interp.filter_native_remarks,
                               concretized           = concretized, # or construct partial `CodeInfo` from remaining abstract statements
                               )

        profile_toplevel!(interp, virtualmod, src)

        append!(ret.inference_error_reports, interp.reports) # correct error reports
    end

    return ret, interp
end

"""
    partial_interpret!(interp, mod, src)

partially interprets statements in `src` using JuliaInterpreter.jl:
- concretize "toplevel" definitions, i.e. `:method`, `:struct_type`, `:abstract_type` and
    `primitive_type` expressions and their dependencies
- special case `include` calls so that [`virtual_process!`](@ref) recursively runs on the
    included file
"""
function partial_interpret!(interp, mod, src)
    concretized = select_statements(src)

    # LoweredCodeUtils.print_with_code(stdout, src, concretized)

    selective_eval_fromstart!(interp, Frame(mod, src), concretized, #= istoplevel =# true)

    return concretized
end

# select statements that should be not abstracted away, but rather actually interpreted
function select_statements(src)
    stmts = src.code
    edges = CodeEdges(src)

    concretized = fill(false, length(stmts))

    for (i, stmt) in enumerate(stmts)
        if begin
                isinclude(stmt) || # special case `include` calls
                ismethod(stmt) ||  # don't abstract away method definitions
                istypedef(stmt)    # don't abstract away type definitions
            end
            concretized[i] = true
            continue
        end

        if isexpr(stmt, :(=))
            stmt = (stmt::Expr).args[2] # rhs
        end
        if isexpr(stmt, :call)
            f = (stmt::Expr).args[1]

            # add more statements necessary for the first time interpretation of a type definition
            # TODO: maybe upstream these into LoweredCodeUtils.jl ?
            if isa(f, GlobalRef)
                if f.mod === Core && f.name in (:_setsuper!, :_equiv_typedef, :_typebody!)
                    concretized[i] = true
                    continue
                end
            end
            if isa(f, QuoteNode)
                if f.value in (Core._setsuper!, Core._equiv_typedef, Core._typebody!)
                    concretized[i] = true
                    continue
                end
            end

            # analysis of `eval` calls are difficult, let's give up it and just evaluate
            # toplevel `eval` calls; they may contain toplevel definitions
            # adapted from
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L53
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L87-L88
            if f === :eval || (callee_matches(f, Base, :getproperty) && is_quotenode_egal(stmt.args[end], :eval))
                # statement `i` may be the equivalent of `f = Core.eval`, so require each
                # stmt that calls `eval` via `f(expr)`
                concretized[edges.succs[i]] .= true
                concretized[i] = true
            end
        end
    end

    lines_required!(concretized, src, edges)

    return concretized
end

isinclude(ex) = isexpr(ex, :call) && first(ex.args) === :include

# TODO: needs to overload `JuliaInterpreter.handle_err` against `ConcreteInterpreter`
"""
    ConcreteInterpreter

trait to inject code into JuliaInterpreter's interpretation process; we overload:
- `JuliaInterpreter.evaluate_call_recurse!` to special case `include` calls
"""
struct ConcreteInterpreter
    filename::String
    lnn::LineNumberNode
    eval_with_err_handling::Function
    actualmodsym::Symbol
    virtualmod::Module
    interp::TPInterpreter
    ret::VirtualProcessResult
end

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/2f5f80034bc287a60fe77c4e3b5a49a087e38f8b/src/interpret.jl#L188-L199
# works as `JuliaInterpreter.evaluate_call_compiled!`, but special cases for TypeProfiler.jl added
function evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)
    # @assert !enter_generated
    pc = frame.pc
    ret = bypass_builtins(frame, call_expr, pc)
    isa(ret, Some{Any}) && return ret.value
    ret = maybe_evaluate_builtin(frame, call_expr, false)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    f = fargs[1]
    popfirst!(fargs)  # now it's really just `args`

    # @info "concretizing: " f fargs ret
    if isinclude(f)
        return handle_include(interp, fargs)
    else
        ret = f(fargs...)
        return ret
    end
end

isinclude(@nospecialize(f::Function)) = nameof(f) === :include

function handle_include(interp, fargs)
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
        report = isexpr(ex, :error) ? SyntaxErrorReport(string("syntax: ", first(ex.args)), filename, line) :
                 isexpr(ex, :incomplete) ? SyntaxErrorReport(first(ex.args), filename, line) :
                 nothing
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
