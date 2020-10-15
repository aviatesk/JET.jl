"""
    @invokelatest f(args...; kwargs...)

provides a convenient way to call [`Base.invokelatest`](@ref);
`@invokelatest f(args...; kwargs...)` will be simply expanded into `Base.invokelatst(f, args...; kwargs...)`
```
"""
macro invokelatest(ex)
    @assert isexpr(ex, :call) "call expression should be given"

    f = first(ex.args)
    args = []
    kwargs = []
    for x in ex.args[2:end]
        if isexpr(x, :parameters)
            append!(kwargs, x.args)
        elseif isexpr(x, :kw)
            push!(kwargs, x)
        else
            push!(args, x)
        end
    end
    return if isempty(kwargs) # eliminates dispatch to kwarg methods, might unnecessary to be special cased
        :(Base.invokelatest($(f), $(args...)))
    else
        :(Base.invokelatest($(f), $(args...); $(kwargs...)))
    end |> esc
end

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
                     virtualmod::Module,
                     actualmodsym::Symbol,
                     interp::JETInterpreter,
                     )::Tuple{VirtualProcessResult,JETInterpreter}
    virtual_process!(toplevelex::Expr,
                     filename::AbstractString,
                     virtualmod::Module,
                     actualmodsym::Symbol,
                     interp::JETInterpreter,
                     )::Tuple{VirtualProcessResult,JETInterpreter}

simulates Julia's toplevel execution and profiles error reports, and returns
`ret::VirtualProcessResult`, which keeps the following information:
- `ret.included_files::Set{String}`: files that have been profiled
- `ret.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `ret.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `JETInterpreter`

this function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
  following steps on each code block (`blk`) of `toplevelex`:
1. if `blk` is a `:module` expression, recusively call `virtual_process!` with an newly defined
     virtual module
2. `lower`s `blk` into `lwr` (including macro expansion)
3. replaces self-references of the original root module (that is represented as `actualmodsym`)
     with that of `virtualmod`: see `fix_self_references`
3. `ConcreteInterpreter` partially interprets some of `lwr`'s statements that should not be
     abstracted away (e.g. a `:method` definition); see also [`partial_interpret!`](@ref)
4. finally, `JETInterpreter` profiles the remaining statements by abstract interpretation

!!! warning
    the current approach splits entire code into code blocks and we're not tracking
      inter-code-block level dependencies, and so a partial interpretation of toplevle
      definitions will fail if it needs an access to global variables that are defined
      in the other code block
"""
function virtual_process! end

function virtual_process!(s::AbstractString,
                          filename::AbstractString,
                          virtualmod::Module,
                          actualmodsym::Symbol,
                          interp::JETInterpreter,
                          ret::VirtualProcessResult = gen_virtual_process_result(),
                          )::Tuple{VirtualProcessResult,JETInterpreter}
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

    return virtual_process!(toplevelex, filename, virtualmod, actualmodsym, interp, ret)
end

function virtual_process!(toplevelex::Expr,
                          filename::AbstractString,
                          virtualmod::Module,
                          actualmodsym::Symbol,
                          interp::JETInterpreter,
                          ret::VirtualProcessResult = gen_virtual_process_result(),
                          )::Tuple{VirtualProcessResult,JETInterpreter}
    @assert isexpr(toplevelex, :toplevel)

    filename::String = filename
    lnn::LineNumberNode = LineNumberNode(0, filename)
    interp::JETInterpreter = interp

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
    # function macroexpand_err_handler(err, st)
    #     # `4` corresponds to `with_err_handling`, `f`, `macroexpand` and its kwfunc
    #     st = crop_stacktrace(st, 4)
    #     push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
    #     return nothing
    # end
    # macroexpand_with_err_handling(mod, x) = with_err_handling(macroexpand_err_handler) do
    #     return macroexpand(mod, x)
    # end
    function eval_err_handler(err, st)
        # `3` corresponds to `with_err_handling`, `f` and `eval`
        st = crop_stacktrace(st, 3)
        push!(ret.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    eval_with_err_handling(mod, x) = with_err_handling(eval_err_handler) do
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

        # XXX: expand macros at this point ?
        # macro can essentially generate `:toplevel` and `:module` expressions

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

            virtual_process!(newtoplevelex, filename, newvirtualmod, actualmodsym, interp, ret)

            continue
        end

        blk = Expr(:block, lnn, x) # attach line number info
        lwr = lower_with_err_handling(virtualmod, blk)

        isnothing(lwr) && continue # error happened during lowering
        isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        fix_self_references!(src, actualmodsym, virtualmod)

        interp′ = ConcreteInterpreter(filename,
                                      lnn,
                                      eval_with_err_handling,
                                      virtualmod,
                                      actualmodsym,
                                      interp,
                                      ret,
                                      )
        concretized = @invokelatest partial_interpret!(interp′, virtualmod, src)

        # NOTE: needs to happen here since JuliaInterpreter.jl assumes there're `Symbol`s as
        # `GlobalRef`s in toplevel frames
        fix_global_symbols!(src, virtualmod)
        
        # bail out if nothing to profile (just a performance optimization)
        if all(is_return(last(src.code)) ? concretized[begin:end-1] : concretized)
            continue
        end

        interp = JETInterpreter(; # world age gets updated to take in newly added methods defined by `ConcreteInterpreter`
                                inf_params            = InferenceParams(interp),
                                opt_params            = OptimizationParams(interp),
                                optimize              = may_optimize(interp),
                                compress              = may_compress(interp),
                                discard_trees         = may_discard_trees(interp),
                                filter_native_remarks = interp.filter_native_remarks,
                                concretized           = concretized, # or construct partial `CodeInfo` from remaining abstract statements
                                )

        profile_toplevel!(interp, virtualmod, src)

        append!(ret.inference_error_reports, interp.reports) # collect error reports
    end

    return ret, interp
end

# replace self references of `actualmodsym` with `virtualmod` (as is)
function fix_self_references!(ex, actualmodsym, virtualmod)
    prewalk_and_transform!(ex) do x, scope
        if isa(x, Symbol)
            if x === actualmodsym
                return virtualmod
            end
        end

        return x
    end
end

# replace global `Symbol`s with `GlobalRef`s
function fix_global_symbols!(ex, mod)
    prewalk_and_transform!(ex) do x, scope
        # `_walk_and_transform!` doesn't recur into `QuoteNode`, so this apparently simple
        # approach just works
        if isa(x, Symbol)
            return GlobalRef(mod, x)
        end

        return x
    end
end

prewalk_and_transform!(args...) = walk_and_transform!(true, args...)
postwalk_and_transform!(args...) = walk_and_transform!(false, args...)
function walk_and_transform!(pre, f, @nospecialize(x), scope = Symbol[])
    x = pre ? f(x, scope) : x
    _walk_and_transform!(pre, f, x, scope)
    return pre ? x : f(x, scope)
end
function _walk_and_transform!(pre, f, ex::Expr, scope)
    push!(scope, ex.head)
    for (i, x) in enumerate(ex.args)
        ex.args[i] = walk_and_transform!(pre, f, x, scope)
    end
    pop!(scope)
end
function _walk_and_transform!(pre, f, src::CodeInfo, scope)
    for (i, x) in enumerate(src.code)
        src.code[i] = walk_and_transform!(pre, f, x, scope)
    end
end
_walk_and_transform!(pre, f, @nospecialize(_), scope) = return

"""
    partial_interpret!(interp, mod, src)

partially interprets statements in `src` using JuliaInterpreter.jl:
- concretize "toplevel definitions", i.e. `:method`, `:struct_type`, `:abstract_type` and
    `:primitive_type` expressions and their dependencies
- directly evaluates module usage expressions and report error of invalid module usages;
    or profile the package loading
- special case `include` calls so that [`virtual_process!`](@ref) recursively runs on the
    included file
"""
function partial_interpret!(interp, mod, src)
    concretize = select_statements(src)

    # LoweredCodeUtils.print_with_code(stdout, src, concretize)

    selective_eval_fromstart!(interp, Frame(mod, src), concretize, #= istoplevel =# true)

    return concretize
end

# select statements that should be not abstracted away, but rather actually interpreted
function select_statements(src)
    stmts = src.code
    edges = CodeEdges(src)

    concretize = fill(false, length(stmts))

    for (i, stmt) in enumerate(stmts)
        if begin
                ismethod(stmt)      || # don't abstract away method definitions
                istypedef(stmt)     || # don't abstract away type definitions
                ismoduleusage(stmt)    # module usages are handled by `ConcreteInterpreter`
            end
            concretize[i] = true
            continue
        end

        if isexpr(stmt, :(=))
            stmt = (stmt::Expr).args[2] # rhs
        end
        if isexpr(stmt, :call)
            f = (stmt::Expr).args[1]

            # special case `include` calls
            if f === :include
                concretize[i] = true
            end

            # analysis of `eval` calls are difficult, let's give up it and just evaluate
            # toplevel `eval` calls; they may contain toplevel definitions
            # adapted from
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L53
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L87-L88
            if f === :eval || (callee_matches(f, Base, :getproperty) && is_quotenode_egal(stmt.args[end], :eval))
                # statement `i` may be the equivalent of `f = Core.eval`, so require each
                # stmt that calls `eval` via `f(expr)`
                concretize[edges.succs[i]] .= true
                concretize[i] = true
            end
        end
    end

    lines_required!(concretize, src, edges)

    return concretize
end

"""
    ConcreteInterpreter

trait to inject code into JuliaInterpreter's interpretation process; JET.jl overloads:
- `JuliaInterpreter.step_expr!` to add error report pass for module usage expressions and
    support package profiling
- `JuliaInterpreter.evaluate_call_recurse!` to special case `include` calls
- `JuliaInterpreter.handle_err` to wrap an error happened during interpretation into
    `ActualErrorWrapped`
"""
struct ConcreteInterpreter
    filename::String
    lnn::LineNumberNode
    eval_with_err_handling::Function
    virtualmod::Module
    actualmodsym::Symbol
    interp::JETInterpreter
    ret::VirtualProcessResult
end

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    # TODO:
    # - support package profiling
    # - add report pass (report usage of undefined name, etc.)
    if ismoduleusage(node)
        for ex in to_single_usages(node)
            # NOTE: usages of virtual global variables also work here, since they are supposed
            # to be actually evaluated into `interp.virtualmod` (as `VirtualGlobalVariable`
            # object) at this point

            return interp.eval_with_err_handling(interp.virtualmod, ex)
        end
    end

    return @invoke step_expr!(interp, frame, node, istoplevel::Bool)
end

ismoduleusage(x) = isexpr(x, (:import, :using, :export))

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

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/2f5f80034bc287a60fe77c4e3b5a49a087e38f8b/src/interpret.jl#L188-L199
# works as `JuliaInterpreter.evaluate_call_compiled!`, but special cases for JET.jl added
function JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)
    # @assert !enter_generated
    pc = frame.pc
    ret = bypass_builtins(frame, call_expr, pc)
    isa(ret, Some{Any}) && return ret.value
    ret = maybe_evaluate_builtin(frame, call_expr, false)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    f = fargs[1]
    popfirst!(fargs)  # now it's really just `args`

    # @info "concretizing: " call_expr
    if isinclude(f)
        return handle_include(interp, fargs)
    else
        ret = f(fargs...)
        return ret
    end
end

isinclude(@nospecialize(_))           = false
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

    virtual_process!(include_text, include_file, interp.virtualmod, interp.actualmodsym, interp.interp, interp.ret)

    # TODO: actually, here we need to try to get the last profiling result of the `virtual_process!` call above
    return nothing
end

function JuliaInterpreter.handle_err(interp::ConcreteInterpreter, frame, err)
    # this error handler is only supposed to be called from toplevel frame
    data = frame.framedata
    @assert isempty(data.exception_frames) && !data.caller_will_catch_err

    # catch stack trace
    bt = catch_backtrace()
    st = stacktrace(bt)
    st = crop_stacktrace(st, 1) do frame
        # cut until the internal frame (i.e the one within this module or JuliaInterpreter)
        def = frame.linfo.def
        mod = isa(def, Method) ? def.module : def::Module
        return mod == JuliaInterpreter || mod == @__MODULE__
    end

    push!(interp.ret.toplevel_error_reports, ActualErrorWrapped(err,
                                                                st,
                                                                interp.filename,
                                                                interp.lnn.line
                                                                ))

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

function crop_stacktrace(pred, st, offset)
    i = findfirst(pred, st)
    return st[1:(isnothing(i) ? end : i - offset)]
end

function crop_stacktrace(st, offset)
    file = Symbol(@__FILE__)
    func = Symbol(with_err_handling)
    return crop_stacktrace(st, offset) do frame
        frame.file ===  file && frame.func === func
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

islnn(@nospecialize(_)) = false
islnn(::LineNumberNode) = true
