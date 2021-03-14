"""
    ToplevelConfig

Configurations for top-level analysis.
These configurations will be active for entries explained in [Analysis entry points](@ref).

---
- `concretization_patterns::Vector{Expr} = Expr[]` \\

  Also see: [`virtual_process!`](@ref).
---
"""
struct ToplevelConfig
    concretization_patterns::Vector{Expr}
    @jetconfigurable ToplevelConfig(; concretization_patterns::Vector{Expr} = Expr[],
                                      ) =
        return new(concretization_patterns,
                   )
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
                     config::ToplevelConfig,
                     ) -> VirtualProcessResult
    virtual_process!(toplevelex::Expr,
                     filename::AbstractString,
                     virtualmod::Module,
                     actualmodsym::Symbol,
                     interp::JETInterpreter,
                     config::ToplevelConfig,
                     ) -> VirtualProcessResult

Simulates Julia's toplevel execution and collects error points, and finally returns
`res::VirtualProcessResult`, which keeps the following information:
- `res.included_files::Set{String}`: files that have been analyzed
- `res.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `res.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `JETInterpreter`

This function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
  following steps on each code block (`blk`) of `toplevelex`:
1. if `blk` is a `:module` expression, recusively call `virtual_process!` with an newly defined
     virtual module
2. `lower`s `blk` into `:thunk` expression `lwr` (macros are also expanded in this step)
3. replaces self-references of the original root module (that is represented as `actualmodsym`)
     with that of `virtualmod`: see `fix_self_references`
3. `ConcreteInterpreter` partially interprets some statements in `lwr` that should not be
     abstracted away (e.g. a `:method` definition); see also [`partially_interpret!`](@ref)
4. finally, `JETInterpreter` analyzes the remaining statements by abstract interpretation

!!! warning
    In order to process the toplevel code sequentially as Julia runtime does, `virtual_process!`
      splits the entire code, and then iterate a simulation process on each code block.
    With this approach, we can't track the inter-code-block level dependencies, and so a
      partial interpretation of toplevle definitions will fail if it needs an access to global
      variables defined in other code blocks that are not interpreted but just abstracted.
    We can circumvent this issue using JET's `concretization_patterns` configuration, which
      allows us to customize JET's concretization strategy.
    See [`ToplevelConfig`](@ref) for more details.
"""
function virtual_process! end

function virtual_process!(s::AbstractString,
                          filename::AbstractString,
                          virtualmod::Module,
                          actualmodsym::Symbol,
                          interp::JETInterpreter,
                          config::ToplevelConfig,
                          res::VirtualProcessResult = gen_virtual_process_result(),
                          )::VirtualProcessResult
    start = time()

    with_toplevel_logger(interp) do io
        println(io, "entered into $filename")
    end

    push!(res.included_files, filename)

    toplevelex = parse_input_line(s; filename)

    if @isexpr(toplevelex, (:error, :incomplete))
        # if there's any syntax error, try to identify all the syntax error location
        append!(res.toplevel_error_reports, collect_syntax_errors(s, filename))
    elseif isnothing(toplevelex)
        # just return if there is nothing to analyze
    else
        res = virtual_process!(toplevelex, filename, virtualmod, actualmodsym, interp, config, res)
    end

    with_toplevel_logger(interp) do io
        sec = round(time() - start; digits = 3)
        println(io, " exited from $filename (took $sec sec)")
    end

    return res
end

function virtual_process!(toplevelex::Expr,
                          filename::AbstractString,
                          virtualmod::Module,
                          actualmodsym::Symbol,
                          interp::JETInterpreter,
                          config::ToplevelConfig,
                          res::VirtualProcessResult = gen_virtual_process_result(),
                          )::VirtualProcessResult
    @assert @isexpr(toplevelex, :toplevel)

    local lnn::LineNumberNode = LineNumberNode(0, filename)

    function macroexpand_err_handler(err, st)
        push!(res.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    # `scrub_offset = 4` corresponds to `with_err_handling` -> `f` -> `macroexpand` -> kwfunc (`macroexpand`)
    macroexpand_with_err_handling(mod, x) = with_err_handling(macroexpand_err_handler, #= scrub_offset =# 4) do
        # XXX we want to non-recursive, sequential partial macro expansion here, which allows
        # us to collect more fine-grained error reports within macro expansions
        # but it can lead to invalid macro hygiene escaping because of https://github.com/JuliaLang/julia/issues/20241
        return macroexpand(mod, x; recursive = true #= but want to use `false` here =#)
    end
    function eval_err_handler(err, st)
        push!(res.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `eval`
    eval_with_err_handling(mod, x) = with_err_handling(eval_err_handler, #= scrub_offset =# 3) do
        return Core.eval(mod, x)
    end
    function lower_err_handler(err, st)
        push!(res.toplevel_error_reports, ActualErrorWrapped(err, st, filename, lnn.line))
        return nothing
    end
    # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `lower`
    lower_with_err_handling(mod, x) = with_err_handling(lower_err_handler, #= scrub_offset =# 3) do
        lwr = lower(mod, x)

        # here we should capture syntax errors found during lowering
        if @isexpr(lwr, :error)
            msg = first(lwr.args)
            push!(res.toplevel_error_reports, SyntaxErrorReport("syntax: $(msg)", filename, lnn.line))
            return nothing
        end

        return lwr
    end

    # transform, and then analyze sequentially
    # IDEA the following code has some of duplicated work with `JuliaInterpreter.ExprSpliter` and we may want to factor them out
    exs = reverse(toplevelex.args)
    while !isempty(exs)
        x = pop!(exs)

        # with_toplevel_logger(interp, ≥(DEBUG_LOGGER_LEVEL)) do io
        #     println(io, "analyzing ", x)
        # end

        # update line info
        if isa(x, LineNumberNode)
            lnn = x
            continue
        end

        # we will end up lowering `x` later, but special case `macrocall`s and expand it here
        # this is because macros can arbitrarily generate `:toplevel` and `:module` expressions
        if @isexpr(x, :macrocall)
            newx = macroexpand_with_err_handling(virtualmod, x)
            # unless (toplevel) error happened during macro expansion, queue it and continue
            isnothing(newx) || push!(exs, newx)
            continue
        end

        # flatten container expression
        if @isexpr(x, :toplevel)
            append!(exs, reverse(x.args))
            continue
        end

        # handle `:module` definition and module usage;
        # should happen here because modules need to be loaded sequentially while
        # "toplevel definitions" inside of the loaded modules shouldn't be evaluated in a
        # context of `virtualmod`

        if @isexpr(x, :module)
            newblk = x.args[3]
            @assert @isexpr(newblk, :block)
            newtoplevelex = Expr(:toplevel, newblk.args...)

            x.args[3] = Expr(:block) # empty module's code body
            newvirtualmod = eval_with_err_handling(virtualmod, x)

            isnothing(newvirtualmod) && continue # error happened, e.g. duplicated naming

            virtual_process!(newtoplevelex, filename, newvirtualmod::Module, actualmodsym, interp, config, res)

            continue
        end

        blk = Expr(:block, lnn, x) # attach line number info
        lwr = lower_with_err_handling(virtualmod, blk)

        isnothing(lwr) && continue # error happened during lowering
        @isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        fix_self_references!(src, actualmodsym, virtualmod)

        interp′ = ConcreteInterpreter(filename,
                                      lnn,
                                      eval_with_err_handling,
                                      virtualmod,
                                      actualmodsym,
                                      interp,
                                      config,
                                      res,
                                      )
        concretized = partially_interpret!(interp′, virtualmod, src)

        # bail out if nothing to analyze (just a performance optimization)
        all(concretized) && continue

        interp = JETInterpreter(interp, concretized, virtualmod)

        analyze_toplevel!(interp, src)

        append!(res.inference_error_reports, interp.reports) # collect error reports
    end

    return res
end

# replace self references of `actualmodsym` with `virtualmod` (as is)
function fix_self_references!(ex, actualmodsym, virtualmod)
    function _fix_self_reference(x, scope)
        if isa(x, Symbol)
            if x === actualmodsym
                return virtualmod
            end
        elseif isa(x, GotoIfNot)
            newcond = _fix_self_reference(x.cond, scope)
            return GotoIfNot(newcond, x.dest)
        end

        return x
    end

    prewalk_and_transform!(_fix_self_reference, ex)
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

# # configure user-specified concretization strategy with pattern matching on surface level AST
# # `select_statements` will use and actually apply configuration using :force_concretize_(start|end) annotations
# function annotate_force_concretizations(@nospecialize(x), config::ToplevelConfig)
#     patterns = config.concretization_patterns
#     return MacroTools.postwalk(x) do @nospecialize(x)
#         for pat in patterns
#             if @capture(x, $pat)
#                 return Expr(:block,
#                             Expr(:meta, :force_concretize_start),
#                             Expr(:local, Expr(:(=), :ret, x)),
#                             Expr(:meta, :force_concretize_end),
#                             :ret)
#             end
#         end
#         return x
#     end
# end

"""
    ConcreteInterpreter

The trait to inject code into JuliaInterpreter's interpretation process; JET.jl overloads:
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
    config::ToplevelConfig
    res::VirtualProcessResult
end

"""
    partially_interpret!(interp::ConcreteInterpreter, mod::Module, src::CodeInfo)

Partially interprets statements in `src` using JuliaInterpreter.jl:
- concretizes "toplevel definitions", i.e. `:method`, `:struct_type`, `:abstract_type` and
    `:primitive_type` expressions and their dependencies
- concretizes user-specified toplevel code (see [`ToplevelConfig`](@ref))
- directly evaluates module usage expressions and report error of invalid module usages
  (TODO: enter into the loaded module and keep JET analysis)
- special-cases `include` calls so that [`virtual_process!`](@ref) recursively runs on the
    included file
"""
function partially_interpret!(interp::ConcreteInterpreter, mod::Module, src::CodeInfo)
    concretize = select_statements(src, interp.config)

    with_toplevel_logger(interp.interp, ≥(DEBUG_LOGGER_LEVEL)) do io
        println(io, "concretization plan:")
        LoweredCodeUtils.print_with_code(io, src, concretize)
    end

    selective_eval_fromstart!(interp, Frame(mod, src), concretize, #= istoplevel =# true)

    return concretize
end

# select statements that should be not abstracted away, but rather actually interpreted
function select_statements(src, config)
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

        # apply user-specified concretization strategy
        # currently our concretization strategy is configured here with expression pattern
        # matching on lowered representation; while it can be a bit user-unfriendly, but it
        # has several benefits over the pattern matching with surface level AST,
        # especially, here in lowered representation a function definition signature
        # (`f(args...)`) is clearly distinguished from the call expression while within
        # surface AST level we should care about the scope of the expression, etc.
        local force_concretize = false
        for pat in config.concretization_patterns
            if @capture(stmt, $pat)
                force_concretize = true
                continue
            end
        end
        if force_concretize
            concretize[i] = true
            continue
        end

        if @isexpr(stmt, :(=))
            lhs, rhs = stmt.args
            stmt = rhs
        end
        if @isexpr(stmt, :call)
            f = stmt.args[1]

            # special case `include` calls
            if f === :include
                concretize[i] = true
                continue
            end

            # analysis of `eval` calls are difficult, let's give up it and just evaluate
            # toplevel `eval` calls; they may contain toplevel definitions
            # adapted from
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L53
            # - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L87-L88
            if begin
                    f === :eval ||
                    (callee_matches(f, Base, :getproperty) && is_quotenode_egal(stmt.args[end], :eval)) ||
                    isa(f, GlobalRef) && f.name === :eval
                end
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

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    # TODO:
    # - support package profiling
    # - add report pass (report usage of undefined name, etc.)
    if ismoduleusage(node)
        for ex in to_single_usages(node::Expr)
            # NOTE: usages of abstract global variables also work here, since they are supposed
            # to be actually evaluated into `interp.virtualmod` (as `AbstractGlobal`
            # object) at this point

            interp.eval_with_err_handling(interp.virtualmod, ex)
        end
        return nothing
    end

    return @invoke step_expr!(interp, frame, node, istoplevel::Bool)
end

ismoduleusage(@nospecialize(x)) = @isexpr(x, (:import, :using, :export))

# assuming `ismoduleusage(x)` holds
function to_single_usages(x::Expr)
    if length(x.args) != 1
        # using A, B, export a, b
        return Expr.(x.head, x.args)
    else
        arg = first(x.args)
        if isa(arg, Symbol)
            # export a
            return [x]
        else
            if arg.head === :.
                # using A
                return [x]
            else
                # using A: sym1, sym2, ...
                @assert @isexpr(arg, :(:))
                a, as... = arg.args
                return Expr.(x.head, Expr.(arg.head, Ref(a), as))::Vector{Expr}
            end
        end
    end
end

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/2f5f80034bc287a60fe77c4e3b5a49a087e38f8b/src/interpret.jl#L188-L199
# works almost same as `JuliaInterpreter.evaluate_call_compiled!`, but also added special
# cases for JET analysis
function JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)
    # @assert !enter_generated
    pc = frame.pc
    ret = bypass_builtins(frame, call_expr, pc)
    isa(ret, Some{Any}) && return ret.value
    ret = @invokelatest maybe_evaluate_builtin(frame, call_expr, false)
    isa(ret, Some{Any}) && return ret.value
    fargs = collect_args(frame, call_expr)
    f = fargs[1]
    popfirst!(fargs)  # now it's really just `args`

    if isinclude(f)
        return handle_include(interp, fargs)
    else
        # `virtual_process!` iteratively interpret toplevel expressions but it doesn't hit toplevel
        # we may want to make `virtual_process!` hit the toplevel on each interation rather than
        # using `invokelatest` here, but assuming concretized calls are supposed only to be
        # used for other toplevel definitions and as such not so computational heavy,
        # I'd like to go with this simplest way
        return @invokelatest f(fargs...)
    end
end

isinclude(@nospecialize(_))           = false
isinclude(@nospecialize(f::Function)) = nameof(f) === :include

function handle_include(interp, fargs)
    filename = interp.filename
    res = interp.res
    lnn = interp.lnn
    eval_with_err_handling = interp.eval_with_err_handling
    virtualmod = interp.virtualmod

    # TODO: maybe find a way to handle two args `include` calls
    include_file = normpath(dirname(filename), first(fargs))

    # handle recursive `include`s
    if include_file in res.included_files
        report = RecursiveIncludeErrorReport(include_file, res.included_files, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end

    read_ex      = :(read($(include_file), String))
    include_text = eval_with_err_handling(virtualmod, read_ex)

    isnothing(include_text) && return nothing # typically no file error

    virtual_process!(include_text::String, include_file, interp.virtualmod, interp.actualmodsym, interp.interp, interp.config, interp.res)

    # TODO: actually, here we need to try to get the last profiling result of the `virtual_process!` call above
    return nothing
end

const JET_VIRTUALPROCESS_FILE = Symbol(@__FILE__)
const JULIAINTERPRETER_BUILTINS_FILE = let
    jlfile = pathof(JuliaInterpreter)::String
    Symbol(normpath(jlfile, "..", "builtins.jl"))
end

# handle errors from toplevel user code
function JuliaInterpreter.handle_err(interp::ConcreteInterpreter, frame, err)
    # catch stack trace
    bt = catch_backtrace()
    st = stacktrace(bt)

    # scrub the original stacktrace so that it only contains frames from user code
    i = 0
    for (j, frame) in enumerate(st)
        # if errors happen in `JuliaInterpreter.maybe_evaluate_builtin`, we just discard all
        # the stacktrace assuming they are enough self-explanatory (corresponding to the last logic below)
        if frame.file === JULIAINTERPRETER_BUILTINS_FILE && frame.func === :maybe_evaluate_builtin
            break # keep `i = 0`
        end

        # find an error frame that happened at `@invokelatest f(fargs...)` in the overload
        # `JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)`
        if frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :evaluate_call_recurse!
            i = j - 4 # offset: `evaluate_call_recurse` -> kwfunc (`evaluate_call_recurse`) -> `invokelatest` -> kwfunc (`invokelatest`)
            break
        end

        # other general errors may happen at `collect_args`, etc.
        # we don't show any stacktrace for those errors (by keeping the original `i = 0`)
        # since they are hopefully enough self-explanatory (XXX is it really so ?) and even
        # Julia's base error handler only shows something like `[1] top-level scope` in these cases
        continue
    end
    st = st[1:i]

    push!(interp.res.toplevel_error_reports,
          ActualErrorWrapped(err, st, interp.filename, interp.lnn.line))

    return nothing
end

function with_err_handling(f, err_handler, scrub_offset)
    return try
        f()
    catch err
        bt = catch_backtrace()
        st = stacktrace(bt)

        # scrub the original stacktrace so that it only contains frames from user code
        i = findfirst(st) do frame
            frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :with_err_handling
        end
        @assert i !== nothing
        st = st[1:(i - scrub_offset)]

        err_handler(err, st)
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
        report = @isexpr(ex, :error) ? SyntaxErrorReport(string("syntax: ", first(ex.args)), filename, line) :
                 @isexpr(ex, :incomplete) ? SyntaxErrorReport(first(ex.args), filename, line) :
                 nothing
        isnothing(report) || push!(reports, report)
        index = nextindex
    end
    return reports
end
