"""
Configurations for top-level analysis.
These configurations will be active for all the top-level entries explained in [Analysis entry points](@ref).

---
- `analyze_from_definitions::Bool = false` \\
  If `true`, JET will start analysis using signatures of top-level definitions (e.g. method signatures),
    after the top-level interpretation has been done (unless no serious top-level error has
    happened, like errors involved within a macro expansion).

  This is useful when you want to analyze a package, which usually contains only definitions
    but not top-level callsites.
  With this option, JET can enter analysis just with method or type definitions, and we don't
    need to pass a file that uses the target package.

  !!! warning
      This feature is very experimental at this point, and you may face lots of false positive
        errors, especially when trying to analyze a big package with lots of dependencies.
      If a file that contains top-level callsites (e.g. `test/runtests.jl`) is available,
        JET analysis entered from there will produce more accurate analysis results than
          using with this configuration.

  Also see: [`report_file`](@ref), [`report_and_watch_file`](@ref)
---
- `concretization_patterns::Vector{<:Any} = Expr[]` \\
  Specifies a customized top-level code concretization strategy.

  When analyzing a top-level code, JET first splits the entire code and then iterate a virtual
    top-level code execution process on each code block, in order to simulate Julia's sequential
    top-level code execution.
  However, with this approach, JET can't track the "inter-code-block" level dependencies, and
    so a partial interpretation of top-level definitions can fail if it needs an access to
    global variables defined in other code blocks that are not actually interpreted ("concretized")
    but just abstract-interpreted ("abstracted").

  For example, the issue happens when your macro accesses to a global variable during its expansion, e.g.:
  > test/fixtures/concretization_patterns.jl
  $(let
      text = read(normpath(@__DIR__, "..", "test", "fixtures", "concretization_patterns.jl"), String)
      lines = split(text, '\n')
      pushfirst!(lines, "```julia"); push!(lines, "```")
      join(lines, "\n  ")
  end)

  To circumvent this issue, JET offers the `concretization_patterns::Vector{<:Any}` configuration,
    which allows us to customize JET's top-level code concretization strategy.
  `concretization_patterns` specifies the _patterns of code_ that should be concretized.
  JET internally uses [MacroTools.jl's expression pattern match](https://fluxml.ai/MacroTools.jl/stable/pattern-matching/),
    and we can specify any expression pattern that is expected by `MacroTools.@capture` macro.
  For example, in order to solve the issue explained above, we can have:
  ```julia
  concretization_patterns = [:(GLOBAL_CODE_STORE = x_)]
  ```
  Please note that we must use `:(GLOBAL_CODE_STORE = x_)` rather than `:(const GLOBAL_CODE_STORE = x_)`.
  This is because currently the specified patterns will be matched against [the lowered code representation](https://juliadebug.github.io/JuliaInterpreter.jl/stable/ast/),
    in which `const x = y` has been lowered to the sequence of 1.) the declaration `const x`,
    2.) value computation `%2 = Dict()` and 3.) actual assignment part `x = %2`.
  Although this could be really tricky, we can effectively debug JET's top-level code concretization plan
    using [`JETLogger`](@ref)'s `toplevel_logger` with the logging level above than `$DEBUG_LOGGER_LEVEL` ("debug") level,
    where `t`-annotated statements will be concretize while `f`-annotated statements will be analyzed by abstract interpretation.
  ```julia
  julia> report_file("test/fixtures/concretization_patterns.jl";
                     concretization_patterns = [:(GLOBAL_CODE_STORE = x_)],
                     toplevel_logger = IOContext(stdout, :JET_LOGGER_LEVEL => 1))
  ```
  ```
  [toplevel-debug] entered into test/fixtures/concretization_patterns.jl
  [toplevel-debug] concretization plan:
  1 f 1 ─      const GLOBAL_CODE_STORE
  2 t │   %2 = Dict()
  3 t │        GLOBAL_CODE_STORE = %2
  4 f └──      return %2
  [toplevel-debug] concretization plan:
  1 f 1 ─      \$(Expr(:thunk, CodeInfo(
      @ none within `top-level scope'
  1 ─     return \$(Expr(:method, Symbol("@with_code_record")))
  )))
  2 t │        \$(Expr(:method, Symbol("@with_code_record")))
  3 t │   %3 = Core.Typeof(var"@with_code_record")
  4 t │   %4 = Core.svec(%3, Core.LineNumberNode, Core.Module, Core.Any)
  5 t │   %5 = Core.svec()
  6 t │   %6 = Core.svec(%4, %5, \$(QuoteNode(:(#= test/fixtures/concretization_patterns.jl:4 =#))))
  7 t │        \$(Expr(:method, Symbol("@with_code_record"), :(%6), CodeInfo(
      @ test/fixtures/concretization_patterns.jl:5 within `none'
  1 ─      \$(Expr(:meta, :nospecialize, :(a)))
  │        Base.setindex!(GLOBAL_CODE_STORE, a, __source__)
  │   @ test/fixtures/concretization_patterns.jl:6 within `none'
  │   %3 = esc(a)
  └──      return %3
  )))
  8 f └──      return var"@with_code_record"
  [toplevel-debug] concretization plan:
  1 f 1 ─      \$(Expr(:thunk, CodeInfo(
      @ none within `top-level scope'
  1 ─     return \$(Expr(:method, :foo))
  )))
  2 t │        \$(Expr(:method, :foo))
  3 t │   %3 = Core.Typeof(foo)
  4 t │   %4 = Core.svec(%3, Core.Any)
  5 t │   %5 = Core.svec()
  6 t │   %6 = Core.svec(%4, %5, \$(QuoteNode(:(#= test/fixtures/concretization_patterns.jl:11 =#))))
  7 t │        \$(Expr(:method, :foo, :(%6), CodeInfo(
      @ test/fixtures/concretization_patterns.jl:11 within `none'
  1 ─ %1 = identity(a)
  └──      return %1
  )))
  8 f └──      return foo
  [toplevel-debug] concretization plan:
  1 f 1 ─ %1 = foo(10)
  2 f └──      return %1
  [toplevel-debug]  exited from test/fixtures/concretization_patterns.jl (took 0.018 sec)
  ```

  Also see: [Logging Configurations](@ref), [`virtual_process`](@ref).
---
"""
struct ToplevelConfig
    analyze_from_definitions::Bool
    concretization_patterns::Vector{<:Any}
    @jetconfigurable ToplevelConfig(; analyze_from_definitions::Bool         = false,
                                      concretization_patterns::Vector{<:Any} = Expr[],
                                      ) =
        return new(analyze_from_definitions,
                   concretization_patterns,
                   )
end

const Actual2Virtual = Pair{Module,Module}

"""
    res::VirtualProcessResult

- `res.included_files::Set{String}`: files that have been analyzed
- `res.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `res.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `JETInterpreter`
- `res.toplevel_signatures`: signatures of methods defined within the analyzed files
- `res.actual2virtual::$Actual2Virtual`: keeps actual and virtual module
"""
const VirtualProcessResult = @NamedTuple begin
    included_files::Set{String}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
    toplevel_signatures::Vector{Type}
    actual2virtual::Actual2Virtual
end

function gen_virtual_process_result(actualmod, virtualmod)
    return (; included_files          = Set{String}(),
              toplevel_error_reports  = ToplevelErrorReport[],
              inference_error_reports = InferenceErrorReport[],
              toplevel_signatures     = Type[],
              actual2virtual          = Actual2Virtual(actualmod, virtualmod),
              )::VirtualProcessResult
end

gen_virtual_module(actualmod = Main) =
    Core.eval(actualmod, :(module $(gensym(:JETVirtualModule)) end))::Module

# TODO maybe we need an entry point where `context` can be directly specified, w/o going through the module virtualization

"""
    virtual_process(s::AbstractString,
                    filename::AbstractString,
                    actualmod::Module,
                    virtualmod::Module,
                    interp::JETInterpreter,
                    config::ToplevelConfig,
                    ) -> res::VirtualProcessResult

Simulates Julia's toplevel execution and collects error points, and finally returns $(@doc VirtualProcessResult)

This function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
  following steps on each code block (`blk`) of `toplevelex`:
1. if `blk` is a `:module` expression, recusively enters analysis into an newly defined
     virtual module
2. `lower`s `blk` into `:thunk` expression `lwr` (macros are also expanded in this step)
3. replaces self-references of the original root module (i.e. `actualmod`)
     with that of `virtualmod`: see `fix_self_references`
3. `ConcreteInterpreter` partially interprets some statements in `lwr` that should not be
     abstracted away (e.g. a `:method` definition); see also [`partially_interpret!`](@ref)
4. finally, `JETInterpreter` analyzes the remaining statements by abstract interpretation

!!! warning
    In order to process the toplevel code sequentially as Julia runtime does, `virtual_process`
      splits the entire code, and then iterate a simulation process on each code block.
    With this approach, we can't track the inter-code-block level dependencies, and so a
      partial interpretation of toplevle definitions will fail if it needs an access to global
      variables defined in other code blocks that are not interpreted but just abstracted.
    We can circumvent this issue using JET's `concretization_patterns` configuration, which
      allows us to customize JET's concretization strategy.
    See [`ToplevelConfig`](@ref) for more details.
"""
function virtual_process(x::Union{AbstractString,Expr},
                         filename::AbstractString,
                         actualmod::Module,
                         virtualmod::Module,
                         interp::JETInterpreter,
                         config::ToplevelConfig,
                         )
    res = _virtual_process!(x,
                            filename,
                            interp,
                            config,
                            virtualmod,
                            gen_virtual_process_result(actualmod, virtualmod),
                            )

    # analyze collected signatures unless critical error happened
    if config.analyze_from_definitions && isempty(res.toplevel_error_reports)
        analyze_from_definitions!(interp, res)
    end

    return res
end

function analyze_from_definitions!(interp::JETInterpreter, res::VirtualProcessResult)
    n = length(res.toplevel_signatures)
    succeeded = 0
    clearline(io) = print(io, '\r')
    for (i, tt) in enumerate(res.toplevel_signatures)
        mms = _methods_by_ftype(tt, -1, get_world_counter())
        isa(mms, Bool) && @goto failed

        filter!(mm::MethodMatch->mm.spec_types===tt, mms)
        if length(mms) == 1
            succeeded += 1
            with_toplevel_logger(interp; pre=clearline) do io
                (i == n ? println : print)(io, "analyzing from top-level definitions ... $succeeded/$n")
            end
            interp = JETInterpreter(interp, _CONCRETIZED, _TOPLEVELMOD)
            mm = first(mms)
            analyze_method_signature!(interp, mm.method, mm.spec_types, mm.sparams)
            append!(res.inference_error_reports, interp.reports)
            continue
        end

        @label failed
        with_toplevel_logger(interp, ≥(DEBUG_LOGGER_LEVEL); pre=clearline) do io
            println(io, "couldn't find a single method matching the signature `$tt`")
        end
    end
end

function _virtual_process!(s::AbstractString,
                           filename::AbstractString,
                           interp::JETInterpreter,
                           config::ToplevelConfig,
                           context::Module,
                           res::VirtualProcessResult,
                           )
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
        res = _virtual_process!(toplevelex, filename, interp, config, context, res)
    end

    with_toplevel_logger(interp) do io
        sec = round(time() - start; digits = 3)
        println(io, " exited from $filename (took $sec sec)")
    end

    return res
end

function _virtual_process!(toplevelex::Expr,
                           filename::AbstractString,
                           interp::JETInterpreter,
                           config::ToplevelConfig,
                           context::Module,
                           res::VirtualProcessResult,
                           )
    @assert @isexpr(toplevelex, :toplevel)

    local lnn::LineNumberNode = LineNumberNode(0, filename)

    function macroexpand_err_handler(err, st)
        report = is_missing_concretization(err) ?
                 MissingConcretization(err, st, filename, lnn.line) :
                 ActualErrorWrapped(err, st, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
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
        report = is_missing_concretization(err) ?
                 MissingConcretization(err, st, filename, lnn.line) :
                 ActualErrorWrapped(err, st, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end
    # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `eval`
    eval_with_err_handling(mod, x) = with_err_handling(eval_err_handler, #= scrub_offset =# 3) do
        return Core.eval(mod, x)
    end
    function lower_err_handler(err, st)
        report = is_missing_concretization(err) ?
                 MissingConcretization(err, st, filename, lnn.line) :
                 ActualErrorWrapped(err, st, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end
    # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `lower`
    lower_with_err_handling(mod, x) = with_err_handling(lower_err_handler, #= scrub_offset =# 3) do
        lwr = lower(mod, x)

        # here we should capture syntax errors found during lowering
        if @isexpr(lwr, :error)
            msg = first(lwr.args)
            push!(res.toplevel_error_reports, SyntaxErrorReport("syntax: \$(msg)", filename, lnn.line))
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
            newx = macroexpand_with_err_handling(context, x)
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
        # context of `context` module

        if @isexpr(x, :module)
            newblk = x.args[3]
            @assert @isexpr(newblk, :block)
            newtoplevelex = Expr(:toplevel, newblk.args...)

            x.args[3] = Expr(:block) # empty module's code body
            newcontext = eval_with_err_handling(context, x)

            isnothing(newcontext) && continue # error happened, e.g. duplicated naming

            _virtual_process!(newtoplevelex, filename, interp, config, newcontext::Module, res)

            continue
        end

        blk = Expr(:block, lnn, x) # attach line number info
        lwr = lower_with_err_handling(context, blk)

        isnothing(lwr) && continue # error happened during lowering
        @isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        fix_self_references!(src, res.actual2virtual)

        interp′ = ConcreteInterpreter(filename,
                                      lnn,
                                      eval_with_err_handling,
                                      context,
                                      interp,
                                      config,
                                      res,
                                      )
        concretized = partially_interpret!(interp′, context, src)

        # bail out if nothing to analyze (just a performance optimization)
        all(concretized) && continue

        interp = JETInterpreter(interp, concretized, context)

        analyze_toplevel!(interp, src)

        append!(res.inference_error_reports, interp.reports) # collect error reports
    end

    return res
end

# replace self references of `actualmod` with `virtualmod` (as is)
function fix_self_references!(src, (actualmod, virtualmod))
    actualmodsym   = Symbol(actualmod)
    virtualmodsyms = Symbol.(split(string(virtualmod), '.'))

    # we can't use `Module` object in module usage expression, so we need to replace it with
    # its symbolic representation
    function fix_modpath(modpath)
        ret = Symbol[]
        for s in modpath
            if s === virtualmod # postwalk, so we compare to `virtualmod`
                push!(ret, virtualmodsyms...)
            else
                push!(ret, s)
            end
        end
        return ret
    end

    fix_module_usage(head, modpath) =
        Expr(head, Expr(:., fix_modpath(modpath)...))
    fix_module_usage_specific(head, modpath, names) =
        Expr(head, Expr(:(:), Expr(:., fix_modpath(modpath)...), Expr(:., names...)))

    function fix_simple_module_usage(usage)
        return @capture(usage, import modpath__) ? fix_module_usage(:import, modpath) :
               @capture(usage, using modpath__) ? fix_module_usage(:using, modpath) :
               @capture(usage, import modpath__: names__) ? fix_module_usage_specific(:import, modpath, names) :
               @capture(usage, using modpath__: names__) ? fix_module_usage_specific(:using, modpath, names) :
               usage # :export
    end

    function fix_module_usage(usage)
        head = usage.head
        ret = Expr(head)
        for simple in to_simple_module_usages(usage)
            fixed = fix_simple_module_usage(simple)
            @assert fixed.head === head && length(fixed.args) == 1
            push!(ret.args, first(fixed.args))
        end
        return ret
    end

    return postwalk_and_transform!(src) do x, scope
        if ismoduleusage(x)
            return fix_module_usage(x)
        elseif x === actualmodsym
            return virtualmod
        end
        return x
    end
end

postwalk_and_transform!(f, x, scope = Symbol[]) =
    walk_and_transform!(x, x->postwalk_and_transform!(f, x, scope), f, scope)
prewalk_and_transform!(f, x, scope = Symbol[]) =
    walk_and_transform!(f(x, scope), x->prewalk_and_transform!(f, x, scope), (x,scope)->x, scope)

walk_and_transform!(@nospecialize(x), inner, outer, scope) = outer(x, scope)
function walk_and_transform!(node::GotoIfNot, inner, outer, scope)
    push!(scope, :gotoifnot)
    cond = inner(walk_and_transform!(node.cond, inner, outer, scope))
    dest = inner(walk_and_transform!(node.dest, inner, outer, scope))
    pop!(scope)
    return outer(GotoIfNot(cond, dest), scope)
end
function walk_and_transform!(ex::Expr, inner, outer, scope)
    push!(scope, ex.head)
    for (i, x) in enumerate(ex.args)
        ex.args[i] = inner(walk_and_transform!(x, inner, outer, scope))
    end
    pop!(scope)
    return outer(ex, scope)
end
function walk_and_transform!(src::CodeInfo, inner, outer, scope)
    for (i, x) in enumerate(src.code)
        src.code[i] = inner(walk_and_transform!(x, inner, outer, scope))
    end
    return outer(src, scope)
end

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
    support package analysis
- `JuliaInterpreter.evaluate_call_recurse!` to special case `include` calls
- `JuliaInterpreter.handle_err` to wrap an error happened during interpretation into
    `ActualErrorWrapped`
"""
struct ConcreteInterpreter
    filename::String
    lnn::LineNumberNode
    eval_with_err_handling::Function
    context::Module
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
- special-cases `include` calls so that top-level analysis recursively enters the included file
"""
function partially_interpret!(interp::ConcreteInterpreter, mod::Module, src::CodeInfo)
    concretize = select_statements(src, interp.config)

    with_toplevel_logger(interp.interp, ≥(DEBUG_LOGGER_LEVEL)) do io
        println(io, "concretization plan:")
        print_with_code(io, src, concretize)
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
                force_concretize = concretize[i] = true
                break
            end
        end
        force_concretize && continue

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

    norequire = BitSet()

    # exclude blocks without any definitions
    dependencies = BitSet()
    blocks = compute_basic_blocks(stmts).blocks
    has_definition(r) = any(view(concretize, r))
    changed::Bool = true
    while changed
        changed = false
        for (ibb, bb) in enumerate(blocks)
            if has_definition(rng(bb))
                for i in bb.preds
                    if i ∉ dependencies
                        push!(dependencies, i)
                        changed = true
                    end
                end
            end
        end
    end
    for (ibb, bb) in enumerate(blocks)
        r = rng(bb)
        if !has_definition(r) && ibb ∉ dependencies
            pushall!(norequire, r)
        end

        # here we try to ignore try/catch control flow
        # LoweredCodeUtils's control flow traversal starts from last statement of each basic block,
        # and so we mark `norequire` it unless it's involved with a loop
        i = Core.Compiler.last(bb.stmts)
        if !(is_goto(stmts[i]) || concretize[i])
            push!(norequire, i)
        end
    end

    # exclude ignore try/catch statements
    for (i,x) in enumerate(stmts)
        if is_trycatch(x)
            push!(norequire, i)
        end
    end

    lines_required!(concretize, src, edges; norequire)

    return concretize
end

is_goto(@nospecialize(x)) = isa(x, GotoNode) || isa(x, GotoIfNot)

function is_trycatch(@nospecialize(x))
    if isa(x, Expr)
        @isexpr(x, :enter) && return true
        @isexpr(x, :leave) && return true
        @isexpr(x, :pop_exception) && return true
        if @isexpr(x, :(=))
            @isexpr(x.args[2], :the_exception) && return true
        end
    end
    return false
end

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    # TODO:
    # - support package analysis
    # - add report pass (report usage of undefined name, etc.)
    if ismoduleusage(node)
        for ex in to_simple_module_usages(node::Expr)
            # NOTE: usages of abstract global variables also work here, since they are supposed
            # to be actually evaluated into `interp.context` (as `AbstractGlobal`
            # object) at this point

            interp.eval_with_err_handling(interp.context, ex)
        end
        return nothing
    end

    res = @invoke step_expr!(interp, frame, node, istoplevel::Bool)

    interp.config.analyze_from_definitions && collect_toplevel_signature!(interp, frame, node)

    return res
end

function collect_toplevel_signature!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node))
    if @isexpr(node, :method, 3)
        sigs = node.args[2]
        atype_params, sparams, _ = @lookup(frame, sigs)::SimpleVector
        # t = atype_params[1]
        # if isdefined(t, :name)
        #     # XXX ignore constructor methods, just because it can lead to false positives ...
        #     t.name === CC._TYPE_NAME && return
        # end
        atype = Tuple{(atype_params::SimpleVector)...}
        push!(interp.res.toplevel_signatures, atype)
    end
end

ismoduleusage(@nospecialize(x)) = @isexpr(x, (:import, :using, :export))

# assuming `ismoduleusage(x)` holds
function to_simple_module_usages(x::Expr)
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
        # `_virtual_process!` iteratively interpret toplevel expressions but it doesn't hit toplevel
        # we may want to make `_virtual_process!` hit the toplevel on each interation rather than
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
    context = interp.context

    # TODO: maybe find a way to handle two args `include` calls
    include_file = normpath(dirname(filename), first(fargs))

    # handle recursive `include`s
    if include_file in res.included_files
        report = RecursiveIncludeErrorReport(include_file, res.included_files, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end

    read_ex      = :(read($(include_file), String))
    include_text = eval_with_err_handling(context, read_ex)

    isnothing(include_text) && return nothing # typically no file error

    _virtual_process!(include_text::String, include_file, interp.interp, interp.config, context, interp.res)

    # TODO: actually, here we need to try to get the lastly analyzed result of the `_virtual_process!` call above
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

    report = is_missing_concretization(err) ?
             MissingConcretization(err, st, interp.filename, interp.lnn.line) :
             ActualErrorWrapped(err, st, interp.filename, interp.lnn.line)
    push!(interp.res.toplevel_error_reports, report)

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

function is_missing_concretization(@nospecialize(err))
    io = IOBuffer()
    showerror(io, err)
    occursin(string(AbstractGlobal), String(take!(io)))
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
