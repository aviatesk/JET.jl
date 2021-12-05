"""
    ToplevelErrorReport

An interface type of error reports that JET collects while top-level concrete interpration.
All `ToplevelErrorReport` should have the following fields:
- `file::String`: the path to the file containing the interpretation context
- `line::Int`: the line number in the file containing the interpretation context

See also: [`virtual_process`](@ref), [`ConcreteInterpreter`](@ref)
"""
abstract type ToplevelErrorReport end

# `ToplevelErrorReport` interface
function Base.getproperty(er::ToplevelErrorReport, sym::Symbol)
    return if sym === :file
        getfield(er, sym)::String
    elseif sym === :line
        getfield(er, sym)::Int
    else
        getfield(er, sym) # fallback
    end
end

"""
Configurations for top-level analysis.
These configurations will be active for all the top-level entries explained in the
[top-level analysis entry points](@ref jetanalysis-toplevel-entry) section.

---
- `context::Bool = Main` \\
  The module context in which the top-level execution will be simulated.

  This configuration can be useful when you just want to analyze a submodule, without
  starting entire analysis from the root module.
  For example, we can analyze `Base.Math` like below:
  ```julia-repl
  julia> report_file(JET.fullbasepath("math.jl");
                     context = Base,                  # `Base.Math`'s root module
                     analyze_from_definitions = true, # there're only definitions in `Base`
                     )
  ```

  Note that this module context will be virtualized by default so that JET can repeat analysis
  in the same session without having "invalid redefinition of constant ..." error etc.
  In other word, JET virtualize the module context of `context` and make sure the original
  module context isn't polluted by JET.
---
- `target_defined_modules::Bool = false` \\
  If `true`, automatically set the [`target_modules`](@ref result-config) configuration so that
  JET filters out errors that are reported within modules that JET doesn't analyze directly.
---
- `analyze_from_definitions::Bool = false` \\
  If `true`, JET will start analysis using signatures of top-level definitions (e.g. method signatures),
  after the top-level interpretation has been done (unless no serious top-level error has
  happened, like errors involved within a macro expansion).

  This can be handy when you want to analyze a package, which usually contains only definitions
  but not their usages (i.e. top-level callsites).
  With this option, JET can enter analysis just with method or type definitions, and we don't
  need to pass a file that uses the target package.

  !!! warning
      This feature is very experimental at this point, and you may face lots of false positive
      errors, especially when trying to analyze a big package with lots of dependencies.
      If a file that contains top-level callsites (e.g. `test/runtests.jl`) is available,
      JET analysis using the file is generally preferred, since analysis entered from
      concrete call sites will produce more accurate results than analysis entered from
      (maybe not concrete-typed) method signatures.

  Also see: [`report_file`](@ref), [`report_and_watch_file`](@ref)
---
- `concretization_patterns::Vector{<:Any} = Expr[]` \\
  Specifies a customized top-level code concretization strategy.

  When analyzing a top-level code, JET first splits the entire code into appropriate units
  of code (i.e. code blocks), and then iterate a virtual top-level code execution process
  on each code block in order to simulate Julia's sequential top-level code execution.
  In virtual code execution, JET will selectively interpret "top-level definitions" (like a function definition)
  just like Julia's top-level code execution, while it tries to avoid executing any other
  parts of code like function calls and leaves them to succeeding static analysis by
  abstract interpretation.

  However, currently, JET doesn't track the "inter-code-block" level code dependencies, and
  so the selective interpretation of top-level definitions can fail if it needs an access to
  global variables defined in other code blocks that are not actually interpreted (i.e. "concretized")
  but just left for abstract interpreation (i.e. "abstracted").

  For example, the issue happens when your macro accesses to a global variable during its expansion, e.g.:
  > test/fixtures/concretization_patterns.jl
  $(let
      text = read(normpath(@__DIR__, "..", "..", "test", "fixtures", "concretization_patterns.jl"), String)
      lines = split(text, '\n')
      pushfirst!(lines, "```julia"); push!(lines, "```")
      join(lines, "\n  ")
  end)

  To circumvent this issue, JET offers the `concretization_patterns::Vector{<:Any}` configuration,
  which allows us to customize JET's top-level code concretization strategy.
  `concretization_patterns` specifies the _patterns of code_ that should be concretized.
  To put in other word, when JET sees a code that matches any of code patterns specified by
  an user, JET will try to interpret and concretize the code, regardless of whether JET's
  code selection logic decides to concretize it or not.

  JET uses [MacroTools.jl's expression pattern match](https://fluxml.ai/MacroTools.jl/stable/pattern-matching/),
  and we can specify whatever code pattern expected by `MacroTools.@capture` macro.
  For example, in order to solve the issue explained above, we can have:
  ```julia
  concretization_patterns = [:(const GLOBAL_CODE_STORE = Dict())]
  ```
  Then `GLOBAL_CODE_STORE` will just be concretized and so any top-level error won't happen
  at the macro expansion.

  Although configuring `concretization_patterns` properly could be really tricky, we can
  effectively debug JET's top-level code concretization plan using [`toplevel_logger`](@ref JETLogger)
  configuration with the logging level above than `$DEBUG_LOGGER_LEVEL` ("debug") level.
  With the `toplevel_logger` configuration, we can see:
  - which code is matched with `concretization_patterns` and forcibly concretized
  - which code is selected to be concretized or not by JET's code selection logic:
    where `t`-annotated statements are concretized while `f`-annotated statements are abstracted
    and left abstract interpretation
  ```julia-repl
  julia> report_file("test/fixtures/concretization_patterns.jl";
                     concretization_patterns = [:(const GLOBAL_CODE_STORE = Dict())],
                     toplevel_logger = IOContext(stdout, :JET_LOGGER_LEVEL => 1))
  ```
  ```
  [toplevel-debug] virtualized the context of Main (took 0.003 sec)
  [toplevel-debug] entered into test/fixtures/concretization_patterns.jl
  [toplevel-debug] concretization pattern `const GLOBAL_CODE_STORE = Dict()` matched `const GLOBAL_CODE_STORE = Dict()` at test/fixtures/concretization_patterns.jl:2
  [toplevel-debug] concretization plan at test/fixtures/concretization_patterns.jl:4:
  1 f 1 ─      \$(Expr(:thunk, CodeInfo(
      @ none within `top-level scope`
  1 ─     return \$(Expr(:method, Symbol("@with_code_record")))
  )))
  2 t │        \$(Expr(:method, Symbol("@with_code_record")))
  3 t │   %3 = Core.Typeof(var"@with_code_record")
  4 t │   %4 = Core.svec(%3, Core.LineNumberNode, Core.Module, Core.Any)
  5 t │   %5 = Core.svec()
  6 t │   %6 = Core.svec(%4, %5, \$(QuoteNode(:(#= test/fixtures/concretization_patterns.jl:4 =#))))
  7 t │        \$(Expr(:method, Symbol("@with_code_record"), :(%6), CodeInfo(
      @ test/fixtures/concretization_patterns.jl:5 within `none`
  1 ─      \$(Expr(:meta, :nospecialize, :(a)))
  │        Base.setindex!(GLOBAL_CODE_STORE, a, __source__)
  │   @ test/fixtures/concretization_patterns.jl:6 within `none`
  │   %3 = esc(a)
  └──      return %3
  )))
  8 f └──      return var"@with_code_record"
  [toplevel-debug] concretization plan at test/fixtures/concretization_patterns.jl:11:
  1 f 1 ─      \$(Expr(:thunk, CodeInfo(
      @ none within `top-level scope`
  1 ─     return \$(Expr(:method, :foo))
  )))
  2 t │        \$(Expr(:method, :foo))
  3 t │   %3 = Core.Typeof(foo)
  4 t │   %4 = Core.svec(%3, Core.Any)
  5 t │   %5 = Core.svec()
  6 t │   %6 = Core.svec(%4, %5, \$(QuoteNode(:(#= test/fixtures/concretization_patterns.jl:11 =#))))
  7 t │        \$(Expr(:method, :foo, :(%6), CodeInfo(
      @ test/fixtures/concretization_patterns.jl:11 within `none`
  1 ─ %1 = identity(a)
  └──      return %1
  )))
  8 f └──      return foo
  [toplevel-debug] concretization plan at test/fixtures/concretization_patterns.jl:13:
  1 f 1 ─ %1 = foo(10)
  2 f └──      return %1
  [toplevel-debug]  exited from test/fixtures/concretization_patterns.jl (took 0.032 sec)
  ```

  Also see: [JET's logging configurations](@ref logging-config), [`virtual_process`](@ref).
---
- `virtualize::Bool = true` \\
  When `true`, JET will virtualize the given root module context.

  This configuration is supposed to be used only for testing or debugging.
  See [`virtualize_module_context`](@ref) for the internal.
---
"""
struct ToplevelConfig{CP<:Any}
    context::Module
    analyze_from_definitions::Bool
    concretization_patterns::Vector{CP}
    virtualize::Bool
    @jetconfigurable function ToplevelConfig(; context::Module                    = Main,
                                               analyze_from_definitions::Bool     = false,
                                               concretization_patterns::Vector{T} = Expr[],
                                               virtualize::Bool                   = true,
                                               ) where {T<:Any}
        if typeintersect(T, Expr) === Bottom
            CP = tmerge(T, Expr)
            concretization_patterns = convert(Vector{CP}, concretization_patterns)
        else
            CP = T
        end
        push!(concretization_patterns,
              # `@enum` macro is fairly complex and especially the `let insts = (Any[ $(esc(typename))(v) for v in $values ]...,)`
              # (adapted from https://github.com/JuliaLang/julia/blob/e5d7ef01b06f44cb75c871e54c81eb92eceae738/base/Enums.jl#L198)
              # part requires the statement selection logic to choose statements that only
              # pushes elements (`v`) into a slot representing an array (`insts`),
              # which is very hard to be generalized;
              # here we add them as pre-defined concretization patterns and make sure
              # false postive top-level errors won't happen by the macro expansion
              :(@enum(args__)), :(Base.@enum(args__)),
              )
        concretization_patterns = CP[striplines(normalise(x)) for x in concretization_patterns]
        return new{CP}(context,
                       analyze_from_definitions,
                       concretization_patterns,
                       virtualize,
                       )
    end
end

const Actual2Virtual = Pair{Module,Module}

"""
    res::VirtualProcessResult

- `res.included_files::Set{String}`: files that have been analyzed
- `res.defined_modules::Set{Module}`: module contexts created while this top-level analysis
- `res.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `res.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `AbstractAnalyzer`
- `res.toplevel_signatures`: signatures of methods defined within the analyzed files
- `res.actual2virtual::$Actual2Virtual`: keeps actual and virtual module
"""
struct VirtualProcessResult
    included_files::Set{String}
    defined_modules::Set{Module}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
    toplevel_signatures::Vector{Type}
    actual2virtual::Union{Actual2Virtual,Nothing}
end

function VirtualProcessResult(actual2virtual, context)
    return VirtualProcessResult(Set{String}(),
                                Set{Module}((context,)),
                                ToplevelErrorReport[],
                                InferenceErrorReport[],
                                Type[],
                                actual2virtual,
                                )
end

"""
    virtual_process(s::AbstractString,
                    filename::AbstractString,
                    analyzer::AbstractAnalyzer,
                    config::ToplevelConfig,
                    ) -> res::VirtualProcessResult

Simulates Julia's toplevel execution and collects error points, and finally returns $(@doc VirtualProcessResult)

This function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
following steps on each code block (`blk`) of `toplevelex`:
1. if `blk` is a `:module` expression, recusively enters analysis into an newly defined
   virtual module
2. `lower`s `blk` into `:thunk` expression `lwr` (macros are also expanded in this step)
3. if the context module is virtualized, replaces self-references of the original context
   module with virtualized one: see `fix_self_references`
4. `ConcreteInterpreter` partially interprets some statements in `lwr` that should not be
   abstracted away (e.g. a `:method` definition); see also [`partially_interpret!`](@ref)
5. finally, `AbstractAnalyzer` analyzes the remaining statements by abstract interpretation

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
                         analyzer::AbstractAnalyzer,
                         config::ToplevelConfig,
                         )
    if config.virtualize
        actual  = config.context

        start = time()
        virtual = virtualize_module_context(actual)
        with_toplevel_logger(analyzer) do @nospecialize(io)
            sec = round(time() - start; digits = 3)
            println(io, "virtualized the context of $actual (took $sec sec)")
        end

        actual2virtual = Actual2Virtual(actual, virtual)
        context = virtual
    else
        actual2virtual = nothing
        context = config.context
    end

    res = _virtual_process!(x,
                            filename,
                            analyzer,
                            config,
                            context,
                            VirtualProcessResult(actual2virtual, context),
                            )

    # analyze collected signatures unless critical error happened
    if config.analyze_from_definitions && isempty(res.toplevel_error_reports)
        analyze_from_definitions!(analyzer, res)
    end

    # TODO want to do in `analyze_from_definitions!` ?
    unique!(aggregation_policy(analyzer), res.inference_error_reports)

    return res
end

"""
    virtualize_module_context(actual::Module)

HACK: Returns a module where the context of `actual` is virtualized.

The virtualization will be done by 2 steps below:
1. loads the module context of `actual` into a sandbox module, and export the whole context from there
2. then uses names exported from the sandbox

This way, JET's runtime simulation in the virtual module context will be able to define
a name that is already defined in `actual` without causing
"cannot assign a value to variable ... from module ..." error, etc.
It allows JET to virtualize the context of already-existing module other than `Main`.

!!! warning "TODO"
    Currently this function relies on `Base.names`, and thus it can't restore the `using`ed
    names.
"""
function virtualize_module_context(actual::Module)
    modpath = split_module_path(actual)

    if length(modpath) ≥ 2
        if modpath[1] === :Main && modpath[2] === :anonymous
            error(ArgumentError("can't virtualize an anonymous module"))
        end
    end

    virtual = gen_virtual_module(actual)
    sandbox = gen_virtual_module(actual; name = :JETSandboxModule)

    uex = Expr(:(:), Expr(:., :., :., modpath...))
    usage = Expr(:using, uex)
    exprt = Expr(:export)
    unames = uex.args
    enames = exprt.args
    for n in names(actual; all = true, imported = true)
        isdefined(sandbox, n) && continue
        isdefined(actual, n) && push!(unames, Expr(:., n)) # an exported name can be undefined, and `using` of it will throw otherwise

        push!(enames, n)
    end
    Core.eval(sandbox, usage)
    Core.eval(sandbox, exprt)

    usage = Expr(:using, Expr(:., :., :., split_module_path(sandbox)...))
    Core.eval(virtual, usage)

    return virtual
end

const VIRTUAL_MODULE_NAME = :JETVirtualModule

gen_virtual_module(root = Main; name = VIRTUAL_MODULE_NAME) =
    Core.eval(root, :(module $(gensym(name)) end))::Module

# NOTE when `@generated` function has been defined, signatures of both its entry and
# generator should have been collected, and we will just analyze them separately
# if code generation has failed given the entry method signature, the overload of
# `InferenceState(..., ::AbstractAnalyzer)` will collect `GeneratorErrorReport`
function analyze_from_definitions!(analyzer::AbstractAnalyzer, res::VirtualProcessResult)
    n = length(res.toplevel_signatures)
    succeeded = 0
    for (i, tt) in enumerate(res.toplevel_signatures)
        mms = _methods_by_ftype(tt, -1, get_world_counter())
        if isa(mms, Vector{Any})
            filter!(mm::MethodMatch->mm.spec_types===tt, mms)
            if length(mms) == 1
                succeeded += 1
                with_toplevel_logger(analyzer; pre=clearline) do @nospecialize(io)
                    (i == n ? println : print)(io, "analyzing from top-level definitions ... $succeeded/$n")
                end
                analyzer = AbstractAnalyzer(analyzer, _CONCRETIZED, _TOPLEVELMOD)
                analyzer, result = analyze_method!(
                    analyzer, (first(mms)::MethodMatch).method;
                    # JETAnalyzer{BasicPass}: don't report errors unless this frame is concrete
                    set_entry = false)
                append!(res.inference_error_reports, get_reports(result))
                continue
            end
        end
        # something went wrong
        with_toplevel_logger(analyzer, ≥(DEBUG_LOGGER_LEVEL); pre=clearline) do @nospecialize(io)
            println(io, "couldn't find a single method matching the signature `$tt`")
        end
    end
    return nothing
end
clearline(io) = print(io, '\r')

function _virtual_process!(s::AbstractString,
                           filename::AbstractString,
                           analyzer::AbstractAnalyzer,
                           config::ToplevelConfig,
                           context::Module,
                           res::VirtualProcessResult,
                           )
    start = time()

    with_toplevel_logger(analyzer) do @nospecialize(io)
        println(io, "entered into $filename")
    end

    push!(res.included_files, filename)

    toplevelex = parse_input_line(s; filename)

    if isexpr(toplevelex, (:error, :incomplete))
        # if there's any syntax error, try to identify all the syntax error location
        append!(res.toplevel_error_reports, collect_syntax_errors(s, filename))
    elseif isnothing(toplevelex)
        # just return if there is nothing to analyze
    else
        res = _virtual_process!(toplevelex, filename, analyzer, config, context, res)
    end

    with_toplevel_logger(analyzer) do @nospecialize(io)
        sec = round(time() - start; digits = 3)
        println(io, " exited from $filename (took $sec sec)")
    end

    return res
end

function _virtual_process!(toplevelex::Expr,
                           filename::AbstractString,
                           analyzer::AbstractAnalyzer,
                           config::ToplevelConfig,
                           context::Module,
                           res::VirtualProcessResult,
                           force_concretize::Bool = false,
                           )
    @assert isexpr(toplevelex, :toplevel)

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
        if isexpr(lwr, :error)
            msg = first(lwr.args)
            push!(res.toplevel_error_reports, SyntaxErrorReport("syntax: $msg", filename, lnn.line))
            return nothing
        end

        return lwr
    end

    # transform, and then analyze sequentially
    # IDEA the following code has some of duplicated work with `JuliaInterpreter.ExprSpliter` and we may want to factor them out
    exs = push_vex_stack!(VExpr[], toplevelex, force_concretize)
    while !isempty(exs)
        (; x, force_concretize) = pop!(exs)

        # with_toplevel_logger(analyzer, ≥(DEBUG_LOGGER_LEVEL)) do @nospecialize(io)
        #     println(io, "analyzing ", x)
        # end

        # update line info
        if isa(x, LineNumberNode)
            lnn = x
            continue
        end

        # apply user-specified concretization strategy, which is configured as expression
        # pattern match on surface level AST code representation; if any of the specified
        # patterns matches `x`, JET just concretizes everything involved with it
        # since patterns are expected to work on surface level AST, we should configure it
        # here before macro expansion and lowering
        for pat in config.concretization_patterns
            if @capture(x, $pat)
                with_toplevel_logger(analyzer, ≥(DEBUG_LOGGER_LEVEL)) do @nospecialize(io)
                    line, file = lnn.line, lnn.file
                    x′ = striplines(normalise(x))
                    println(io, "concretization pattern `$pat` matched `$x′` at $file:$line")
                end
                force_concretize = true
                break
            end
        end

        # we will end up lowering `x` later, but special case `macrocall`s and expand it here
        # this is because macros can arbitrarily generate `:toplevel` and `:module` expressions
        if isexpr(x, :macrocall)
            newx = macroexpand_with_err_handling(context, x)

            # if any error happened during macro expansion, bail out now and continue
            isnothing(newx) && continue

            # special case and flatten the resulting expression expanded from `@doc` macro
            # the macro expands to a block expression and so it makes it difficult to specify
            # concretization pattern correctly since `@doc` macro is attached implicitly
            if first(x.args) === GlobalRef(Core, Symbol("@doc"))
                # `@doc` macro usually produces :block expression, but may also produce :toplevel
                # one when attached to a module expression
                @assert isexpr(newx, :block) || isexpr(newx, :toplevel)
                push_vex_stack!(exs, newx::Expr, force_concretize)
            else
                push!(exs, VExpr(newx, force_concretize))
            end

            continue
        end

        # flatten container expression
        if isexpr(x, :toplevel)
            push_vex_stack!(exs, x, force_concretize)
            continue
        end

        # handle `:module` definition and module usage;
        # should happen here because modules need to be loaded sequentially while
        # "toplevel definitions" inside of the loaded modules shouldn't be evaluated in a
        # context of `context` module

        if isexpr(x, :module)
            newblk = x.args[3]
            @assert isexpr(newblk, :block)
            newtoplevelex = Expr(:toplevel, newblk.args...)

            x.args[3] = Expr(:block) # empty module's code body
            newcontext = eval_with_err_handling(context, x)

            isnothing(newcontext) && continue # error happened, e.g. duplicated naming

            newmod = newcontext::Module
            push!(res.defined_modules, newmod)
            _virtual_process!(newtoplevelex, filename, analyzer, config, newmod, res, force_concretize)

            continue
        end

        # can't wrap `:global` declaration into a block
        if isexpr(x, :global)
            eval_with_err_handling(context, x)
            continue
        end

        blk = Expr(:block, lnn, x) # attach current line number info
        lwr = lower_with_err_handling(context, blk)

        isnothing(lwr) && continue # error happened during lowering
        isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        fix_self_references!(res.actual2virtual, src)

        interp = ConcreteInterpreter(filename,
                                     lnn,
                                     eval_with_err_handling,
                                     context,
                                     analyzer,
                                     config,
                                     res,
                                     )
        if force_concretize
            JuliaInterpreter.finish!(interp, Frame(context, src), true)
            continue
        end
        concretized = partially_interpret!(interp, context, src)

        # bail out if nothing to analyze (just a performance optimization)
        all(concretized) && continue

        analyzer = AbstractAnalyzer(analyzer, concretized, context)

        _, result = analyze_toplevel!(analyzer, src)

        append!(res.inference_error_reports, get_reports(result)) # collect error reports
    end

    return res
end

struct VExpr
    x
    force_concretize::Bool
    VExpr(@nospecialize(x), force_concretize::Bool) = new(x, force_concretize)
end

function push_vex_stack!(exs::Vector{VExpr}, newex::Expr, force_concretize::Bool)
    nargs = length(newex.args)
    for i in 0:(nargs-1)
        push!(exs, VExpr(newex.args[nargs-i], force_concretize))
    end
    return exs
end

split_module_path(::Type{String}, m::Module) = split(string(m), '.')
split_module_path(::Type{Symbol}, m::Module) = Symbol.(split_module_path(String, m))
split_module_path(m::Module)                 = split_module_path(Symbol, m)

# if virtualized, replace self references of `actualmod` with `virtualmod` (as is)
fix_self_references!(::Nothing, x) = return
function fix_self_references!((actualmod, virtualmod)::Actual2Virtual, x)
    actualmodsym   = Symbol(actualmod)
    virtualmodsyms = split_module_path(virtualmod)

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

    return postwalk_and_transform!(x) do x, scope
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

# wraps an error that might happen because of inappropriate top-level code abstraction
struct MissingConcretization <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int
end

# wraps general errors from actual execution
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int

    # default constructor
    ActualErrorWrapped(err, st, file, line) = new(err, st, file, line)

    # forward syntax error
    function ActualErrorWrapped(err::ErrorException, st, file, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, file, line)
        else
            new(err, st, file, line)
        end
    end
end

"""
    ConcreteInterpreter

The trait to inject code into JuliaInterpreter's interpretation process; JET.jl overloads:
- `JuliaInterpreter.step_expr!` to add error report pass for module usage expressions and
  support package analysis
- `JuliaInterpreter.evaluate_call_recurse!` to special case `include` calls
- `JuliaInterpreter.handle_err` to wrap an error happened during interpretation into
  `ActualErrorWrapped`
"""
struct ConcreteInterpreter{Analyzer<:AbstractAnalyzer,CP}
    filename::String
    lnn::LineNumberNode
    eval_with_err_handling::Function
    context::Module
    analyzer::Analyzer
    config::ToplevelConfig{CP}
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
    concretize = select_statements(src)

    with_toplevel_logger(interp.analyzer, ≥(DEBUG_LOGGER_LEVEL)) do @nospecialize(io)
        line, file = interp.lnn.line, interp.lnn.file
        println(io, "concretization plan at $file:$line:")
        print_with_code(io, src, concretize)
    end

    # NOTE if `JuliaInterpreter.optimize!` may modify `src`, `src` and `concretize` can be inconsistent
    # here we create `JuliaInterpreter.Frame` by ourselves disabling the optimization (#277)
    # TODO: change to a better Frame constructor when available
    framecode = JuliaInterpreter.FrameCode(mod, src, optimize=false)
    @assert length(framecode.src.code) == length(concretize)
    frame = Frame(framecode, JuliaInterpreter.prepare_framedata(framecode, Any[]))
    selective_eval_fromstart!(interp, frame, concretize, #= istoplevel =# true)

    return concretize
end

# select statements that should be concretized, and actually interpreted rather than abstracted
function select_statements(src::CodeInfo)
    stmts = src.code
    edges = CodeEdges(src)

    concretize = fill(false, length(stmts))

    select_direct_requirement!(concretize, stmts, edges)

    select_dependencies!(concretize, src, edges)

    return concretize
end

function select_direct_requirement!(concretize, stmts, edges)
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
            lhs, rhs = stmt.args
            stmt = rhs
        end
        if isexpr(stmt, :call)
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
end

# TODO implement these prototypes and support a following pattern:
# N = 10
# let tpl = ([i for i in 1:N]...,)
#     @eval gettpl() = $tpl # `tpl` here should be fully concretized
# end
function find_iterblocks(src) end
function add_iterblocks!(cocretize, src, edges, iterblocks) end

# implementation of https://github.com/aviatesk/JET.jl/issues/196
function select_dependencies!(concretize, src, edges)
    debug = false
    debug && println("initially selected:", findall(concretize))

    # find statement sets that come from iteration/iterator protocol
    # TODO iterblocks = find_iterblocks(src)

    # We'll mostly use generic graph traversal to discover all the lines we need,
    # but structs are in a bit of a different category (especially on Julia 1.5+).
    # It's easiest to discover these at the beginning.
    typedefs = find_typedefs(src)

    changed = true
    while changed
        changed = false

        # track SSA predecessors of initial requirements
        changed |= add_ssa_preds!(concretize, src, edges, ())

        # add some domain-specific information
        # TODO changed |= add_iterblocks!(concretized, src, edges, iterblocks)
        changed |= add_typedefs!(concretize, src, edges, typedefs, ())
    end
    debug && (println("after initial requirements discovery:"); print_with_code(stdout::IO, src, concretize))

    # find a loop region and check if any of the requirements discovered so far is involved
    # with it, and if require everything involved with the loop in order to properly
    # concretize the requirement — "require everything involved with the loop" means we will
    # care about "strongly connected components" rather than "cycles" of a directed graph, and
    # strongly connected components detection runs in linear time ``O(|V|+|E|)`` as opposed to
    # cycle dection (, whose worst time complexity is exponential with the number of vertices)
    # and thus the analysis here should terminate in reasonable time even with a fairly
    # complex control flow graph
    cfg = compute_basic_blocks(src.code)
    loops = filter!(>(1)∘length, strongly_connected_components(cfg)) # filter
    debug && @show loops

    critical_blocks = BitSet()
    for (i, block) in enumerate(cfg.blocks)
        if any(view(concretize, block.stmts))
            for loop in loops
                if i in loop
                    push!(critical_blocks, i)
                    for j in loop
                        push!(critical_blocks, j)
                    end
                end
            end
        end
    end
    for loop in loops
        if any(in(critical_blocks), loop)
            for j in loop
                push!(critical_blocks, j)
            end
            # push!(critical_blocks, minimum(loop) - 1)
        end
    end
    debug && @show critical_blocks

    norequire = BitSet()
    for (i, block) in enumerate(cfg.blocks)
        if i ∉ critical_blocks
            pushall!(norequire, rng(block))
        end
    end
    debug && @show norequire

    changed = true
    while changed
        changed = false

        # track SSA predecessors and control flows of the critical blocks
        changed |= add_ssa_preds!(concretize, src, edges, norequire)
        changed |= add_control_flow!(concretize, cfg, norequire)
    end
    debug && (println("after all requirements discovery:"); print_with_code(stdout::IO, src, concretize))
end

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    @assert istoplevel "JET.ConcreteInterpreter can only work for top-level code"

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

    res = @invoke JuliaInterpreter.step_expr!(interp, frame, node, true::Bool)

    interp.config.analyze_from_definitions && collect_toplevel_signature!(interp, frame, node)

    return res
end

function collect_toplevel_signature!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node))
    if isexpr(node, :method, 3)
        sigs = node.args[2]
        atype_params, sparams, _ = @lookup(moduleof(frame), frame, sigs)::SimpleVector
        # t = atype_params[1]
        # if isdefined(t, :name)
        #     # XXX ignore constructor methods, just because it can lead to false positives ...
        #     t.name === CC._TYPE_NAME && return
        # end
        atype = Tuple{(atype_params::SimpleVector)...}
        push!(interp.res.toplevel_signatures, atype)
    end
end

ismoduleusage(@nospecialize(x)) = isexpr(x, (:import, :using, :export))

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
            # import Pkg as P
            if arg.head === :as
                arg = first(arg.args)
            end
            if arg.head === :.
                # using A
                return [x]
            else
                # using A: sym1, sym2, ...
                @assert isexpr(arg, :(:))
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
    f = popfirst!(fargs)  # now it's really just `args`

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

    _virtual_process!(include_text::String, include_file, interp.analyzer, interp.config, context, interp.res)

    # TODO: actually, here we need to try to get the lastly analyzed result of the `_virtual_process!` call above
    return nothing
end

struct RecursiveIncludeErrorReport <: ToplevelErrorReport
    duplicated_file::String
    files::Set{String}
    file::String
    line::Int
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
        report = isexpr(ex, :error) ? SyntaxErrorReport(string("syntax: ", first(ex.args)), filename, line) :
                 isexpr(ex, :incomplete) ? SyntaxErrorReport(first(ex.args), filename, line) :
                 nothing
        isnothing(report) || push!(reports, report)
        index = nextindex
    end
    return reports
end

struct SyntaxErrorReport <: ToplevelErrorReport
    err::ErrorException
    file::String
    line::Int
    SyntaxErrorReport(msg::AbstractString, file, line) = new(ErrorException(msg), file, line)
end
