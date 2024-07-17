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

struct SyntaxErrorReport <: ToplevelErrorReport
    err::Exception
    file::String
    line::Int
    function SyntaxErrorReport(@nospecialize(err), file, line)
        isa(err, Exception) || (err = ErrorException(err))
        return new(err, file, line)
    end
end
# don't show stacktrace for syntax errors
print_report(io::IO, report::SyntaxErrorReport) = showerror(io, report.err)

# wraps general errors from actual execution
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int
    function ActualErrorWrapped(@nospecialize(err), st, file, line)
        if isa(err, ErrorException) && startswith(err.msg, "syntax: ")
            # forward syntax error
            return SyntaxErrorReport(err.msg, file, line)
        end
        return new(err, st, file, line)
    end
end
# TODO: add context information, i.e. during macroexpansion, defining something
print_report(io::IO, report::ActualErrorWrapped) = showerror(io, report.err, report.st)

struct DependencyError <: ToplevelErrorReport
    pkg::String
    dep::String
    file::String
    line::Int
end
function print_report(io::IO, report::DependencyError)
    (; pkg, dep) = report
    # NOTE this message should sync with `Base.require`
    print(io, """
    Package $pkg does not have $dep in its dependencies:
    - You may have a partially installed environment. Try `Pkg.instantiate()`
      to ensure all packages in the environment are installed.
    - Or, if you have $pkg checked out for development and have
      added $dep as a dependency but haven't updated your primary
      environment's manifest file, try `Pkg.resolve()`.
    - Otherwise you may need to report an issue with $pkg""")
end

# wraps an error that might happen because of inappropriate top-level code abstraction
struct MissingConcretization <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int
end
function print_report(io::IO, report::MissingConcretization)
    printstyled(io, "HINT: "; bold = true, color = HINT_COLOR)
    printlnstyled(io, """
    the following error happened mostly because of the missing concretization of global variables,
    and this could be fixed with the `concretization_patterns` configuration.
    Check https://aviatesk.github.io/JET.jl/dev/config/#JET.ToplevelConfig for the details.
    ---"""; color = HINT_COLOR)
    showerror(io, report.err, report.st)
end

struct RecursiveIncludeErrorReport <: ToplevelErrorReport
    duplicated_file::String
    files::Vector{String}
    file::String
    line::Int
end
function print_report(io::IO, report::RecursiveIncludeErrorReport)
    printstyled(io, "ERROR: "; bold = true, color = ERROR_COLOR)
    println(io, "recursive `include` call detected:")
    println(io, " ⚈ duplicated file: ", report.duplicated_file)
    println(io, " ⚈  included files: ", join(report.files, ' '))
end

"""
Configurations for top-level analysis.
These configurations will be active for all the top-level entries explained in the
[top-level analysis entry points](@ref jetanalysis-toplevel-entry) section.

---
- `context::Module = Main` \\
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
  In other word, JET virtualizes the module context of `context` and make sure the original
  module context isn't polluted by JET.
---
- `target_defined_modules::Bool = false` \\
  If `true`, automatically set the [`target_modules`](@ref result-config) configuration so that
  JET filters out errors that are reported within modules that JET doesn't analyze directly.
---
- `analyze_from_definitions::Union{Bool,Symbol} = false` \\
  If `true`, JET will start analysis using signatures of top-level definitions (e.g. method signatures),
  after the top-level interpretation has been done (unless no serious top-level error has
  happened, like errors involved within a macro expansion).
  This can be handy when you want to analyze a package, which usually contains only definitions
  but not their usages (i.e. top-level callsites).
  With this option, JET can enter analysis just with method or type definitions, and we don't
  need to pass a file that uses the target package.

  When `analyze_from_definitions` is specified as `name::Symbol`, JET starts its analysis
  using the interpreted method signature whose name is equal to `name` as the analysis entry
  point. For example, when analyzing a script that uses `@main` to specify the entry point,
  it would be convenient to specify `analyze_from_definitions = :main`.

  !!! warning
      This feature is very experimental at this point, and you may face lots of false positive
      errors, especially when trying to analyze a big package with lots of dependencies.
      If a file that contains top-level callsites (e.g. `test/runtests.jl`) is available,
      JET analysis using the file is generally preferred, since analysis entered from
      concrete call sites will produce more accurate results than analysis entered from
      (maybe not concrete-typed) method signatures.

  Also see: [`report_file`](@ref), [`watch_file`](@ref)
---
- `concretization_patterns::Vector{Any} = Any[]` \\
  Specifies a customized top-level code concretization strategy.

  When analyzing a top-level code, JET first splits the entire code into appropriate units
  of code (i.e. "code blocks"), and then iterate a virtual top-level code execution process
  on each code block in order to simulate Julia's top-level code execution.
  In the virtual code execution, JET will selectively interpret "top-level definitions"
  (like a function definition), while it tries to avoid executing any other parts of code
  including function calls that typically do a main computational task, leaving them to be
  analyzed by the succeeding abstract interpretation based analysis.

  However, currently, JET doesn't track "inter-block" level code dependencies, and therefore
  the selective interpretation of top-level definitions may fail when it needs to use global
  bindings defined in the other code blocks that have not been selected and actually
  interpreted (i.e. "concretized") but left for abstract interpretation (i.e. "abstracted").

  For example, the issue would happen if the expansion of a macro uses a global variable, e.g.:
  > test/fixtures/concretization_patterns.jl
  $(let
      text = read(normpath(@__DIR__, "..", "..", "test", "fixtures", "concretization_patterns.jl"), String)
      lines = split(text, '\n')
      pushfirst!(lines, "```julia"); push!(lines, "```")
      join(lines, "\n  ")
  end)

  To circumvent this issue, JET offers this `concretization_patterns::Vector{<:Any}` configuration,
  which allows us to customize JET's top-level code concretization strategy.
  `concretization_patterns` specifies the _patterns of code_ that should be concretized.
  To put in other word, when JET sees a code that matches any of code patterns specified by
  this configuration, JET will try to interpret and concretize the code, regardless of
  whether or not JET's default code selection logic decides to concretize it.

  JET uses [MacroTools.jl's expression pattern match](https://fluxml.ai/MacroTools.jl/stable/pattern-matching/),
  and we can specify whatever code pattern expected by `MacroTools.@capture` macro.
  For example, in order to solve the issue explained above, we can have:
  ```julia
  concretization_patterns = [:(const GLOBAL_CODE_STORE = Dict())]
  ```
  Then `GLOBAL_CODE_STORE` will just be concretized and so any top-level error won't happen
  at the macro expansion.

  Since configuring `concretization_patterns` properly can be tricky, JET offers a logging
  system that allows us to debug 's top-level code concretization plan. With the
  `toplevel_logger` configuration with specifying the logging level to be above than
  `$JET_LOGGER_LEVEL_DEBUG` ("debug") level, we can see:
  - which code is matched with `concretization_patterns` and forcibly concretized
  - which code is selected to be concretized by JET's default code selection logic:
    where `t`-annotated statements are concretized while `f`-annotated statements are abstracted
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

  Also see: the `toplevel_logger` section below, [`virtual_process`](@ref).

  !!! note
      [`report_package`](@ref) automatically sets this configuration as
      ```julia
      concretization_patterns = [:(x_)]
      ```
      meaning that it will concretize all top-level code included in a package being analyzed.
---
- `toplevel_logger::Union{Nothing,IO} = nothing` \\
  If `IO` object is given, it will track JET's toplevel analysis.
  Logging level can be specified with `$(repr(JET_LOGGER_LEVEL))` `IO` property.
  Currently supported logging levels are either of $(JET_LOGGER_LEVELS_DESC).

  Examples:
  * logs into `stdout`
  ```julia-repl
  julia> report_file(filename; toplevel_logger = stdout)
  ```
  * logs into `io::IOBuffer` with "debug" logger level
  ```julia-repl
  julia> report_file(filename; toplevel_logger = IOContext(io, $(repr(JET_LOGGER_LEVEL)) => $JET_LOGGER_LEVEL_DEBUG));
  ```
---
- `virtualize::Bool = true` \\
  When `true`, JET will virtualize the given root module context.

  This configuration is supposed to be used only for testing or debugging.
  See [`virtualize_module_context`](@ref) for the internal.
---
"""
struct ToplevelConfig
    pkgid::Union{Nothing,Base.PkgId}
    context::Module
    analyze_from_definitions::Union{Bool,Symbol}
    concretization_patterns::Vector{Any}
    virtualize::Bool
    toplevel_logger # ::Union{Nothing,IO}
    function ToplevelConfig(
        pkgid::Union{Nothing,Base.PkgId} = nothing;
        context::Module = Main,
        analyze_from_definitions::Union{Bool,Symbol} = false,
        concretization_patterns = Any[],
        virtualize::Bool = true,
        toplevel_logger::Union{Nothing,IO} = nothing,
        __jetconfigs...)
        concretization_patterns = Any[striplines(normalise(x)) for x in concretization_patterns]
        for pat in default_concretization_patterns()
            push!(concretization_patterns, striplines(normalise(pat)))
        end
        if isa(toplevel_logger, IO)
            @assert jet_logger_level(toplevel_logger) in keys(JET_LOGGER_LEVELS) "toplevel_logger's $JET_LOGGER_LEVEL should be either of $JET_LOGGER_LEVELS_DESC"
        end
        return new(
            pkgid,
            context,
            analyze_from_definitions,
            concretization_patterns,
            virtualize,
            toplevel_logger)
    end
end

should_analyze_from_definitions(config::ToplevelConfig) = config.analyze_from_definitions !== false

default_concretization_patterns() = (
    # concretize type aliases
    # https://github.com/aviatesk/JET.jl/issues/237
    :(const T_ = U_{P__}), :(T_ = U_{P__}),
    )

@nospecialize
with_toplevel_logger(f, config::ToplevelConfig; kwargs...) =
    with_toplevel_logger(f, config.toplevel_logger; kwargs...)
function with_toplevel_logger(f, io; filter=≥(DEFAULT_LOGGER_LEVEL), pre=identity)
    isa(io, IO) || return false
    level = jet_logger_level(io)
    filter(level) || return
    pre(io)
    print(io, "[toplevel-$(JET_LOGGER_LEVELS[level])] ")
    f(io)
end
@specialize

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
    files_stack::Vector{String}
    defined_modules::Set{Module}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
    toplevel_signatures::Vector{Type}
    actual2virtual::Union{Actual2Virtual,Nothing}
end

function VirtualProcessResult(actual2virtual, context)
    return VirtualProcessResult(Set{String}(),
                                Vector{String}(),
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
                    config::ToplevelConfig) -> res::VirtualProcessResult

Simulates Julia's toplevel execution and collects error points, and finally returns $(@doc VirtualProcessResult)

This function first parses `s::AbstractString` into `toplevelex::Expr` and then iterate the
following steps on each code block (`blk`) of `toplevelex`:
1. if `blk` is a `:module` expression, recursively enters analysis into an newly defined
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
                         config::ToplevelConfig)
    if config.virtualize
        actual  = config.context

        start = time()
        virtual = virtualize_module_context(actual)
        with_toplevel_logger(config) do @nospecialize(io)
            sec = round(time() - start; digits = 3)
            println(io, "virtualized the context of $actual (took $sec sec)")
        end

        actual2virtual = Actual2Virtual(actual, virtual)
        context = virtual
    else
        actual2virtual = nothing
        context = config.context
    end

    # Override the configurations for the virtualized module with that for the original package
    # to prevent Preferences.jl from throwing errors. Without this, we will encounter
    # the issues reported at https://github.com/aviatesk/JET.jl/issues/497:
    # ```
    # │ ArgumentError: Module XXX does not correspond to a loaded package!
    # │ Stacktrace:
    # │  [1] get_uuid(m::Module)
    # │    @ Preferences ~/.julia/packages/Preferences/VmJXL/src/utils.jl:8
    # │  [2] var"@load_preference"(__source__::LineNumberNode, __module__::Module, key::Any, default::Any)
    # │    @ Preferences ~/.julia/packages/Preferences/VmJXL/src/Preferences.jl:45
    # ```
    # Note that we can't use the CassetteOverlay-like mechanism here for a cleaner
    # implementation, since Preferences.jl might be called within `macroexpand` or `lower`
    # of the main `_virtual_process!` loop, where we don't have control over execution.
    pkgid = config.pkgid
    old_main_uuid = Preferences.main_uuid[]
    if pkgid !== nothing && pkgid.uuid !== nothing
        Preferences.main_uuid[] = pkgid.uuid
    end
    res = VirtualProcessResult(actual2virtual, context)
    try
        _virtual_process!(res, x, filename, analyzer, config, context, #=pkg_mod_depth=#0)
    finally
        Preferences.main_uuid[] = old_main_uuid
    end

    # analyze collected signatures unless critical error happened
    if should_analyze_from_definitions(config) && isempty(res.toplevel_error_reports)
        analyze_from_definitions!(analyzer, res, config)
    end

    # TODO move this aggregation to `analyze_from_definitions!`?
    unique!(aggregation_policy(analyzer), res.inference_error_reports)

    return res
end

"""
    virtualize_module_context(actual::Module)

HACK to return a module where the context of `actual` is virtualized.

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
            error("can't virtualize an anonymous module")
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

gen_virtual_module(parent::Module = Main; name = VIRTUAL_MODULE_NAME) =
    Core.eval(parent, :(module $(gensym(name)) end))::Module

# NOTE when `@generated` function has been defined, signatures of both its entry and
# generator should have been collected, and we will just analyze them separately
# if code generation has failed given the entry method signature, the overload of
# `InferenceState(..., ::AbstractAnalyzer)` will collect `GeneratorErrorReport`
function analyze_from_definitions!(analyzer::AbstractAnalyzer, res::VirtualProcessResult, config::ToplevelConfig)
    succeeded = Ref(0)
    start = time()
    n = length(res.toplevel_signatures)
    state = AnalyzerState(analyzer)
    oldworld = state.world
    new_world = get_world_counter()
    state.world = new_world
    if analyzer isa JETAnalyzer && analyzer.report_pass === BasicPass()
        analyzer = JETAnalyzer(state, DefinitionAnalysisPass(), JETAnalyzerConfig(analyzer))
    else
        analyzer = AbstractAnalyzer(analyzer, state)
    end
    entrypoint = config.analyze_from_definitions
    for (i, tt) in enumerate(res.toplevel_signatures)
        match = Base._which(tt;
            # NOTE use the latest world counter with `method_table(analyzer)` unwrapped,
            # otherwise it may use a world counter when this method isn't defined yet
            method_table=unwrap_method_table(CC.method_table(analyzer)),
            world=new_world,
            raise=false)
        if match !== nothing && (
            !(entrypoint isa Symbol)#=implies analyze_from_definitions === true=# ||
            match.method.name === entrypoint)
            succeeded[] += 1
            with_toplevel_logger(config; pre=clearline) do @nospecialize(io)
                (i == n ? println : print)(io, "analyzing from top-level definitions ($(succeeded[])/$n)")
            end
            analyzer, result = analyze_method_signature!(analyzer,
                match.method, match.spec_types, match.sparams)
            reports = get_reports(analyzer, result)
            append!(res.inference_error_reports, reports)
            continue
        end
        # something went wrong
        with_toplevel_logger(config; filter=≥(JET_LOGGER_LEVEL_DEBUG), pre=clearline) do @nospecialize(io)
            println(io, "couldn't find a single method matching the signature `", tt, "`")
        end
    end
    state.world = oldworld
    with_toplevel_logger(config) do @nospecialize(io)
        sec = round(time() - start; digits = 3)
        println(io, "analyzed $(succeeded[]) top-level definitions (took $sec sec)")
    end
    return nothing
end
clearline(io) = print(io, '\r')

function _virtual_process!(res::VirtualProcessResult,
                           s::AbstractString,
                           filename::AbstractString,
                           analyzer::AbstractAnalyzer,
                           config::ToplevelConfig,
                           context::Module,
                           pkg_mod_depth::Int)
    start = time()

    with_toplevel_logger(config) do @nospecialize(io)
        println(io, "entered into $filename")
    end

    push!(res.included_files, filename)
    push!(res.files_stack, filename)

    s = String(s)::String
    toplevelex = Base.parse_input_line(s; filename)

    if isexpr(toplevelex, (:error, :incomplete))
        # if there's any syntax error, try to identify all the syntax error location
        report_syntax_errors!(res, s, filename)
    elseif isnothing(toplevelex)
        # just return if there is nothing to analyze
    else
        @assert isexpr(toplevelex, :toplevel)
        _virtual_process!(res, toplevelex, filename, analyzer, config, context, pkg_mod_depth)
    end
    pop!(res.files_stack)

    with_toplevel_logger(config) do @nospecialize(io)
        sec = round(time() - start; digits = 3)
        println(io, " exited from $filename (took $sec sec)")
    end

    return res
end

function _virtual_process!(res::VirtualProcessResult,
                           toplevelex::Expr,
                           filename::AbstractString,
                           analyzer::AbstractAnalyzer,
                           config::ToplevelConfig,
                           context::Module,
                           pkg_mod_depth::Int,
                           force_concretize::Bool = false)
    local lnnref = Ref(LineNumberNode(0, filename))

    function err_handler(@nospecialize(err), st)
        local report = is_missing_concretization(err) ?
                       MissingConcretization(err, st, filename, lnnref[].line) :
                       ActualErrorWrapped(err, st, filename, lnnref[].line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end

    function macroexpand_with_err_handling(mod::Module, x::Expr)
        # `scrub_offset = 4` corresponds to `with_err_handling` -> `f` -> `macroexpand` -> kwfunc (`macroexpand`)
        return with_err_handling(err_handler, #=scrub_offset=#4) do
            # XXX we want to non-recursive, sequential partial macro expansion here, which allows
            # us to collect more fine-grained error reports within macro expansions
            # but it can lead to invalid macro hygiene escaping because of https://github.com/JuliaLang/julia/issues/20241
            return macroexpand(mod, x; recursive = true #= but want to use `false` here =#)
        end
    end
    function eval_with_err_handling(mod::Module, x::Expr)
        # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `Core.eval`
        return with_err_handling(err_handler, #=scrub_offset=#3) do
            return Core.eval(mod, x)
        end
    end
    function lower_with_err_handling(mod::Module, x::Expr)
        # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `lower`
        return with_err_handling(err_handler, #=scrub_offset=#3) do
            lwr = lower(mod, x)
            # here we should capture syntax errors found during lowering
            if isexpr(lwr, :error)
                msg = first(lwr.args)
                push!(res.toplevel_error_reports, SyntaxErrorReport(lazy"syntax: $msg", filename, lnnref[].line))
                return nothing
            end
            return lwr
        end
    end
    local dependencies = Set{Symbol}()
    function usemodule_with_err_handling(mod::Module, ex::Expr)
        if isexpr(ex, (:export, :public))
            @goto eval_usemodule
        end
        # TODO recursive analysis on dependencies?
        pkgid = config.pkgid
        if pkgid !== nothing
            module_usage = pattern_match_module_usage(ex)
            (; modpath) = module_usage
            dep = first(modpath)::Symbol
            if !(dep === :. || # relative module doesn't need to be fixed
                 dep === :Base || dep === :Core) # modules available by default
                if dep === Symbol(pkgid.name)
                    # it's somehow allowed to use the package itself without the relative module path,
                    # so we need to special case it and fix it to use the relative module path
                    for _ = 1:pkg_mod_depth
                        pushfirst!(modpath, :.)
                    end
                else
                    if dep ∉ dependencies
                        depstr = String(dep)
                        depid = Base.identify_package(pkgid, depstr)
                        if depid === nothing
                            # IDEA better message in a case of `any(m::Module->dep===nameof(m), res.defined_modules))`?
                            local report = DependencyError(pkgid.name, depstr, filename, lnnref[].line)
                            push!(res.toplevel_error_reports, report)
                            return nothing
                        end
                        require_ex = :(const $dep = Base.require($depid))
                        # TODO better handling of loading errors that may happen here
                        require_res = with_err_handling(err_handler, #=scrub_offset=#3) do
                            return Core.eval(mod, require_ex)
                        end
                        isnothing(require_res) && return nothing
                        push!(dependencies, dep)
                    end
                    pushfirst!(modpath, :.)
                end
                fixed_module_usage = ModuleUsage(module_usage; modpath)
                ex = form_module_usage(fixed_module_usage)
            elseif dep === :.
                # The syntax `import ..Submod` refers to the name that is available within
                # a parent module specified by the number of `.` dots, indicating how many
                # levels up the module hierarchy to go. However, when it comes to package
                # loading, it seems to work regardless of the number of dots. For now, in
                # `report_package`, adjust `modpath` here to mimic the package loading behavior.
                topmodidx = findfirst(@nospecialize(mp)->mp!==:., modpath)::Int
                topmodsym = modpath[topmodidx]
                curmod = mod
                for i = 1:(topmodidx-1)
                    if topmodsym isa Symbol && isdefined(curmod, topmodsym)
                        modpath = modpath[topmodidx:end]
                        for j = 1:i
                            pushfirst!(modpath, :.)
                        end
                        fixed_module_usage = ModuleUsage(module_usage; modpath)
                        ex = form_module_usage(fixed_module_usage)
                        break
                    else
                        curmod = parentmodule(curmod)
                    end
                end
            end
        end
        # `scrub_offset = 3` corresponds to `with_err_handling` -> `f` -> `Core.eval`
        @label eval_usemodule
        return with_err_handling(err_handler, #=scrub_offset=#3) do
            return Core.eval(mod, ex)
        end
    end

    # transform, and then analyze sequentially
    # IDEA the following code has some of duplicated work with `JuliaInterpreter.ExprSpliter` and we may want to factor them out
    exs = push_vex_stack!(VExpr[], toplevelex, force_concretize)
    while !isempty(exs)
        (; x, force_concretize) = pop!(exs)

        # with_toplevel_logger(config; filter=≥(JET_LOGGER_LEVEL_DEBUG)) do @nospecialize(io)
        #     println(io, "analyzing ", x)
        # end

        # update line info
        if isa(x, LineNumberNode)
            lnnref[] = x
            continue
        end

        # apply user-specified concretization strategy, which is configured as expression
        # pattern match on surface level AST code representation; if any of the specified
        # patterns matches `x`, JET just concretizes everything involved with it
        # since patterns are expected to work on surface level AST, we should configure it
        # here before macro expansion and lowering
        if !force_concretize
            for pat in config.concretization_patterns
                if @capture(x, $pat)
                    with_toplevel_logger(config; filter=≥(JET_LOGGER_LEVEL_DEBUG)) do @nospecialize(io)
                        line, file = lnnref[].line, lnnref[].file
                        x′ = striplines(normalise(x))
                        println(io, "concretization pattern `$pat` matched `$x′` at $file:$line")
                    end
                    force_concretize = true
                    break
                end
            end
        end

        # although we will lower `x` after special-handling `:toplevel` and `:module` expressions,
        # expand `macrocall`s here because macro can arbitrarily generate those expressions
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

            newcontext = newcontext::Module
            push!(res.defined_modules, newcontext)
            _virtual_process!(res, newtoplevelex, filename, analyzer, config, newcontext,
                              pkg_mod_depth+1, force_concretize)

            continue
        end

        # can't wrap `:global` declaration into a block
        if isexpr(x, :global)
            eval_with_err_handling(context, x)
            continue
        end

        blk = Expr(:block, lnnref[], x) # attach current line number info
        lwr = lower_with_err_handling(context, blk)

        isnothing(lwr) && continue # error happened during lowering
        isexpr(lwr, :thunk) || continue # literal

        src = first((lwr::Expr).args)::CodeInfo

        fix_self_references!(res.actual2virtual, src)

        interp = ConcreteInterpreter(filename, lnnref[], usemodule_with_err_handling,
                                     context, analyzer, config, res, pkg_mod_depth)
        if force_concretize
            JuliaInterpreter.finish!(interp, Frame(context, src), true)
            continue
        end
        concretized = partially_interpret!(interp, context, src)

        # bail out if nothing to analyze (just a performance optimization)
        if bail_out_concretized(concretized, src)
            continue
        end

        analyzer = AbstractAnalyzer(analyzer, concretized, context)

        _, result = analyze_toplevel!(analyzer, src)

        append!(res.inference_error_reports, get_reports(analyzer, result)) # collect error reports
    end

    return res
end

# check if all statements of `src` have been concretized
function bail_out_concretized(concretized::BitVector, src::CodeInfo)
    if all(concretized)
        return true
    elseif (length(concretized) == 2 && (concretized[1] && ismoduleusage(src.code[1])) &&
            (!concretized[2] && src.code[2] isa ReturnNode))
        # special case module usage statements
        return true
    end
    return false
end

struct VExpr
    x
    force_concretize::Bool
    VExpr(@nospecialize(x), force_concretize::Bool) = new(x, force_concretize)
end

function push_vex_stack!(exs::Vector{VExpr}, newex::Expr, force_concretize::Bool)
    nargs = length(newex.args)
    for i = nargs:-1:1
        push!(exs, VExpr(newex.args[i], force_concretize))
    end
    return exs
end

function split_module_path(m::Module)
    ret = Symbol[]
    split_module_path!(m, ret)
    return ret
end
function split_module_path!(m::Module, ret)
    mp = parentmodule(m)
    if m === Main || m === Base || m === Core || mp === m
        push!(ret, nameof(m))
        return
    end
    split_module_path!(mp, ret)
    push!(ret, nameof(m))
end

struct ModuleUsage
    head::Symbol
    modpath::Vector{Any}
    namepath::Union{Vector{Any},Nothing}
    alias::Union{Symbol,Nothing}
end
function ModuleUsage(m::ModuleUsage;
                     head::Symbol = m.head,
                     modpath::Vector{Any} = m.modpath,
                     namepath::Union{Vector{Any},Nothing} = m.namepath,
                     alias::Union{Symbol,Nothing} = m.alias)
    return ModuleUsage(head, modpath, namepath, alias)
end

function pattern_match_module_usage(usage::Expr)
    modpath = namepath = alias = nothing
    if @capture(usage, import modpath__)
        head = :import
    elseif @capture(usage, using modpath__)
        head = :using
    elseif @capture(usage, import modpath__: namepath__)
        head = :import
    elseif @capture(usage, using modpath__: namepath__)
        head = :using
    elseif @capture(usage, import modpath__ as alias_)
        head = :import
    elseif @capture(usage, import modpath__: namepath__ as alias_)
        head = :import
    elseif @capture(usage, using modpath__: namepath__ as alias_)
        head = :using
    else
        error(lazy"unexpected module usage found: $usage")
    end
    return ModuleUsage(head, modpath::Vector{Any}, namepath::Union{Nothing,Vector{Any}}, alias::Union{Nothing,Symbol})
end

function form_module_usage(moduleusage::ModuleUsage)
    (; head, modpath, namepath, alias) = moduleusage
    if isa(alias, Symbol)
        if !isnothing(namepath)
            return form_module_usage_alias(head, modpath, namepath, alias)
        else
            return form_module_import_alias(modpath, alias)
        end
    elseif !isnothing(namepath)
        return form_module_usage_specific(head, modpath, namepath)
    end
    return form_module_usage(head, modpath)
end

# using A.B.C
form_module_usage(head::Symbol, modpath::Vector{Any}) =
    Expr(head, Expr(:., modpath...))
# using A.B.C: abc
form_module_usage_specific(head::Symbol, modpath::Vector{Any}, namepath::Vector{Any}) =
    Expr(head, Expr(:(:), Expr(:., modpath...), Expr(:., namepath...)))
# using A.B.C: abc as abc′
form_module_usage_alias(head::Symbol, modpath::Vector{Any}, namepath::Vector{Any}, alias::Symbol) =
    Expr(head, Expr(:(:), Expr(:., modpath...), Expr(:as, Expr(:., namepath...), alias)))
# import A.B.C as abc
form_module_import_alias(modpath::Vector{Any}, alias::Symbol) =
    Expr(:import, Expr(:as, Expr(:., modpath...), alias))

# if virtualized, replace self references of `actualmod` with `virtualmod` (as is)
fix_self_references!(::Nothing, @nospecialize(x)) = x
function fix_self_references!((actualmod, virtualmod)::Actual2Virtual, @nospecialize(x))
    actualmodsym   = Symbol(actualmod)
    virtualmodsyms = split_module_path(virtualmod)

    function any_self(modpath::Vector{Any})
        for x in modpath
            if x === virtualmod # postwalk, so we compare to `virtualmod`
                return true
            end
        end
        return false
    end

    # we can't use `Module` object in module usage expression, so we need to replace it with
    # its symbolic representation
    function fix_self(modpath::Vector{Any})
        ret = Any[]
        for x in modpath
            if x === virtualmod # postwalk, so we compare to `virtualmod`
                push!(ret, virtualmodsyms...)
            else
                push!(ret, x)
            end
        end
        return ret
    end

    function fix_self_reference_simple(usage::Expr)
        if isexpr(usage, (:export, :public))
            return usage
        end
        module_usage = pattern_match_module_usage(usage)
        any_self(module_usage.modpath) || return usage
        fixed_module_usage = ModuleUsage(module_usage; modpath = fix_self(module_usage.modpath))
        return form_module_usage(fixed_module_usage)
    end

    function fix_self_reference(usage::Expr)
        ret = Expr(usage.head)
        for simple in to_simple_module_usages(usage)
            fixed = fix_self_reference_simple(simple)
            @assert usage.head === fixed.head === simple.head && length(fixed.args) == 1
            push!(ret.args, first(fixed.args))
        end
        return ret
    end

    return postwalk_and_transform!(x) do @nospecialize(xx), scope::Vector{Symbol}
        if ismoduleusage(xx)
            return fix_self_reference(xx)
        elseif xx === actualmodsym
            return virtualmod
        end
        return xx
    end
end

function postwalk_and_transform!(f, x, scope = Symbol[])
    inner(@nospecialize(x)) = postwalk_and_transform!(f, x, scope)
    return walk_and_transform!(x, inner, f, scope)
end
function prewalk_and_transform!(f, x, scope::Vector{Symbol} = Symbol[])
    inner(@nospecialize(x)) = prewalk_and_transform!(f, x, scope)
    outer(@nospecialize(x), scope::Vector{Symbol}) = x
    return walk_and_transform!(f(x, scope), inner, outer, scope)
end

function walk_and_transform!(@nospecialize(x), inner, outer, scope::Vector{Symbol})
    if isa(x, GotoIfNot)
        push!(scope, :gotoifnot)
        cond = inner(walk_and_transform!(x.cond, inner, outer, scope))
        dest = inner(walk_and_transform!(x.dest, inner, outer, scope))
        x = GotoIfNot(cond, dest)
        pop!(scope)
    elseif isa(x, Expr)
        push!(scope, x.head)
        for i = 1:length(x.args)
            x.args[i] = inner(walk_and_transform!(x.args[i], inner, outer, scope))
        end
        pop!(scope)
    elseif isa(x, CodeInfo)
        for i = 1:length(x.code)
            x.code[i] = inner(walk_and_transform!(x.code[i], inner, outer, scope))
        end
    end
    return outer(x, scope)
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
struct ConcreteInterpreter{F,Analyzer<:AbstractAnalyzer}
    filename::String
    lnn::LineNumberNode
    usemodule_with_err_handling::F
    context::Module
    analyzer::Analyzer
    config::ToplevelConfig
    res::VirtualProcessResult
    pkg_mod_depth::Int
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
    @assert length(src.code) == length(concretize)

    with_toplevel_logger(interp.config; filter=≥(JET_LOGGER_LEVEL_DEBUG)) do @nospecialize(io)
        line, file = interp.lnn.line, interp.lnn.file
        println(io, "concretization plan at $file:$line:")
        LoweredCodeUtils.print_with_code(io, src, concretize)
    end

    # NOTE if `JuliaInterpreter.optimize!` may modify `src`, `src` and `concretize` can be inconsistent
    # here we create `JuliaInterpreter.Frame` by ourselves disabling the optimization (#277)
    frame = Frame(mod, src; optimize=false)
    LoweredCodeUtils.selective_eval_fromstart!(interp, frame, concretize, #=istoplevel=#true)

    return concretize
end

# select statements that should be concretized, and actually interpreted rather than abstracted
function select_statements(src::CodeInfo)
    stmts = src.code
    cl = LoweredCodeUtils.CodeLinks(src) # make `CodeEdges` hold `CodeLinks`?
    edges = LoweredCodeUtils.CodeEdges(src, cl)

    concretize = falses(length(stmts))

    select_direct_requirement!(concretize, stmts, edges)

    select_dependencies!(concretize, src, edges, cl)

    return concretize
end

function select_direct_requirement!(concretize, stmts, edges)
    for (idx, stmt) in enumerate(stmts)
        if (LoweredCodeUtils.ismethod(stmt) ||    # don't abstract away method definitions
            LoweredCodeUtils.istypedef(stmt) ||   # don't abstract away type definitions
            ismoduleusage(stmt)) # module usages are handled by `ConcreteInterpreter`
            concretize[idx] = true
            continue
        end

        if isexpr(stmt, :(=))
            lhs, rhs = stmt.args
            stmt = rhs
        end
        # `include` calls are special cased
        if is_known_call(stmt, :include, stmts)
            concretize[idx] = true
        elseif is_known_getproperty(stmt, :include, stmts)
            # this is something like:
            # ```
            # %1 = getproperty(Base, :include)
            # ...
            # %x = Expr(:call, %1, ...)
            # ```
            # so require `%x` too
            concretize[idx] = true
            concretize[edges.succs[idx]] .= true
        # `eval` calls are difficult to analyze, but since they may contain toplevel
        # definitions, JET just concretizes them always
        elseif is_known_call(stmt, :eval, stmts)
            concretize[idx] = true
        elseif is_known_getproperty(stmt, :eval, stmts)
            concretize[idx] = true
            concretize[edges.succs[idx]] .= true
        end
    end
end

# adapted from
# - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L53
# - https://github.com/timholy/Revise.jl/blob/266ed68d7dd3bea67c39f96513cda30bbcd7d441/src/lowered.jl#L87-L88
function is_known_call(@nospecialize(stmt), func::Symbol, stmts::Vector{Any})
    isexpr(stmt, :call) || return false
    f = stmt.args[1]
    if f isa SSAValue
        f = stmts[f.id]
    end
    isa(f, Symbol) && f === func && return true
    isa(f, GlobalRef) && f.name === func && return true
    return false
end
function is_known_getproperty(@nospecialize(stmt), func::Symbol, stmts::Vector{Any})
    isexpr(stmt, :call) || return false
    length(stmt.args) ≥ 3 || return false
    f = stmt.args[1]
    if f isa SSAValue
        f = stmts[f.id]
    end
    if (callee_matches(f, Base, :getproperty) ||
        callee_matches(f, Core, :getproperty) ||
        callee_matches(f, Core.Compiler, :getproperty))
        if is_quotenode_egal(stmt.args[3], func)
            return true
        end
    end
    return false
end

# The goal of this function is to request concretization of the minimal necessary control
# flow to evaluate statements whose concretization have already been requested.
# The basic approach is to check if there are any active successors for each basic block,
# and if there is an active successor and the terminator is not a fall-through, then request
# the concretization of that terminator. Additionally, for conditional terminators, a simple
# optimization using post-domination analysis is also performed.
function add_control_flow!(concretize::BitVector, src::CodeInfo, cfg::CFG, postdomtree)
    local changed::Bool = false
    function mark_concretize!(idx::Int)
        if !concretize[idx]
            concretize[idx] = true
            return true
        end
        return false
    end
    nblocks = length(cfg.blocks)
    for bbidx = 1:nblocks
        bb = cfg.blocks[bbidx] # forward traversal
        nsuccs = length(bb.succs)
        if nsuccs == 0
            continue
        elseif nsuccs == 1
            terminator_idx = bb.stmts[end]
            if src.code[terminator_idx] isa GotoNode
                # If the destination of this `GotoNode` is not active, it's fine to ignore
                # the control flow caused by this `GotoNode` and treat it as a fall-through.
                # If the block that is fallen through to is active and has a dependency on
                # this goto block, then the concretization of this goto block should already
                # be requested (at some point of the higher concretization convergence cycle
                # of `select_dependencies`), and thus, this `GotoNode` will be concretized.
                if any(@view concretize[cfg.blocks[only(bb.succs)].stmts])
                    changed |= mark_concretize!(terminator_idx)
                end
            end
        elseif nsuccs == 2
            terminator_idx = bb.stmts[end]
            @assert is_conditional_terminator(src.code[terminator_idx]) "invalid IR"
            succ1, succ2 = bb.succs
            succ1_req = any(@view concretize[cfg.blocks[succ1].stmts])
            succ2_req = any(@view concretize[cfg.blocks[succ2].stmts])
            if succ1_req
                if succ2_req
                    changed |= mark_concretize!(terminator_idx)
                else
                    active_bb, inactive_bb = succ1, succ2
                    @goto asymmetric_case
                end
            elseif succ2_req
                active_bb, inactive_bb = succ2, succ1
                @label asymmetric_case
                # We can ignore the control flow of this conditional terminator and treat
                # it as a fall-through if only one of its successors is active and the
                # active block post-dominates the inactive one, since the post-domination
                # ensures that the active basic block will be reached regardless of the
                # control flow.
                if CC.postdominates(postdomtree, active_bb, inactive_bb)
                    # fall through this block
                else
                    changed |= mark_concretize!(terminator_idx)
                end
            else
                # both successors are inactive, just fall through this block
            end
        end
    end
    return changed
end

is_conditional_terminator(@nospecialize stmt) = stmt isa GotoIfNot ||
    (@static @isdefined(EnterNode) ? stmt isa EnterNode : isexpr(stmt, :enter))

function add_required_inplace!(concretize::BitVector, src::CodeInfo, edges, cl)
    changed = false
    for i = 1:length(src.code)
        stmt = src.code[i]
        if isexpr(stmt, :call) && length(stmt.args) ≥ 2
            func = stmt.args[1]
            if (callee_matches(func, Base, :push!) ||
                callee_matches(func, Base, :pop!) ||
                callee_matches(func, Base, :empty!) ||
                callee_matches(func, Base, :setindex!))
                if is_arg_requested(stmt.args[2], concretize, edges, cl)
                    if !concretize[i]
                        changed = concretize[i] = true
                    end
                end
            end
        end
    end
    return changed
end
# check if the first argument is requested to be concretized
function is_arg_requested(@nospecialize(arg), concretize, edges, cl)
    if arg isa SSAValue
        return concretize[arg.id] || any(@view concretize[edges.preds[arg.id]])
    elseif arg isa SlotNumber
        return any(@view concretize[cl.slotassigns[arg.id]])
    end
    return false
end

function select_dependencies!(concretize::BitVector, src::CodeInfo, edges, cl)
    typedefs = LoweredCodeUtils.find_typedefs(src)
    cfg = CC.compute_basic_blocks(src.code)
    postdomtree = CC.construct_postdomtree(cfg.blocks)

    while true
        changed = false

        # discover struct/method definitions at the beginning,
        # and propagate the definition requirements by tracking SSA precedessors
        changed |= LoweredCodeUtils.add_typedefs!(concretize, src, edges, typedefs, ())
        changed |= add_ssa_preds!(concretize, src, edges, ())

        # mark some common inplace operations like `push!(x, ...)` and `setindex!(x, ...)`
        # when `x` has been marked already: otherwise we may end up using it with invalid state
        changed |= add_required_inplace!(concretize, src, edges, cl)
        changed |= add_ssa_preds!(concretize, src, edges, ())

        # mark necessary control flows,
        # and propagate the definition requirements by tracking SSA precedessors
        changed |= add_control_flow!(concretize, src, cfg, postdomtree)
        changed |= add_ssa_preds!(concretize, src, edges, ())

        changed || break
    end
end

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    @assert istoplevel "ConcreteInterpreter can only work for top-level code"

    if ismoduleusage(node)
        for ex in to_simple_module_usages(node)
            interp.usemodule_with_err_handling(interp.context, ex)
        end
        return frame.pc += 1
    end

    res = @invoke JuliaInterpreter.step_expr!(interp::Any, frame::Any, node::Any, true::Bool)

    should_analyze_from_definitions(interp.config) && collect_toplevel_signature!(interp, frame, node)

    return res
end

function collect_toplevel_signature!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node))
    isexpr(node, :method, 3) || return nothing
    entrypoint = interp.config.analyze_from_definitions
    if entrypoint isa Symbol
        methname = node.args[1]
        if !(methname isa Symbol && methname === entrypoint)
            return nothing
        end
    end
    sigs = node.args[2]
    atype_params, sparams, #=linenode=#_ =
        JuliaInterpreter.@lookup(JuliaInterpreter.moduleof(frame), frame, sigs)::SimpleVector
    tt = form_method_signature(atype_params::SimpleVector, sparams::SimpleVector)
    @assert !CC.has_free_typevars(tt) "free type variable left in toplevel_signatures"
    push!(interp.res.toplevel_signatures, tt)
end

# form a method signature from the first and second parameters of lowered `:method` expression
function form_method_signature(atype_params::SimpleVector, sparams::SimpleVector)
    atype = Tuple{atype_params...}
    for i = length(sparams):-1:1
        atype = UnionAll(sparams[i]::TypeVar, atype)
    end
    return atype
end

ismoduleusage(@nospecialize(x)) = isexpr(x, (:import, :using, :export, :public))

# assuming `ismoduleusage(x)` holds
function to_simple_module_usages(x::Expr)
    @assert ismoduleusage(x)
    ret = _to_simple_module_usages(x)
    foreach(ret) do ex::Expr
        @assert ex.head === x.head && length(ex.args) == 1
    end
    return ret
end
function _to_simple_module_usages(x::Expr)
    if length(x.args) != 1
        # `using A, B`, `export a, b`, `public a, b`
        return Expr[Expr(x.head, arg) for arg in x.args]
    end
    arg = only(x.args)
    if isa(arg, Symbol)
        # `export a`, `public a`
        return Expr[x]
    end
    if isexpr(arg, :as)
        # `import Pkg as P`
        arg = first(arg.args)
    end
    if isexpr(arg, :.)
        # `using A`
        return Expr[x]
    end
    # `using A: sym1, sym2, ...`
    @assert isexpr(arg, :(:))
    a, as... = arg.args
    return Expr[Expr(x.head, ex) for ex in Expr[Expr(arg.head, a, a′) for a′ in as]]
end

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/2f5f80034bc287a60fe77c4e3b5a49a087e38f8b/src/interpret.jl#L188-L199
# works almost same as `JuliaInterpreter.evaluate_call_compiled!`, but with few important tweaks:
# - a special handling for `include` call to recursively apply JET's analysis on the included file
# - some `@invokelatest` are added where we directly call an user expression
#   since `_virtual_process!` iteratively interprets toplevel expressions but the world age
#   is not updated at each iteration so we need to make sure the user expression is
#   evaluated in the latest world age where newly defined functions are available.
function JuliaInterpreter.evaluate_call_recurse!(interp::ConcreteInterpreter, frame::Frame, call_expr::Expr; enter_generated::Bool=false)
    # @assert !enter_generated
    pc = frame.pc
    ret = JuliaInterpreter.bypass_builtins(interp, frame, call_expr, pc)
    isa(ret, Some{Any}) && return ret.value
    ret = @invokelatest JuliaInterpreter.maybe_evaluate_builtin(frame, call_expr, false)
    isa(ret, Some{Any}) && return ret.value
    args = JuliaInterpreter.collect_args(interp, frame, call_expr)
    f = popfirst!(args) # now it's really just `args`
    isinclude(f) && return handle_include(interp, f, args)
    if f === Base._ccallable
        # skip concrete-interpretation of `jl_extern_c` as the C-side function definition
        # isn't really essential for Julia-level analysis
        if length(args) == 2 && args[1] isa Type && args[2] isa Type
            # ignore it only if the method dispatch is successful
            return nothing
        else
            # otherwise just call it to trigger a method error
        end
    end
    return @invokelatest f(args...)
end

isinclude(@nospecialize f) = isa(f, Function) && nameof(f) === :include

function handle_include(interp::ConcreteInterpreter, @nospecialize(include_func), args::Vector{Any})
    filename = interp.filename
    res = interp.res
    lnn = interp.lnn
    context = interp.context

    function handle_actual_method_error!(args::Vector{Any})
        err = MethodError(include_func, args)
        local report = ActualErrorWrapped(err, [], filename, lnn.line)
        push!(res.toplevel_error_reports, report)
    end

    nargs = length(args)
    if nargs == 1
        fname = only(args)
    elseif nargs == 2
        x, fname = args
        if isa(x, Module)
            context = x
        elseif isa(x, Function)
            @warn "JET is unable to analyze `include(mapexpr::Function, filename::String)` call currently."
        else
            handle_actual_method_error!(args)
            return nothing
        end
    else
        handle_actual_method_error!(args)
        return nothing
    end
    if !isa(fname, String)
        handle_actual_method_error!(args)
        return nothing
    end

    include_file = normpath(dirname(filename), fname)
    # handle recursive `include`s
    if include_file in res.files_stack
        local report = RecursiveIncludeErrorReport(include_file, copy(res.files_stack), filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end

    function read_err_handler(@nospecialize(err), st)
        local report = ActualErrorWrapped(err, st, filename, lnn.line)
        push!(res.toplevel_error_reports, report)
        return nothing
    end
    # `scrub_offset = 3` corresponds to `with_err_handling` -> `f`
    include_text = with_err_handling(read_err_handler, #=scrub_offset=#2) do
        read(include_file, String)
    end
    isnothing(include_text) && return nothing # typically no file error

    _virtual_process!(interp.res, include_text::String, include_file, interp.analyzer,
                      interp.config, context, interp.pkg_mod_depth)

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

    # if the last error is from this file, it's likely to be a serious error of JET
    lastframe = last(st)
    if lastframe.file === JET_VIRTUALPROCESS_FILE && lastframe.func === :evaluate_call_recurse!
        rethrow(err)
    end

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

        # other general errors may happen at `JuliaInterpreter.collect_args`, etc.
        # we don't show any stacktrace for those errors (by keeping the original `i = 0`)
        # since they are hopefully self-explanatory
        continue
    end
    st = st[1:i]

    report = is_missing_concretization(err) ?
             MissingConcretization(err, st, interp.filename, interp.lnn.line) :
             ActualErrorWrapped(err, st, interp.filename, interp.lnn.line)
    push!(interp.res.toplevel_error_reports, report)

    return nothing # stop further interpretation
end

function with_err_handling(f, err_handler, scrub_offset::Int)
    try
        return f()
    catch err
        bt = catch_backtrace()
        st = stacktrace(bt)

        # scrub the original stacktrace so that it only contains frames from user code
        for (i, frame) in enumerate(st)
            if frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :with_err_handling
                st = st[1:(i - scrub_offset)]
                break
            end
        end

        return err_handler(err, st)
    end
end

let s = string(nameof(AbstractGlobal))
    global function is_missing_concretization(@nospecialize(err))
        io = IOBuffer()
        showerror(io, err)
        occursin(s, String(take!(io)))
    end
end

function report_syntax_errors!(res, s, filename)
    index = line = 1
    while begin
            ex, nextindex = Base.Meta._parse_string(s, filename, line, index, :statement)
            !isnothing(ex)
        end
        line += count(==('\n'), s[index:nextindex-1])
        if isexpr(ex, :error)
            err = only(ex.args)
            if (@static JULIA_SYNTAX_ENABLED && true) && isa(err, ParseError)
                report = SyntaxErrorReport(err, filename, line)
            else
                report = SyntaxErrorReport(lazy"syntax: $err", filename, line)
            end
        elseif isexpr(ex, :incomplete)
            err = only(ex.args)
            if (@static JULIA_SYNTAX_ENABLED && true) && isa(err, ParseError)
                report = SyntaxErrorReport(err, filename, line)
            else
                report = SyntaxErrorReport(lazy"syntax: $err", filename, line)
            end
        else
            report = nothing
        end
        isnothing(report) || push!(res.toplevel_error_reports, report)
        index = nextindex
    end
end

# a bridge to abstract interpretation
function analyze_toplevel!(analyzer::AbstractAnalyzer, src::CodeInfo)
    # construct toplevel `MethodInstance`
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{}

    mi.def = mod = get_toplevelmod(analyzer)
    transform_abstract_global_symbols!(src, analyzer)
    resolve_toplevel_symbols!(src, mod)
    @atomic mi.uninferred = src

    result = InferenceResult(mi);
    init_result!(analyzer, result)
    # NOTE toplevel frames don't really need to be cached, but still better to be optimized
    # in order to get reasonable `UncaughtExceptionReport`, and also, otherwise
    # `typeinf_edge` won't add "toplevel-to-callee" edges
    frame = InferenceState(result, src, #=cache_mode=#:global, analyzer)::InferenceState

    return analyze_frame!(analyzer, frame)
end

# This is very naive HACK to re-use `AbstractInterpreter`'s slot type approximation for
# assignments of abstract global variables, which are represented as toplevel symbols at this point;
# the idea is just to transform them into slot from symbol and use their approximated type
# on their assignment (see `finish(::InferenceState, ::AbstractAnalyzer)`).
# NOTE that `transform_abstract_global_symbols!` will produce really invalid code for
# actual interpretation or execution, but all the statements won't be interpreted anymore
# by `ConcreteInterpreter` nor executed by the native compilation pipeline anyway
function transform_abstract_global_symbols!(src::CodeInfo, analyzer::AbstractAnalyzer)
    nslots = length(src.slotnames)
    abstract_global_variables = Dict{Symbol,Int}()
    concretized = get_concretized(analyzer)

    # linear scan, and find assignments of abstract global variables
    for (i, stmt) in enumerate(src.code::Vector{Any})
        if !(concretized[i])
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, Symbol)
                    if !haskey(abstract_global_variables, lhs)
                        nslots += 1
                        push!(abstract_global_variables, lhs => nslots)
                    end
                end
            end
        end
    end

    prewalk_and_transform!(src) do @nospecialize(x), scope::Vector{Symbol}
        if isa(x, Symbol)
            slot = get(abstract_global_variables, x, nothing)
            isnothing(slot) || return SlotNumber(slot)
        end
        return x
    end

    resize!(src.slotnames, nslots)
    resize!(src.slotflags, nslots)
    for (slotname, idx) in abstract_global_variables
        src.slotnames[idx] = slotname
    end

    set_global_slots!(analyzer, Dict(idx => slotname for (slotname, idx) in abstract_global_variables))

    return src
end

# resolve toplevel symbols (and other expressions like `:foreigncall`) within `src`
# so that it is eligible for abstractintepret and optimization
# TODO `jl_resolve_globals_in_ir` may throw, and we should redirect the error to `ToplevelErrorReport`
function resolve_toplevel_symbols!(src::CodeInfo, mod::Module)
    @ccall jl_resolve_globals_in_ir(
        #=jl_array_t *stmts=# src.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# svec()::Any,
        #=int binding_effects=# 0::Int)::Cvoid
    return src
end
