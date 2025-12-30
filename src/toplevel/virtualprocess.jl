"""
    ToplevelErrorReport

An interface type of error reports that JET collects while top-level concrete interpration.
All `ToplevelErrorReport` should have the following fields:
- `file::String`: the path to the file containing the interpretation context
- `line::Int`: the line number in the file containing the interpretation context

See also: [`virtual_process`](@ref), [`ConcreteInterpreter`](@ref)
"""
abstract type ToplevelErrorReport end

"""
    ToplevelErrorReport()

In order for `Report <: ToplevelErrorReport` to implement the interface,
it should satisfy the following requirements:

- **Required fields** \\
  `Report` should have the following fields:
  * `file::String`: the filename of this error
  * `line::Int`: the line number of this error

- **Required overloads** \\

  * [`JETInterface.print_report(io::IO, report::Report)`](@ref print_report)
"""
ToplevelErrorReport()

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

struct ParseErrorReport <: ToplevelErrorReport
    diagnostic::JS.Diagnostic
    source::JS.SourceFile
    file::String
    line::Int
    function ParseErrorReport(diagnostic::JS.Diagnostic, source::JS.SourceFile)
        line = JS.source_line(source, JS.first_byte(diagnostic))
        return new(diagnostic, source, source.filename::String, line)
    end
end
# don't show stacktrace for syntax errors
print_report(io::IO, report::ParseErrorReport) =
    JS.show_diagnostic(io, report.diagnostic, report.source)

# TODO Use JuliaLowering.jl
struct MacroExpansionErrorReport <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int
end
function print_report(io::IO, report::MacroExpansionErrorReport)
    println(io, "Macro expansion error:")
    showerror(io, report.err, report.st)
end

# TODO Use JuliaLowering.jl
struct LoweringErrorReport <: ToplevelErrorReport
    err
    file::String
    line::Int
    st::Base.StackTraces.StackTrace
    LoweringErrorReport(@nospecialize(err), file::String, line::Int, st::Base.StackTraces.StackTrace) = new(err, file, line, st)
    LoweringErrorReport(@nospecialize(msg), file::String, line::Int) = new(msg, file, line)
end
function print_report(io::IO, report::LoweringErrorReport)
    if isdefined(report, :st)
        println(io, "Lowering error:")
        showerror(io, report.err, report.st)
    else
        showerror(io, ErrorException(lazy"syntax: $(report.err)"))
    end
end

# wraps general errors from actual execution
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    st::Base.StackTraces.StackTrace
    file::String
    line::Int
    function ActualErrorWrapped(@nospecialize(err), st, file, line)
        return new(err, st, file, line)
    end
end
# TODO: add context information
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

# a special exception type that is supposed to be thrown only by `JuliaInterpreter.lookup(::ConcreteInterpreter)`
struct MissingConcretizationError <: Exception
    isconst::Bool
    var::GlobalRef
end

struct MissingConcretizationErrorReport <: ToplevelErrorReport
    isconst::Bool
    var::GlobalRef
    file::String
    line::Int
end
function print_report(io::IO, report::MissingConcretizationErrorReport)
    (; isconst, var) = report
    (; mod, name) = var
    recommended_pattern = isconst ? ":(const $name = x_)" : ":($name = x_)"
    msg = """
    `$mod.$name` is not concretized but JET needs to use its actual value in order to define types or methods.
    """
    if !isconst
        msg *= """
        - If this binding can be declared as a constant, try to declare it as a constant (i.e. `const $name = ...`).
        """
    end
    msg *= """- You may need to specify `$recommended_pattern` pattern to the `concretization_patterns`
      configuration to allow JET to actually evaluate this binding, e.g.,
      `report_file("path/to/file.jl"; concretization_patterns = [$recommended_pattern])`.
    """
    msg *= """- If the above $(isconst ? "approach does" : "approaches do") not work, try `concretization_patterns = [:(x_)]` to
      concretize all top-level code in the module (recommended as a last resort since it
      would incur any side effects in your code and may cause the analysis to take longer time).
    """
    print(io, msg)
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

  !!! warning "Deprecated"
      This configuration is deprecated since JET v0.11.
      In particular, if you were using this configuration with [`report_package`](@ref),
      you should load your package in advance and use the
      [`target_modules` configuration](@ref result-config)
      to achieve the same filtering behavior
      ```julia-repl
      julia> report_package("MyPkg"; target_defined_modules=true) # deprecated

      julia> using MyPkg; report_package(MyPkg; target_modules=(MyPkg,)) # preferred
      ```
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

  Also see: [`report_file`](@ref), [`watch_file`](@ref), [`report_package`](@ref)
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
    pkgid::Union{Nothing,PkgId}
    context::Module
    analyze_from_definitions::Union{Bool,Symbol}
    concretization_patterns::Vector{Any}
    virtualize::Bool
    toplevel_logger # ::Union{Nothing,IO}
    function ToplevelConfig(
        pkgid::Union{Nothing,PkgId} = nothing;
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

const toplevel_logger_io_lock = ReentrantLock()

@nospecialize
toplevel_logger(f, config::ToplevelConfig; kwargs...) =
    toplevel_logger(f, config.toplevel_logger; kwargs...)
function toplevel_logger(
        f, io;
        filter = ≥(DEFAULT_LOGGER_LEVEL),
        pre = nothing
    )
    isa(io, IO) || return false
    level = jet_logger_level(io)
    filter(level) || return
    # N.B. When calling this logger from a parallel execution task (`Threads.@spawn`),
    # particularly when calling multiple `print`s, irregular scrambling of print
    # behavior was observed (Julia v1.12).
    # The workaround here is that we first use `IOBuffer` to initially obtain the string
    # that should be printed, and then perform the actual IO writing with a single `print`
    # call (while also setting a global lock for IO writing).
    # By doing this, printing does not seem to get scrambled when called from `Threads.@spawn`.
    buf = IOBuffer()
    ioctx = IOContext(buf, io)
    pre === nothing || pre(ioctx)
    print(ioctx, "[toplevel-", JET_LOGGER_LEVELS[level], "] ")
    f(ioctx)
    log = String(take!(buf))
    if Threads.nthreads(:interactive) == 0 && Threads.nthreads(:default) == 1
        # In this case, the original task is busy, so without sequential execution,
        # logs will not be output
        print(io, log)
    else
        # Without `Threads.@spawn :interactive`, when the caller's task thread is busy,
        # task switching may not occur and logs may not appear
        wait(Threads.@spawn :interactive @lock toplevel_logger_io_lock print(io, log))
    end
end
@specialize

const Actual2Virtual = Pair{Module,Module}

const ModuleRangeInfo = Pair{UnitRange{Int},Module}
struct AnalyzedFileInfo
    module_range_infos::Vector{ModuleRangeInfo}
end
AnalyzedFileInfo() = AnalyzedFileInfo(ModuleRangeInfo[])

struct SignatureInfo
    filename::String
    mod::Module
    tt::Type
    src::CodeInfo
    SignatureInfo(
        filename::AbstractString, mod::Module, @nospecialize(tt::Type), src::CodeInfo
    ) = new(filename, mod, tt, src)
end

"""
    res::VirtualProcessResult

- `res.analyzed_files::Dict{String,AnalyzedFileInfo}`: files that have been analyzed with
    their corresponding module analyzed_files attached.
- `res.toplevel_error_reports::Vector{ToplevelErrorReport}`: toplevel errors found during the
    text parsing or partial (actual) interpretation; these reports are "critical" and should
    have precedence over `inference_error_reports`
- `res.inference_error_reports::Vector{InferenceErrorReport}`: possible error reports found
    by `ToplevelAbstractAnalyzer`
- `res.signature_infos`: signatures of methods defined within the analyzed files
- `res.actual2virtual::$Actual2Virtual`: keeps actual and virtual module
"""
struct VirtualProcessResult
    analyzed_files::Dict{String,AnalyzedFileInfo}
    toplevel_error_reports::Vector{ToplevelErrorReport}
    inference_error_reports::Vector{InferenceErrorReport}
    signature_infos::Vector{SignatureInfo}
    actual2virtual::Union{Actual2Virtual,Nothing}
    VirtualProcessResult(actual2virtual::Union{Actual2Virtual,Nothing}) =
        new(Dict{String,AnalyzedFileInfo}(),
            ToplevelErrorReport[],
            InferenceErrorReport[],
            Type[],
            actual2virtual)
end

function defined_modules(res::VirtualProcessResult)
    ret = Set{Module}()
    for (_, analyzed_file_info) in res.analyzed_files
        for module_range_info in analyzed_file_info.module_range_infos
            push!(ret, last(module_range_info))
        end
    end
    ret
end
included_files(res::VirtualProcessResult) = keys(res.analyzed_files)

"""
    InterpretationState

Holds the state needed by ConcreteInterpreter implementations.
"""
mutable struct InterpretationState
    const filename::String
    curline::Int
    const dependencies::Set{Symbol}
    const context::Module
    const config::ToplevelConfig
    const res::VirtualProcessResult
    const pkg_mod_depth::Int
    const files_stack::Vector{String}
    isfailed::Bool
end
function InterpretationState(state::InterpretationState;
                             filename::String = state.filename,
                             curline::Int = state.curline,
                             dependencies::Set{Symbol} = state.dependencies,
                             context::Module = state.context,
                             pkg_mod_depth::Int = state.pkg_mod_depth)
    # these fields should be shared across all states
    config::ToplevelConfig = state.config
    res::VirtualProcessResult = state.res
    files_stack::Vector{String} = state.files_stack
    # these fields should be initialized with default values
    isfailed = false
    return InterpretationState(
        filename,
        curline,
        dependencies,
        context,
        config,
        res,
        pkg_mod_depth,
        files_stack,
        isfailed)
end

"""
    abstract type ConcreteInterpreter <: JuliaInterpreter.Interpreter end

An interface to inject code into JET's virtual process via JuliaInterpreter's interpretation.

Subtypes are expected to implement:
- `InterpretationState(interp::T) -> InterpretationState` - return the interpreter state
- `ConcreteInterpreter(interp::T, state::InterpretationState) -> T` - create new interpreter with state
- `ToplevelAbstractAnalyzer(interp::T) -> analyzer::ToplevelAbstractAnalyzer` - return the analyzer for this interpreter
"""
:(ConcreteInterpreter)

@noinline function InterpretationState(interp::ConcreteInterpreter)
    InterpType = nameof(typeof(interp))
    error(lazy"""
    Missing `JET.ConcreteInterpreter` API:
    `$InterpType` is required to implement the `JET.InterpretationState(interp::$InterpType) -> JET.InterpretationState` interface.
    """)
end

@noinline function ConcreteInterpreter(interp::ConcreteInterpreter, ::InterpretationState)
    InterpType = nameof(typeof(interp))
    error(lazy"""
    Missing `JET.ConcreteInterpreter` API:
    `$InterpType` is required to implement the `JET.ConcreteInterpreter(interp::$InterpType, state::JET.InterpretationState) -> interp::$InterpType` interface.
    """)
end

@noinline function ToplevelAbstractAnalyzer(interp::ConcreteInterpreter)
    InterpType = nameof(typeof(interp))
    error(lazy"""
    Missing `JET.ConcreteInterpreter` API:
    `$InterpType` is required to implement the `JET.AbstractAnalyzer(interp::$InterpType) -> analyzer::JET.ToplevelAbstractAnalyzer` interface.
    """)
end

"""
    JETConcreteInterpreter

The default implementation of ConcreteInterpreter used by JET's virtual process.
"""
struct JETConcreteInterpreter{Analyzer<:ToplevelAbstractAnalyzer} <: ConcreteInterpreter
    analyzer::Analyzer
    state::InterpretationState
    JETConcreteInterpreter(analyzer::Analyzer) where Analyzer<:ToplevelAbstractAnalyzer = new{Analyzer}(analyzer)
    JETConcreteInterpreter(analyzer::Analyzer, state::InterpretationState) where Analyzer<:ToplevelAbstractAnalyzer = new{Analyzer}(analyzer, state)
end

# `ConcreteInterpreter` required interface
InterpretationState(interp::JETConcreteInterpreter) = interp.state
ConcreteInterpreter(interp::JETConcreteInterpreter, state::InterpretationState) = JETConcreteInterpreter(interp.analyzer, state)
ToplevelAbstractAnalyzer(interp::JETConcreteInterpreter) = interp.analyzer

# `ConcreteInterpreter` optional interface
interpret_world(::JETConcreteInterpreter) = JET_INTERPRET_WORLD[]

"""
    virtual_process(interp::ConcreteInterpreter,
                    x::Union{AbstractString,JS.SyntaxNode},
                    filename::AbstractString,
                    config::ToplevelConfig;
                    overrideex::Union{Nothing,Expr}=nothing) -> res::VirtualProcessResult

Simulates Julia's toplevel execution and collects error points, and finally returns `VirtualProcessResult`.

This function first parses `s::AbstractString` into `toplevelnode::JS.SyntaxNode` and then
iterate the following steps on each code block (`blk`) of `toplevelnode`:
1. if `blk` is a `:module` expression, recursively enters analysis into an newly defined
   virtual module
2. `lower`s `blk` into `:thunk` expression `lwr` (macros are also expanded in this step)
3. if the context module is virtualized, replaces self-references of the original context
   module with virtualized one: see `fix_self_references`
4. `ConcreteInterpreter` partially interprets some statements in `lwr` that should not be
   abstracted away (e.g. a `:method` definition); see also [`partially_interpret!`](@ref)
5. finally, `ToplevelAbstractAnalyzer` analyzes the remaining statements by abstract interpretation

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
function virtual_process(interp::ConcreteInterpreter,
                         x::Union{AbstractString,JS.SyntaxNode},
                         filename::AbstractString,
                         config::ToplevelConfig;
                         overrideex::Union{Nothing,Expr}=nothing)
    if config.virtualize
        actual  = config.context

        start = time()
        virtual = virtualize_module_context(actual)
        toplevel_logger(config) do @nospecialize(io::IO)
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
    res = VirtualProcessResult(actual2virtual)
    state = InterpretationState(filename, #=curline=#0, #=dependencies=#Set{Symbol}(),
                                context, config, res, #=pkg_mod_depth=#0,
                                #=files_stack=#String[], #=isfailed=#false)
    interp = ConcreteInterpreter(interp, state)
    try
        virtual_process!(interp, x, overrideex)
    finally
        Preferences.main_uuid[] = old_main_uuid
    end

    # analyze collected signatures unless critical error happened
    if should_analyze_from_definitions(config) && isempty(res.toplevel_error_reports)
        analyze_from_definitions!(interp, config)
    end

    # TODO move this aggregation to `analyze_from_definitions!`?
    unique!(aggregation_policy(ToplevelAbstractAnalyzer(interp)), res.inference_error_reports)

    return res
end

# analysis is initiated or entered into new file
function virtual_process!(interp::ConcreteInterpreter,
                          x::Union{AbstractString,JS.SyntaxNode},
                          overrideex::Union{Nothing,Expr}=nothing)
    _virtual_process!(interp, x; overrideex)
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
        @invokelatest(isdefinedglobal(sandbox, n)) && continue
        @invokelatest(isdefinedglobal(actual, n)) && push!(unames, Expr(:., n)) # an exported name can be undefined, and `using` of it will throw otherwise
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
function analyze_from_definitions!(interp::ConcreteInterpreter, config::ToplevelConfig)
    start = time()
    entrypoint = config.analyze_from_definitions
    res = InterpretationState(interp).res
    n_sigs = length(res.signature_infos)
    n_sigs == 0 && return nothing

    progress = PackageAnalysisProgress(n_sigs)

    toplevel_logger(config) do @nospecialize(io::IO)
        print(io, "analyzing from top-level definitions (0/$n_sigs)")
    end

    tasks = map(1:n_sigs) do i
        Threads.@spawn begin
            (; tt) = res.signature_infos[i]
            # Create a new analyzer with fresh local caches (`inf_cache` and `analysis_results`)
            # to avoid data races between concurrent signature analysis tasks
            analyzer = ToplevelAbstractAnalyzer(interp, non_toplevel_concretized)
            match = Base._which(tt;
                # NOTE use the latest world counter with `method_table(analyzer)` unwrapped,
                # otherwise it may use a world counter when this method isn't defined yet
                method_table = CC.method_table(analyzer),
                world = CC.get_inference_world(analyzer),
                raise = false)
            if (match !== nothing &&
                (!(entrypoint isa Symbol) || # implies `analyze_from_definitions===true`
                 match.method.name === entrypoint))
                @atomic progress.analyzed += 1
                result = analyze_method_signature!(analyzer,
                    match.method, match.spec_types, match.sparams)
                reports = get_reports(analyzer, result)
                isempty(reports) || @lock progress.reports_lock append!(progress.reports, reports)
            else
                # something went wrong
                toplevel_logger(config; pre=clearline) do @nospecialize(io::IO)
                    println(io, "couldn't find a single method matching the signature `", tt, "`")
                end
            end
            done = (@atomic progress.done += 1)
            current_next = @atomic progress.next_interval
            if done >= current_next
                @atomicreplace progress.next_interval current_next => current_next + progress.interval
                toplevel_logger(config; pre=clearline) do @nospecialize(io::IO)
                    analyzed = @atomic progress.analyzed
                    print(io, "analyzing from top-level definitions ($analyzed/$n_sigs)")
                end
            end
        end
    end

    waitall(tasks)

    append!(res.inference_error_reports, progress.reports)

    toplevel_logger(config; pre=clearline) do @nospecialize(io::IO)
        done = @atomic progress.done
        print(io, "analyzing from top-level definitions ($done/$n_sigs)")
    end
    toplevel_logger(config; pre=println) do @nospecialize(io::IO)
        sec = round(time() - start; digits = 3)
        analyzed = @atomic progress.analyzed
        println(io, "analyzed $analyzed top-level definitions (took $sec sec)")
    end

    return nothing
end

# Clear the entire line and return cursor to beginning
# \e[2K clears the entire line (K=erase line, 2=entire line)
# \r returns cursor to beginning of line
clearline(io) = print(io, "\e[2K\r")

function add_toplevel_error_report!(state::InterpretationState, @nospecialize report::ToplevelErrorReport)
    push!(state.res.toplevel_error_reports, report)
    state.isfailed = true
    nothing
end

function _virtual_process!(interp::ConcreteInterpreter,
                           s::AbstractString;
                           overrideex::Union{Nothing,Expr}=nothing)
    overrideex === nothing ||
        error("Given full code text `overrideex` does not need to be provided")

    start = time()
    state = InterpretationState(interp)
    (; config, filename) = state
    toplevel_logger(config) do @nospecialize(io::IO)
        println(io, "entered into $(filename)")
    end

    s = String(s)::String
    stream = JS.ParseStream(s)
    JS.parse!(stream; rule=:all)
    if isempty(stream.diagnostics)
        parsed = JS.build_tree(JS.SyntaxNode, stream; filename)
        _virtual_process!(interp, parsed)
    else
        sourcefile = JS.SourceFile(stream; filename)
        first_line = JS.source_line(sourcefile, JS.first_byte(stream))
        last_line = JS.source_line(sourcefile, JS.last_byte(stream))
        state.res.analyzed_files[filename] = AnalyzedFileInfo(
            ModuleRangeInfo[first_line:last_line => state.context])
        for diagnostic in stream.diagnostics
            add_toplevel_error_report!(state, ParseErrorReport(diagnostic, sourcefile))
        end
    end

    toplevel_logger(config) do @nospecialize(io::IO)
        sec = round(time() - start; digits = 3)
        println(io, " exited from $(filename) (took $sec sec)")
    end

    return state.res
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

struct VNode
    force_concretize::Bool
    node::JS.SyntaxNode
    x # set if `node` has been expanded (or overridden) # TODO Remove this after JL integration
    VNode(node::JS.SyntaxNode, force_concretize::Bool) = new(force_concretize, node)
    VNode(node::JS.SyntaxNode, @nospecialize(x), force_concretize::Bool) =
        new(force_concretize, node, x)
end

function push_vnode_stack!(vnodes::Vector{VNode}, newnode::JS.SyntaxNode, force_concretize::Bool)
    for i = JS.numchildren(newnode):-1:1
        push!(vnodes, VNode(newnode[i], force_concretize))
    end
    return vnodes
end
function push_vnode_stack!(vnodes::Vector{VNode}, node::JS.SyntaxNode, newex::Expr, force_concretize::Bool)
    for i = length(newex.args):-1:1
        push!(vnodes, VNode(node, newex.args[i], force_concretize))
    end
    return vnodes
end

function general_err_handler(@nospecialize(err), st, state::InterpretationState)
    report = ActualErrorWrapped(err, st, state.filename, state.curline)
    add_toplevel_error_report!(state, report)
    nothing
end

function macro_expansion_err_handler(@nospecialize(err), st, state::InterpretationState)
    report = MacroExpansionErrorReport(err, st, state.filename, state.curline)
    add_toplevel_error_report!(state, report)
    nothing
end

function lowering_err_handler(@nospecialize(err), st, state::InterpretationState)
    report = LoweringErrorReport(err, state.filename, state.curline, st)
    add_toplevel_error_report!(state, report)
    nothing
end

function eval_with_err_handling(state::InterpretationState, x::Expr)
    # `scrub_offset = 1`: `Core.eval`
    with_err_handling(general_err_handler, state; scrub_offset=1) do
        Core.eval(state.context, x)
    end
end

function macroexpand_with_err_handling(state::InterpretationState, x::Expr)
    # `scrub_offset = 2`: `macroexpand` -> kwfunc (`macroexpand`)
    with_err_handling(macro_expansion_err_handler, state; scrub_offset=2) do
        # XXX we want to non-recursive, sequential partial macro expansion here,
        # which allows us to collect more fine-grained error reports within macro expansions
        # but it can lead to invalid macro hygiene escaping because of https://github.com/JuliaLang/julia/issues/20241
        macroexpand(state.context, x; recursive = true)
    end
end

function lower_with_err_handling(interp::ConcreteInterpreter, ::JS.SyntaxNode, xblk::Expr)
    # `scrub_offset = 1`: `lower`
    state = InterpretationState(interp)

    xexpanded = @something macroexpand_with_err_handling(state, xblk) return nothing

    with_err_handling(lowering_err_handler, state; scrub_offset=1) do
        lwr = lower(state.context, xexpanded)
        if isexpr(lwr, :error)
            msg = first(lwr.args)
            add_toplevel_error_report!(state, LoweringErrorReport(msg, state.filename, state.curline))
            return nothing
        end
        return lwr
    end
end

function _virtual_process!(interp::ConcreteInterpreter,
                           toplevelnode::JS.SyntaxNode;
                           force_concretize::Bool = false,
                           # HACK allow expanded `Expr` to be analyzed instead of `toplevelnode`
                           overrideex::Union{Nothing,Expr}=nothing) # TODO Remove this after JL integration)
    if overrideex === nothing
        local toplevelkind = JS.kind(toplevelnode)
        (toplevelkind === K"toplevel" || toplevelkind === K"module") ||
            error(lazy"Can't analyze SyntaxNode with $(toplevelkind)")
    else
        @assert isexpr(overrideex, :toplevel)
    end

    state = InterpretationState(interp)

    sourcefile = JS.sourcefile(toplevelnode)
    first_line = JS.source_line(sourcefile, JS.first_byte(toplevelnode))
    last_line = JS.source_line(sourcefile, JS.last_byte(toplevelnode))
    analyzed_file_info = get!(AnalyzedFileInfo, state.res.analyzed_files, state.filename)
    if sourcefile.code == JS.sourcetext(toplevelnode)
        # If this `toplevelnode` is the file itself, recognize all ranges as this module context
        push!(analyzed_file_info.module_range_infos, 1:typemax(Int) => state.context)
    else
        push!(analyzed_file_info.module_range_infos, first_line:last_line => state.context)
    end
    state.curline = first_line
    push!(state.files_stack, state.filename)

    # transform, and then analyze sequentially
    # IDEA the following code has some of duplicated work with `JuliaInterpreter.ExprSpliter` and we may want to factor them out
    vnodes = VNode[]
    if overrideex === nothing
        if JS.kind(toplevelnode) === K"toplevel"
            push_vnode_stack!(vnodes, toplevelnode, force_concretize)
        else
            local blk = toplevelnode[2]
            @assert JS.kind(blk) === K"block"
            push_vnode_stack!(vnodes, blk, force_concretize)
        end
    else
        push_vnode_stack!(vnodes, toplevelnode, overrideex, force_concretize)
    end
    concretized = falses(0)
    while !isempty(vnodes)
        local ex = pop!(vnodes)
        (; node, force_concretize) = ex
        local x, isexpanded::Bool
        if isdefined(ex, :x)
            isexpanded = true
            x = ex.x
        else
            isexpanded = false
            x = Expr(node)
        end
        state.curline = JS.source_line(node)

        # apply user-specified concretization strategy, which is configured as expression
        # pattern match on surface level AST code representation; if any of the specified
        # patterns matches `x`, JET just concretizes everything involved with it
        # since patterns are expected to work on surface level AST, we should configure it
        # here before macro expansion and lowering
        if !force_concretize
            for pat in state.config.concretization_patterns
                if @capture(x, $pat)
                    toplevel_logger(state.config; filter=≥(JET_LOGGER_LEVEL_DEBUG)) do @nospecialize(io::IO)
                        x′ = striplines(normalise(x))
                        println(io, "concretization pattern `$pat` matched `$x′` at $(state.filename):$(state.curline)")
                    end
                    force_concretize = true
                    break
                end
            end
        end

        # although we will lower `x` after special-handling `:toplevel` and `:module` expressions,
        # expand `macrocall`s here because macro can arbitrarily generate those expressions
        if isexpr(x, :macrocall) ||
            # This kind of code occurs when a macro returns a `:toplevel` expression.
            # This expression contains information about the macro expansion position and module context,
            # but for now we just ignore that.
            isexpr(x, :var"hygienic-scope")
            newx = macroexpand_with_err_handling(state, x)

            # if any error happened during macro expansion, bail out now and continue
            isnothing(newx) && continue

            # special case and flatten the resulting expression expanded from `@doc` macro
            # the macro expands to a block expression and so it makes it difficult to specify
            # concretization pattern correctly since `@doc` macro is attached implicitly
            if first(x.args) === GlobalRef(Core, Symbol("@doc"))
                # `@doc` macro usually produces :block expression, but may also produce :toplevel
                # one when attached to a module expression
                @assert isexpr(newx, :block) || isexpr(newx, :toplevel)
                push_vnode_stack!(vnodes, node, newx, force_concretize)
            else
                push!(vnodes, VNode(node, newx, force_concretize))
            end

            continue
        end

        # flatten container expression
        if isexpr(x, :toplevel)
            if isexpanded
                push_vnode_stack!(vnodes, node, x, force_concretize)
            else
                push_vnode_stack!(vnodes, node, force_concretize)
            end
            continue
        end

        # handle `:module` definition and module usage;
        # should happen here because modules need to be loaded sequentially while
        # "toplevel definitions" inside of the loaded modules shouldn't be evaluated in a
        # context of `context` module

        lnn = LineNumberNode(state.curline, state.filename)

        if isexpr(x, :module)
            if isexpanded
                newblk = x.args[3]
                @assert isexpr(newblk, :block)
                overrideex = Expr(:toplevel, newblk.args...)
                x.args[3] = Expr(:block, lnn) # empty module's code body
                newcontext = eval_with_err_handling(state, x)
                isnothing(newcontext) && continue # error happened, e.g. duplicated naming
                newcontext = newcontext::Module
                newstate = InterpretationState(state;
                                               context = newcontext,
                                               pkg_mod_depth = state.pkg_mod_depth + 1,
                                               dependencies = Set{Symbol}())
                newinterp = ConcreteInterpreter(interp, newstate)
                _virtual_process!(newinterp, node;
                                  force_concretize, overrideex)
            else
                @assert JS.kind(node) === K"module"
                x.args[3] = Expr(:block, lnn) # empty module's code body
                newcontext = eval_with_err_handling(state, x)
                isnothing(newcontext) && continue # error happened, e.g. duplicated naming
                newcontext = newcontext::Module
                newstate = InterpretationState(state;
                                               context = newcontext,
                                               pkg_mod_depth = state.pkg_mod_depth + 1,
                                               dependencies = Set{Symbol}())
                newinterp = ConcreteInterpreter(interp, newstate)
                _virtual_process!(newinterp, node;
                                  force_concretize)
            end
            continue
        end

        # avoid wrapping 1-arg `:global` declaration into a block
        if isexpr(x, :global) && length(x.args) == 1 && only(x.args) isa Symbol
            eval_with_err_handling(state, x)
            continue
        end

        blk = Expr(:block, lnn, x) # attach current line number info
        lwr = lower_with_err_handling(interp, node, blk)

        isnothing(lwr) && continue # error happened during lowering
        isexpr(lwr, :thunk) || continue # literal

        src = only(lwr.args)::CodeInfo

        fix_self_references!(state.res.actual2virtual, src)

        state.isfailed = false
        if force_concretize
            JuliaInterpreter.finish!(interp, Frame(state.context, src), true)
            continue
        end
        partially_interpret!(interp, concretized, state.context, src)

        if bail_out_concretized(concretized, src)
            # bail out if nothing to analyze (just a performance optimization)
            continue
        elseif state.isfailed
            continue
        end

        analyzer = ToplevelAbstractAnalyzer(interp, concretized)

        result = analyze_toplevel!(analyzer, src, state.context)

        append!(state.res.inference_error_reports, get_reports(analyzer, result)) # collect error reports
    end

    pop!(state.files_stack)

    return state.res
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
    partially_interpret!(interp::ConcreteInterpreter, concretize::BitVector, mod::Module, src::CodeInfo)

Partially interprets statements in `src` using JuliaInterpreter.jl:
- concretizes "toplevel definitions", i.e. `:method`, `:struct_type`, `:abstract_type` and
  `:primitive_type` expressions and their dependencies
- concretizes user-specified toplevel code (see [`ToplevelConfig`](@ref))
- directly evaluates module usage expressions and report error of invalid module usages
  (TODO: enter into the loaded module and keep JET analysis)
- special-cases `include` calls so that top-level analysis recursively enters the included file
"""
function partially_interpret!(interp::ConcreteInterpreter, concretize::BitVector, mod::Module, src::CodeInfo)
    fill!(resize!(concretize, length(src.code)), false)
    select_statements!(concretize, mod, src)

    toplevel_logger(InterpretationState(interp).config; filter=≥(JET_LOGGER_LEVEL_DEBUG)) do @nospecialize(io::IO)
        println(io, "concretization plan at $(InterpretationState(interp).filename):$(InterpretationState(interp).curline):")
        LoweredCodeUtils.print_with_code(io, src, concretize)
    end

    # NOTE if `JuliaInterpreter.optimize!` may modify `src`, `src` and `concretize` can be inconsistent
    # here we create `JuliaInterpreter.Frame` by ourselves disabling the optimization (#277)
    frame = Frame(mod, src; optimize=false)
    LoweredCodeUtils.selective_eval_fromstart!(interp, frame, concretize, #=istoplevel=#true)

    return concretize
end

# select statements that should be concretized, and actually interpreted rather than abstracted
function select_statements!(concretize::BitVector, mod::Module, src::CodeInfo)
    cl = LoweredCodeUtils.CodeLinks(mod, src) # make `CodeEdges` hold `CodeLinks`?
    edges = LoweredCodeUtils.CodeEdges(src, cl)
    select_direct_requirement!(concretize, src.code, edges)
    select_dependencies!(concretize, src, edges, cl)
    return concretize
end

# just for testing, and debugging
select_statements(mod::Module, src::CodeInfo) = select_statements!(falses(length(src.code)), mod, src)
function select_statements(mod::Module, src::CodeInfo, names::Symbol...)
    idxs = findall(src.code) do @nospecialize stmt
        return any(names) do name
            isexpr(stmt, :call) || return false
            f = stmt.args[1]
            (f isa GlobalRef && f.name === :setglobal!) || return false
            length(stmt.args) == 4 || return false
            arg3 = stmt.args[3]
            if arg3 isa QuoteNode
                arg3 = arg3.value
            end
            return arg3 === name
        end
    end
    return select_statements(mod, src, idxs...)
end
function select_statements(mod::Module, src::CodeInfo, slots::SlotNumber...)
    cl = LoweredCodeUtils.CodeLinks(mod, src) # make `CodeEdges` hold `CodeLinks`?
    edges = LoweredCodeUtils.CodeEdges(src, cl)
    concretize = falses(length(src.code))
    for slot in slots
        for d in cl.slotassigns[slot.id]
            concretize[d] = true
        end
    end
    select_dependencies!(concretize, src, edges, cl)
    return concretize
end
function select_statements(mod::Module, src::CodeInfo, idxs::Int...)
    cl = LoweredCodeUtils.CodeLinks(mod, src) # make `CodeEdges` hold `CodeLinks`?
    edges = LoweredCodeUtils.CodeEdges(src, cl)
    concretize = falses(length(src.code))
    for idx = idxs
        concretize[idx] |= true
    end
    select_dependencies!(concretize, src, edges, cl)
    return concretize
end

function select_direct_requirement!(concretize, stmts, edges)
    for (idx, stmt) in enumerate(stmts)
        if (LoweredCodeUtils.ismethod(stmt) ||    # don't abstract away method definitions
            LoweredCodeUtils.istypedef(stmt) ||   # don't abstract away type definitions
            (isexpr(stmt, :call) && length(stmt.args) ≥ 1 && stmt.args[1] == GlobalRef(Core, :_defaultctors)) ||
            ismoduleusage(stmt) || # module usages are handled by `ConcreteInterpreter`
            isexpr(stmt, :globaldecl) ||
            isexpr(stmt, :latestworld))
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
        callee_matches(f, CC, :getproperty))
        if JuliaInterpreter.is_quotenode_egal(stmt.args[3], func)
            return true
        end
    end
    return false
end

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

# The purpose of this function is to find other statements that affect the execution of the
# statements choosen by `select_direct_dependencies!`. In other words, it extracts the
# minimal amount of code necessary to realize the required concretization.
# This technique is generally referred to as "program slicing," and specifically as
# "static program slicing". The basic algorithm implemented here is an extension of the one
# proposed in https://dl.acm.org/doi/10.5555/800078.802557, which is especially tuned for
# Julia's intermediate code representation.
function select_dependencies!(concretize::BitVector, src::CodeInfo, edges, cl)
    typedefs = LoweredCodeUtils.find_typedefs(src)
    cfg = CC.compute_basic_blocks(src.code)
    postdomtree = CC.construct_postdomtree(cfg.blocks)

    while true
        changed = false

        # Discover struct/method definitions at the beginning,
        # and propagate the definition requirements by tracking SSA precedessors.
        # (TODO maybe hoist this out of the loop?)
        changed |= LoweredCodeUtils.add_typedefs!(concretize, src, edges, typedefs, ())
        changed |= add_ssa_preds!(concretize, src, edges, ())

        # Mark some common inplace operations like `push!(x, ...)` and `setindex!(x, ...)`
        # when `x` has been marked already: otherwise we may end up using it with invalid state.
        # However, note that this is an incomplete approach, and note that the slice created
        # by this routine will not be sound because of this. This is because
        # `add_required_inplace!` only requires certain special-cased function calls and
        # does not take into account the possibility that arguments may be mutated in
        # arbitrary function calls. Ideally, function calls should be required while
        # considering the effects of these statements, or by some sort of an
        # inter-procedural program slicing
        changed |= add_required_inplace!(concretize, src, edges, cl)
        changed |= add_ssa_preds!(concretize, src, edges, ())

        # Mark necessary control flows.
        changed |= LoweredCodeUtils.add_control_flow!(concretize, src, cfg, postdomtree)
        changed |= add_ssa_preds!(concretize, src, edges, ())

        changed || break
    end

    # now mark the active goto nodes
    LoweredCodeUtils.add_active_gotos!(concretize, src, cfg, postdomtree)

    nothing
end

# TODO use proper world age for the lookups
function JuliaInterpreter.lookup(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node))
    if node isa Symbol
        node = GlobalRef(JuliaInterpreter.moduleof(frame), node)
    end
    if node isa GlobalRef
        if @invokelatest isdefinedglobal(node.mod, node.name)
            val = @invokelatest getglobal(node.mod, node.name)
            if val isa AbstractBindingState
                # HACK/FIXME Concretize `AbstractBindingState`
                if isdefined(val, :typ)
                    typ = val.typ
                    if typ isa Const
                        # allow ConcreteInterpreter to use actual concrete values that have
                        # been figured out by the abstract analyzer
                        return typ.val
                    end
                end
                throw(MissingConcretizationError(val.isconst, node))
            end
            return val
        else
            binding_states = get_binding_states(ToplevelAbstractAnalyzer(interp))
            partition = Base.lookup_binding_partition(Base.get_world_counter(), node)
            binding_state = get(binding_states, partition, nothing)
            if binding_state !== nothing
                if binding_state.maybeundef
                    # if this binding is undefined at this point, just make it raise `UndefVarError`
                else
                    # allow ConcreteInterpreter to use actual concrete values that have been
                    # figured out by the abstract analyzer
                    raise = true
                    if isdefined(binding_state, :typ)
                        typ = binding_state.typ
                        if typ isa Const
                            return typ.val
                        end
                    end
                    # if this binding is not concrete, then propagate this error type so that
                    # it can be handled by `handle_err`
                    raise && throw(MissingConcretizationError(binding_state.isconst, node))
                end
            end
        end
    end
    return @invoke JuliaInterpreter.lookup(interp::Interpreter, frame::Frame, node::Any)
end

function usemodule_with_err_handling(interp::ConcreteInterpreter, ex::Expr)
    state = InterpretationState(interp)
    mod = state.context
    if isexpr(ex, (:export, :public))
        @goto eval_usemodule
    end
    # TODO recursive analysis on dependencies?
    config = state.config
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
                for _ = 1:state.pkg_mod_depth
                    pushfirst!(modpath, :.)
                end
            else
                dependencies = state.dependencies
                if dep ∉ dependencies
                    depstr = String(dep)
                    depid = Base.identify_package(pkgid, depstr)
                    if depid === nothing
                        # IDEA better message in a case of `any(m::Module->dep===nameof(m), res.defined_modules))`?
                        local report = DependencyError(pkgid.name, depstr, state.filename, state.curline)
                        add_toplevel_error_report!(state, report)
                        return nothing
                    end
                    require_ex = :(const $dep = Base.require($depid))
                    # TODO better handling of loading errors that may happen here
                    require_res = with_err_handling(general_err_handler, state; scrub_offset=1) do
                        Core.eval(mod, require_ex)
                        true
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
    @label eval_usemodule
    # `scrub_offset = 1`: `Core.eval`
    with_err_handling(general_err_handler, state; scrub_offset=1) do
        Core.eval(mod, ex)
        true
    end
end

function JuliaInterpreter.step_expr!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node), istoplevel::Bool)
    @assert istoplevel "ConcreteInterpreter can only work for top-level code"

    if ismoduleusage(node)
        for ex in to_simple_module_usages(node)
            if usemodule_with_err_handling(interp, ex) === nothing
                break
            end
        end
        return frame.pc += 1
    end

    res = @invoke JuliaInterpreter.step_expr!(interp::Interpreter, frame::Frame, node::Any, istoplevel::Bool)

    should_analyze_from_definitions(InterpretationState(interp).config) && collect_toplevel_signature!(interp, frame, node)

    return res
end

function collect_toplevel_signature!(interp::ConcreteInterpreter, frame::Frame, @nospecialize(node))
    isexpr(node, :method, 3) || return nothing
    state = InterpretationState(interp)
    entrypoint = state.config.analyze_from_definitions
    if entrypoint isa Symbol
        methname = node.args[1]
        if methname isa GlobalRef
            methname = methname.name
        end
        if !(methname isa Symbol && methname === entrypoint)
            return nothing
        end
    end
    atype_params, sparams, #=linenode=#_ =
        JuliaInterpreter.lookup(frame, node.args[2])::SimpleVector
    tt = form_method_signature(atype_params::SimpleVector, sparams::SimpleVector)
    @assert !CC.has_free_typevars(tt) "free type variable left in signature_infos"
    if !(tt isa Type)
        @warn "Found non-Type method signature" tt
        return nothing
    end
    mod = JuliaInterpreter.moduleof(frame)
    src = JuliaInterpreter.lookup(frame, node.args[3])::Core.CodeInfo
    push!(state.res.signature_infos, SignatureInfo(state.filename, mod, tt, src))
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
    if isexpr(arg, :as)
        # `import Pkg as P`
        arg = first(arg.args)
    end
    if isexpr(arg, :.)
        # `using A`
        return Expr[x]
    end
    if isexpr(arg, :(:))
        # `using A: sym1, sym2, ...`
        a, as... = arg.args
        return Expr[Expr(x.head, ex) for ex in Expr[Expr(arg.head, a, a′) for a′ in as]]
    else
        # `export a`, `public a`
        return Expr[x]
    end
end

# This overload performs almost the same work as
# `JuliaInterpreter.evaluate_call!(::JuliaInterpreter.NonRecursiveInterpreter, ...)`
# but includes a few important adjustments specific to JET's virtual process:
# - Special handling for `include` calls: recursively apply JET analysis to included files.
# - Ignore C-side function definitions created via `Base._ccallable`. These definitions
#   are not namespaced in the module and can cause false-positive name conflict errors
#   when running analysis multiple times (see aviatesk/JET.jl#597).
function JuliaInterpreter.evaluate_call!(interp::ConcreteInterpreter, frame::Frame, fargs::Vector{Any}, enter_generated::Bool)
    args = fargs
    f = popfirst!(fargs)
    args = fargs # now it's really args
    isinclude(f) && return handle_include(interp, f, args)
    if f === Base._ccallable
        # skip concrete-interpretation of `jl_extern_c`
        if length(args) == 2 && args[1] isa Type && args[2] isa Type
            # ignore only if the method dispatch is successful
            return nothing
        else
            # otherwise just call it to trigger a method error
        end
    end
    return @invokelatest f(args...)
end

isinclude(@nospecialize f) = f isa Base.IncludeInto || (isa(f, Function) && nameof(f) === :include)

function handle_include(interp::ConcreteInterpreter, @nospecialize(include_func), args::Vector{Any})
    state = InterpretationState(interp)
    filename = state.filename
    line = state.curline
    include_context = state.context

    function add_actual_method_error_report!(args::Vector{Any})
        err = MethodError(include_func, args)
        local report = ActualErrorWrapped(err, [], filename, line)
        add_toplevel_error_report!(state, report)
    end

    nargs = length(args)
    if nargs == 1
        fname = only(args)
    elseif nargs == 2
        x, fname = args
        if isa(x, Module)
            include_context = x
            # TODO propagate appropriate `dependencies` to `newstate`
        elseif isa(x, Function)
            @warn "JET is unable to analyze `include(mapexpr::Function, filename::String)` call currently."
        else
            add_actual_method_error_report!(args)
            return nothing
        end
    else
        add_actual_method_error_report!(args)
        return nothing
    end
    if !isa(fname, String)
        add_actual_method_error_report!(args)
        return nothing
    end

    include_file = normpath(dirname(filename), fname)
    # handle recursive `include`s
    if include_file in state.files_stack
        local report = RecursiveIncludeErrorReport(include_file, copy(state.files_stack), filename, line)
        add_toplevel_error_report!(state, report)
        return nothing
    end

    included = try_read_file(interp, include_context, include_file)
    isnothing(included) && return nothing # typically no file error
    if !(included isa AbstractString || included isa JS.SyntaxNode)
        @warn lazy"Unexpected value returned from `try_read_file(interp::$(nameof(typeof(interp))), ...)" typeof(included)
        return nothing
    end

    newstate = InterpretationState(state;
                                   filename = include_file,
                                   curline = 0,
                                   context = include_context)
    newinterp = ConcreteInterpreter(interp, newstate)
    virtual_process!(newinterp, included)

    # TODO: actually, here we need to try to get the lastly analyzed result of the `_virtual_process!` call above
    nothing
end

function try_read_file(interp::ConcreteInterpreter, include_context::Module, include_file::AbstractString)
    # `scrub_offset = 1`: `f`
    return with_err_handling(general_err_handler, InterpretationState(interp); scrub_offset=1) do
        return read(include_file, String)
    end
end

const JET_VIRTUALPROCESS_FILE = Symbol(@__FILE__)
const JULIAINTERPRETER_BUILTINS_FILE = let
    jlfile = pathof(JuliaInterpreter)::String
    Symbol(normpath(jlfile, "..", "builtins.jl"))
end

# handle errors from toplevel user code
function JuliaInterpreter.handle_err(interp::ConcreteInterpreter, frame::Frame, @nospecialize(err))
    # catch stack trace
    bt = catch_backtrace()
    st = stacktrace(bt)

    # if the last error is raised from `evaluate_call!`, it's likely to be a serious error of JET
    lastframe = last(st)
    if lastframe.file === JET_VIRTUALPROCESS_FILE && lastframe.func === :evaluate_call!
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

        # if errors happen in `JuliaInterpreter.lookup`, we just discard all the stacktrace
        # and report `MissingConcretizationErrorReport`
        if frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :lookup
            break # keep `i = 0`
        end

        # find an error frame that happened at `@invokelatest f(fargs...)` in the overload
        # `JuliaInterpreter.evaluate_call!(::ConcreteInterpreter, ::Frame, ::Expr, ::Bool)`
        if frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :evaluate_call!
            i = j - 1 # offset: `evaluate_call!`
            break
        end

        # other general errors may happen at `JuliaInterpreter.collect_args`, etc.
        # we don't show any stacktrace for those errors (by keeping the original `i = 0`)
        # since they are hopefully self-explanatory
        continue
    end
    st = st[1:i]

    state = InterpretationState(interp)
    if err isa MissingConcretizationError
        report = MissingConcretizationErrorReport(err.isconst, err.var, state.filename, state.curline)
    else
        report = ActualErrorWrapped(err, st, state.filename, state.curline)
    end
    add_toplevel_error_report!(state, report)

    return nothing # stop further interpretation
end

function with_err_handling(f, err_handler, handler_args...; scrub_offset::Int)
    try
        return f()
    catch err
        bt = catch_backtrace()
        st = stacktrace(bt)

        # scrub the original stacktrace so that it only contains frames from user code
        for (i, frame) in enumerate(st)
            if frame.file === JET_VIRTUALPROCESS_FILE && frame.func === :with_err_handling
                st = st[1:(i-(3+scrub_offset))] # 3 denotes `with_err_handling` + `kwfunc(with_err_handling)` + `f`
                break
            end
        end

        return err_handler(err, st, handler_args...)
    end
end

function analyze_toplevel!(analyzer::ToplevelAbstractAnalyzer, src::CodeInfo, context_module::Module)
    mi = construct_toplevel_mi(src, context_module)

    result = InferenceResult(mi);
    init_result!(analyzer, result)
    # NOTE toplevel frames don't really need to be cached, but still better to be optimized
    # in order to get reasonable `UncaughtExceptionReport`, and also, otherwise
    # `typeinf_edge` won't add "toplevel-to-callee" edges
    frame = InferenceState(result, src, #=cache_mode=#:global, analyzer)::InferenceState

    return analyze_frame!(analyzer, frame)
end

function construct_toplevel_mi(src::Core.CodeInfo, context_module::Module)
    resolve_toplevel_symbols!(src, context_module)
    return @ccall jl_method_instance_for_thunk(src::Any, context_module::Any)::Ref{Core.MethodInstance}
end

# Perform some post-hoc mutation on lowered code, as expected by some abstract interpretation
# routines, especially for `:foreigncall` and `:cglobal`.
# TODO `jl_resolve_globals_in_ir` may throw, and we should redirect the error to `ToplevelErrorReport`
function resolve_toplevel_symbols!(src::CodeInfo, mod::Module)
    @ccall jl_resolve_definition_effects_in_ir(
        #=jl_array_t *stmts=# src.code::Any,
        #=jl_module_t *m=# mod::Any,
        #=jl_svec_t *sparam_vals=# svec()::Any,
        #=jl_value_t *binding_edge=# C_NULL::Ptr{Cvoid},
        #=int binding_effects=# 0::Int)::Cvoid
    return src
end
