module VSCode

import ..JET:
    PostProcessor,
    tofullpath,
    AbstractAnalyzer,
    JETToplevelResult,
    ToplevelErrorReport,
    ToplevelWarningReport,
    JETCallResult,
    InferenceErrorReport,
    get_reports,
    print_report,
    print_frame_sig,
    PrintConfig

# common
# ======

isuntitled(path::AbstractString)   = startswith(path, "Untitled")
tovscodepath(path::Symbol)         = tovscodepath(string(path))
tovscodepath(path::AbstractString) = isuntitled(path) ? path : tofullpath(path)

"""
    vscode_diagnostics_order(analyzer::AbstractAnalyzer) -> Bool

If `true` (default) a diagnostic will be reported at entry site.
Otherwise it's reported at error point.
"""
vscode_diagnostics_order(::AbstractAnalyzer) = true

# configuration
# =============

"""
Configurations for the VS Code integration.
These configurations are active only when used in
[the integrated Julia REPL](https://www.julia-vscode.org/docs/dev/userguide/runningcode/).

---
- `vscode_console_output::Union{Nothing,IO} = nothing` \\
  JET shows the analysis result in VS Code's "PROBLEMS" pane and inline
  annotations. If an `IO` object is supplied, JET also prints the result to that
  stream. When this option is `nothing`, the result appears only in the
  integrated views.
---
"""
struct VSCodeConfig end

function forward_to_console_output(res::Union{JETToplevelResult,JETCallResult};
                                   vscode_console_output::Union{Nothing,IO} = nothing,
                                   __jetconfigs...)
    isa(vscode_console_output, IO) && show(vscode_console_output, res)
end

# top-level
# =========

Base.showable(::MIME"application/vnd.julia-vscode.diagnostics", ::JETToplevelResult) = true
function Base.show(::IO, ::MIME"application/vnd.julia-vscode.diagnostics",
                   res::JETToplevelResult)
    forward_to_console_output(res; res.jetconfigs...)
    config = PrintConfig(; res.jetconfigs...)
    postprocessor = PostProcessor(res.res.actual2virtual)
    return vscode_diagnostics(res.analyzer,
                              get_reports(res),
                              res.source,
                              config;
                              postprocessor)
end
function vscode_diagnostics(analyzer::Analyzer,
                            reports::Vector{<:Union{ToplevelErrorReport,ToplevelWarningReport,InferenceErrorReport}},
                            source::AbstractString,
                            config::PrintConfig=PrintConfig();
                            postprocessor::PostProcessor = PostProcessor()) where {Analyzer<:AbstractAnalyzer}
    order = vscode_diagnostics_order(analyzer)
    return (; source = String(source),
              items = map(reports) do report
                  return vscode_diagnostic(report, config, postprocessor, order)
              end)
end

function vscode_diagnostic(report::Union{ToplevelErrorReport,ToplevelWarningReport},
                           ::PrintConfig, postprocessor::PostProcessor, ::Bool)
    # like other diagnostics in the editor, the first line is a summary in compact views
    msg = sprint(print_report, report; context = :summary_line => true)
    return (; msg = postprocessor(msg),
              path = tovscodepath(report.file),
              line = report.line,
              severity = report isa ToplevelWarningReport ? 1 : 0)
end

# inference
# =========

Base.showable(::MIME"application/vnd.julia-vscode.diagnostics", ::JETCallResult) = true
function Base.show(::IO, ::MIME"application/vnd.julia-vscode.diagnostics",
                   res::JETCallResult)
    forward_to_console_output(res; res.jetconfigs...)
    config = PrintConfig(; res.jetconfigs...)
    return vscode_diagnostics(res.analyzer,
                              get_reports(res),
                              res.source,
                              config)
end
function vscode_diagnostic(report::InferenceErrorReport, config::PrintConfig,
                           postprocessor::PostProcessor, order::Bool)
    showpoint = (order ? first : last)(report.vst)
    return (; msg = postprocessor(sprint(print_report, report, config)),
              path = tovscodepath(showpoint.file),
              line = showpoint.line,
              severity = 1, # 0: Error, 1: Warning, 2: Information, 3: Hint
              relatedInformation = map((order ? identity : reverse)(report.vst)) do frame
                  return (; msg = postprocessor(sprint(print_frame_sig, frame, config)),
                            path = tovscodepath(frame.file),
                            line = frame.line)
              end)
end

end # module VSCode
