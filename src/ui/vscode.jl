module VSCode

import ..JET:
    gen_postprocess,
    tofullpath,
    AbstractAnalyzer,
    JETToplevelResult,
    ToplevelErrorReport,
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
vscode_diagnostics_order(analyzer::AbstractAnalyzer) = true

# configuration
# =============

"""
Configurations for the VSCode integration.
These configurations are active only when used in [the integrated Julia REPL](https://www.julia-vscode.org/docs/dev/userguide/runningcode/).

---
- `vscode_console_output::Union{Nothing,IO} = stdout` \\
  JET will show analysis result in VSCode's "PROBLEMS" pane and inline annotations.
  If `vscode_console_output::IO` is specified, JET will also print the result into the
  specified output stream in addition to showing the result in the integrated views.
  When `nothing`, the result will be only shown in the integrated views.
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
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.diagnostics",
                   res::JETToplevelResult)
    forward_to_console_output(res; res.jetconfigs...)
    config = PrintConfig(; res.jetconfigs...)
    postprocess = gen_postprocess(res.res.actual2virtual)
    return vscode_diagnostics(res.analyzer,
                              get_reports(res),
                              res.source,
                              config;
                              postprocess)
end
function vscode_diagnostics(analyzer::Analyzer,
                            reports::Vector{ToplevelErrorReport},
                            source::AbstractString,
                            config::PrintConfig=PrintConfig();
                            postprocess = identity) where {Analyzer<:AbstractAnalyzer}
    return (; source = String(source),
              items = map(reports) do report
                  return (; msg = postprocess(sprint(print_report, report)),
                            path = tovscodepath(report.file),
                            line = report.line,
                            severity = 0, # 0: Error, 1: Warning, 2: Information, 3: Hint
                            )
              end)
end

# inference
# =========

Base.showable(::MIME"application/vnd.julia-vscode.diagnostics", ::JETCallResult) = true
function Base.show(io::IO, ::MIME"application/vnd.julia-vscode.diagnostics",
                   res::JETCallResult)
    forward_to_console_output(res; res.jetconfigs...)
    config = PrintConfig(; res.jetconfigs...)
    return vscode_diagnostics(res.analyzer,
                              get_reports(res),
                              res.source,
                              config)
end
function vscode_diagnostics(analyzer::Analyzer,
                            reports::Vector{InferenceErrorReport},
                            source::AbstractString,
                            config::PrintConfig=PrintConfig();
                            postprocess = identity) where {Analyzer<:AbstractAnalyzer}
    order = vscode_diagnostics_order(analyzer)
    return (; source = String(source),
              items = map(reports) do report
                  showpoint = (order ? first : last)(report.vst)
                  msg = sprint(print_report, report)
                  return (; msg = postprocess(msg),
                            path = tovscodepath(showpoint.file),
                            line = showpoint.line,
                            severity = 1, # 0: Error, 1: Warning, 2: Information, 3: Hint
                            relatedInformation = map((order ? identity : reverse)(report.vst)) do frame
                                return (; msg = postprocess(sprint(print_frame_sig, frame, config)),
                                          path = tovscodepath(frame.file),
                                          line = frame.line,
                                          )
                            end,
                            )
              end)
end

end # module VSCode
