# colors
# ------

const ERROR_COLOR = :light_red
const NOERROR_COLOR = :light_green
const RAIL_COLORS = [:bold, :light_cyan, :light_green, :blue]

# entry
# -----

print_report(frame::Frame; kwargs...) = print_report(stdout, frame; kwargs...)
function print_report(io::IO, frame::Frame; view = :inline)
  reports = frame.reports

  n = length(reports)
  if n === 0
    printstyled(io, "No errors !\n"; color = NOERROR_COLOR)
    return
  end

  if view === :inline
    printstyled(io, n, " errors found\n"; color = ERROR_COLOR)
    wrote_lins = Set{UInt64}()
    for er in reports
      depth = print_calltrace(io, er.lin, er.frame, wrote_lins; duplicate_lines = false)
      print_rails(io, depth-1)
      ers = report_string(er)
      printstyled(io, "│ ", ers, '\n'; color = ERROR_COLOR)
      print_rails(io, depth-1)
      printstyled(io, '└', '\n'; color = ERROR_COLOR)
    end
  elseif view === :separate
    for er in reports
      print_report(io, er)
      println()
    end
  else
    error("keyword argument view should either of :inline or :separate")
  end
  return
end

print_report(er::ErrorReport; kwargs...) = print_report(stdout, er; kwargs...)
function print_report(io::IO, er::ErrorReport)
  ers = report_string(er)
  print(io, "Error: ")
  printstyled(io, ers, '\n'; color = ERROR_COLOR)

  println(io, "Calltrace:")
  print_calltrace(io, er.lin, er.frame)
  return
end

# report
# ------

function report_string(er::ErrorReport)
  error("report_string(::$(typeof(er))) should be implemented")
end

function report_string(er::UndefVarErrorReport)
  be = er.maybe ? "may not be" : "is not"
  return if er.mod isa Module
    "variable $(er.mod).$(er.name) $be defined"
  else
    "variable $(er.name) $be defined in $(er.mod)"
  end
end

report_string(er::InvalidBuiltinCallErrorReport) =
  "invalid builtin function call: $(tt_to_signature_str(er.tt))"

report_string(er::NoMethodErrorReport) =
  "no method matching signature: $(tt_to_signature_str(er.tt))"

report_string(er::ConditionErrorReport) =
  "non-boolean ($(er.t)) found in boolean context"

# location
# --------

# IDEA: type annotate source lines with profiled types
print_calltrace(io, lin, frame, wrote_lins::Set{UInt64} = Set{UInt64}(); kwargs...) =
  _print_calltrace(io, lin, lin, frame, wrote_lins; kwargs...)

function _print_calltrace(io, lin, err_lin, frame, wrote_lins::Set{UInt64} = Set{UInt64}(); duplicate_lines::Bool = true)
  lin_hash = hash(lin)
  should_print = lin_hash ∉ wrote_lins || duplicate_lines
  push!(wrote_lins, lin_hash)

  if lin.inlined_at === 0 && is_root(frame)
    # reached the initial statement of a root frame
    should_print && print_location(io, lin, err_lin, 0)
    return 1
  end

  if lin.inlined_at === 0
    # callstack is still remaining in the caller frame, recur into it
    prev_lin = frame.caller.lin
    prev_frame = frame.caller.frame
  else
    prev_lin = frame.src.linetable[lin.inlined_at]
    prev_frame = frame
  end

  # prewalk
  depth = _print_calltrace(io, prev_lin, err_lin, prev_frame, wrote_lins; duplicate_lines = duplicate_lines)
  should_print && print_location(io, lin, err_lin, depth)
  return depth + 1
end

function print_location(io, lin, err_lin, depth)
  # rail
  print_rails(io, depth)
  if lin == err_lin
    printstyled(io, "┌ @ ", lin.file, ":", lin.line; color = ERROR_COLOR)
  else
    printstyled(io, "┌ @ ", lin.file, ":", lin.line; color = RAIL_COLORS[(depth+1) % length(RAIL_COLORS) + 1])
  end

  # source
  path = fullpath(string(lin.file))
  source_line = if isfile(path)
    strip(readlines(path)[lin.line])
  else
    string("within `", lin.method, ''') # when the file doesn't exist, e.g. REPL
  end
  println(io, ' ', source_line)
end

function print_rails(io, depth)
  n = length(RAIL_COLORS)
  for i in 1:depth
    c = RAIL_COLORS[i % n + 1]
    printstyled(io, '│'; color = c)
  end
end

# path
# ----

const SRC_DIR = joinpath(Sys.BINDIR, "..", "..", "base")
const RELEASE_DIR = joinpath(Sys.BINDIR, "..", "share", "julia", "base")
basepath(filename) =
  normpath(joinpath((@static isdir(SRC_DIR) ? SRC_DIR : RELEASE_DIR), filename))
function fullpath(filename)
  path = isabspath(filename) ? filename : basepath(filename)
  return try
    realpath(path)
  catch
    path
  end
end
