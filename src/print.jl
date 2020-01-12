# entry
# -----

print_report(frame::Frame; kwargs...) = print_report(stdout, frame; kwargs...)
function print_report(io::IO, frame::Frame)
  reports = frame.reports

  n = length(reports)
  if n === 0
    printstyled(io, "No errors !"; color = :green)
    return
  end

  printstyled(io, n, " errors found\n"; color = :red)
  wrote_lins = Set{UInt64}()
  for er in reports
    depth = print_callstack(io, er.lin, er.frame, wrote_lins; duplicate_lines = false)
    ers = report_string(er)
    printstyled(io, '│'^depth, ' '; color = :cyan)
    printstyled(io, ers, '\n'; bold = true, color = :red)
    printstyled(io, '│'^(depth-1), '└', '\n'; color = :cyan)
  end
end

print_report(er::ErrorReport; kwargs...) = print_report(stdout, er; kwargs...)
function print_report(io::IO, er::ErrorReport)
  ers = report_string(er)
  print(io, "Error: ")
  printstyled(io, ers, '\n'; color = :red)

  println(io, "Calltrace:")
  print_callstack(io, er.lin, er.frame)
  return
end

# report
# ------

report_string(er::InvalidBuiltinCallErrorReport) =
  string("invalid builtin function call: ", tt_signature(er.tt))

report_string(er::NoMethodErrorReport) =
  string("no method matching signature: ", tt_signature(er.tt))

report_string(er::ConditionErrorReport) =
  string("non-boolean (", er.t, ") found in boolean context")

# location
# --------

# IDEA: type annotate source lines with profiled types
function print_callstack(io, lin, frame, wrote_lins::Set{UInt64} = Set{UInt64}(); duplicate_lines::Bool = true)
  lin_hash = hash(lin)
  should_print = lin_hash ∉ wrote_lins || duplicate_lines
  push!(wrote_lins, lin_hash)

  if lin.inlined_at === 0 && is_root(frame)
    # reached the initial statement of a root frame
    should_print && print_location(io, lin, 0)
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
  depth = print_callstack(io, prev_lin, prev_frame, wrote_lins; duplicate_lines = duplicate_lines)
  should_print && print_location(io, lin, depth)
  return depth + 1
end

function print_location(io, lin, depth)
  loc = string('│'^depth, "┌ @ ", lin.file, ":", lin.line)
  path = fullpath(string(lin.file))
  source_line = !isfile(path) ? string(lin.line) : strip(readlines(path)[lin.line])
  printstyled(io, loc; color = :cyan)
  println(io, ' ', source_line)
end

function basepath(filename)
  srcdir = joinpath(Sys.BINDIR, "..","..","base")
  releasedir = joinpath(Sys.BINDIR, "..","share","julia","base")
  normpath(joinpath(isdir(srcdir) ? srcdir : releasedir, filename))
end
function fullpath(filename)
  path = isabspath(filename) ? filename : basepath(filename)
  return try
    realpath(path)
  catch
    path
  end
end
