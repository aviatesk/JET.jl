# entry
# -----

print_report(er::ErrorReport) = print_report(stdout, er; kwargs...)
function print_report(io::IO, er::ErrorReport)
  ers = report_string(er)
  print(io, "Error: ")
  printstyled(io, ers, '\n'; color = :red)

  println(io, "Calltrace:")
  print_callstack(io, er.lin, er.frame)
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
function print_callstack(io, lin, frame)
  if lin.inlined_at === 0 && is_root(frame)
    # reached the initial statement of a root frame
    print_location(io, lin, 0)
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
  depth = print_callstack(io, prev_lin, prev_frame)
  print_location(io, lin, depth)
  return depth + 1
end

function print_location(io, lin, depth)
  Base.with_output_color(:cyan, io) do io
    print(io, '│'^depth)
    print(io, "┌ @ ", lin.file, ":", lin.line)
  end
  path = fullpath(string(lin.file))
  line = if !isfile(path)
    string(lin.line)
  else
    strip(readlines(path)[lin.line])
  end
  println(io, ' ', line)
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
