# entry
# =====

Base.show(io::IO, res::JETToplevelResult) = print_reports(io, res)
function print_reports(io::IO, res::JETToplevelResult)
    io = IOContext(io, :limit => true)
    return print_reports(io,
                         get_reports(res),
                         PostProcessor(res.res.actual2virtual);
                         res.jetconfigs...)
end

Base.show(io::IO, res::JETCallResult) = print_reports(io, res)
function print_reports(io::IO, res::JETCallResult)
    io = IOContext(io, :limit => true)
    return print_reports(io,
                         get_reports(res);
                         res.jetconfigs...)
end

# configuration
# =============

"""
Configurations for report printing.
These configurations apply when [JET's analysis results](@ref analysis-result)
are displayed in the REPL.

---
- `sourceinfo::Symbol = :default` \\
  Controls how file paths are displayed in stack traces and error reports.
  - `:full` - Expand all file paths to absolute paths
  - `:default` - Show paths as-is, prefixing `./` only for relative paths
  - `:compact` - Show basename only for absolute paths, relative paths unchanged
  - `:minimal` - For inference reports, show only `@ Module`, without a file
    path or line number. For top-level reports, treat this as `:compact`.
  - `:none` - For inference reports, omit the entire `@ Module path:line`
    location. For top-level reports, treat this as `:compact` because the source
    location is essential.
---
- `print_toplevel_success::Bool = false` \\
  **Deprecated**. This configuration has no effect and will be removed in a future release.
---
- `print_inference_success::Bool = true` \\
  **Deprecated**. This configuration will be removed in a future release.
  If `true`, print a message when no errors are found by an
  abstract-interpretation-based analysis pass.
  Use [`JET.has_problems(result)`](@ref has_problems) to check whether a result has problems.
---
- `stacktrace_types_limit::Union{Nothing, Int} = nothing` \\
  If `nothing`, limit the type depth of argument types in stack traces based on
  the display size.
  If a positive `Int`, limit the type depth to the given depth.
  If a non-positive `Int`, do not limit the type depth.
---
"""
struct PrintConfig
    print_inference_success::Bool
    sourceinfo::Symbol
    stacktrace_types_limit::Union{Nothing,Int}
    function PrintConfig(; print_toplevel_success::Union{Nothing,Bool} = nothing,
                           print_inference_success::Union{Nothing,Bool} = nothing,
                           sourceinfo::Symbol = :default,
                           stacktrace_types_limit::Union{Nothing,Int} = nothing,
                           _jetconfigs...)
        if print_toplevel_success !== nothing
            Base.depwarn("The `print_toplevel_success` configuration is deprecated and " *
                         "has no effect. It will be removed in a future release.",
                         :PrintConfig)
        end
        if print_inference_success === nothing
            print_inference_success = true
        else
            Base.depwarn("The `print_inference_success` configuration is deprecated and " *
                         "will be removed in a future release. Use " *
                         "`JET.has_problems(result)` to check whether a result has " *
                         "problems.", :PrintConfig)
        end
        if sourceinfo ∉ (:full, :default, :compact, :minimal, :none)
            throw(ArgumentError("Invalid sourceinfo: $sourceinfo. Must be one of :full, :default, :compact, :minimal, :none"))
        end
        return new(print_inference_success,
                   sourceinfo,
                   stacktrace_types_limit)
    end
end

# utility
# =======

const ERROR_COLOR = :light_red
const WARNING_COLOR = :yellow
const NOERROR_COLOR = :light_green
# TODO other nicer color scheme ?
const RAIL_COLORS = ( # Julia color + yellow
    :green,
    :magenta,
    :blue,
    :yellow,
)
const N_RAILS = length(RAIL_COLORS)
const LEFT_ROOF  = "═════ "
const RIGHT_ROOF = " ═════"
const HEADER_COLOR = :reverse
const ERROR_SIG_COLOR = :bold
const TYPE_ANNOTATION_COLOR = :light_cyan
const HINT_COLOR = :light_green

pluralize(n::Integer, one::AbstractString, more::AbstractString = string(one, 's')) =
    string(n, ' ', isone(n) ? one : more)

printlnstyled(args...; kwarg...) = printstyled(args..., '\n'; kwarg...)

function print_rails(io, depth)
    for i = 1:depth
        color = RAIL_COLORS[i%N_RAILS+1]
        printstyled(io, '│'; color)
    end
end

# Terminal control sequences, which take no display width: CSI sequences such as colors and
# OSC sequences such as hyperlinks
const CONTROL_SEQUENCE = r"\e\[[0-?]*[ -/]*[@-~]|\e\][^\a\e]*(?:\a|\e\\)"

# error messages in report messages can be colored
display_width(s::AbstractString) = textwidth(replace(s, CONTROL_SEQUENCE => ""))

# The display width of the widest line of the report message `s` as it is finally shown,
# i.e. after `postprocessor` and without terminal control sequences.
message_width(s::String, postprocessor::PostProcessor) =
    maximum(textwidth, split(replace(postprocessor(s), CONTROL_SEQUENCE => ""), '\n'))

# Closes the boxes from depth `to` through `from` on one line that is `width` wide: `└`
# closes the box at depth `to`, and each `┴` closes a deeper box.
function print_box_bottom(io::IO, width::Int, from::Int, to::Int, color::Symbol)
    print_rails(io, to-1)
    printlnstyled(io, '└', '┴'^(from-to), '─'^max(width-from, 1); color)
end

function format_path(path::AbstractString, sourceinfo::Symbol)
    if sourceinfo === :full
        return tofullpath(path)
    elseif sourceinfo === :compact
        return isabspath(path) ? basename(path) : path
    elseif sourceinfo === :default
        return isabspath(path) ? path : "./" * path
    else # :none
        return ""
    end
end

function with_bufferring(f, ctxargs...)
    buf = IOBuffer()
    io = IOContext(buf, ctxargs...)
    f(io)
    return String(take!(buf))
end

colorctx(io::IO) = :color => get(io, :color, false)

should_limit(::Nothing) = true
should_limit(stacktrace_types_limit::Int) = stacktrace_types_limit > 0
function type_depth_limit(io::IO, s::String; maxtypedepth::Union{Nothing,Int})
    sz = get(io, :displaysize, displaysize(io))::Tuple{Int, Int}
    return Base.type_depth_limit(s, max(sz[2], 120); maxdepth=maxtypedepth)
end

# toplevel
# ========

function print_reports(io::IO,
                       reports::Vector{ToplevelErrorReport},
                       postprocessor::PostProcessor = PostProcessor();
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    n = length(reports)
    n == 0 && return 0

    with_bufferring(colorctx(io), :displaysize => displaysize(io)) do io
        s = "Top-level analysis failed"
        # unlike the headers for found problems, this one tells that the analysis stopped
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = ERROR_COLOR, reverse = true)
        println(io, "JET stopped before completing the analysis.")
        println(io, "Fix the error below and rerun the analysis.")
        for report in reports
            print_toplevel_report(io, report, config, ERROR_COLOR, postprocessor)
        end
    end |> postprocessor |> (x->print(io::IO,x))

    return n
end

function print_reports(io::IO,
                       reports::Vector{ToplevelWarningReport},
                       postprocessor::PostProcessor = PostProcessor();
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    n = length(reports)
    n == 0 && return 0

    with_bufferring(colorctx(io), :displaysize => displaysize(io)) do io
        s = string(pluralize(n, "toplevel warning"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)
        for report in reports
            print_toplevel_report(io, report, config, WARNING_COLOR, postprocessor)
        end
    end |> postprocessor |> (x->print(io::IO,x))

    return n
end

function print_toplevel_report(io::IO,
                               report::Union{ToplevelErrorReport,ToplevelWarningReport},
                               config::PrintConfig, color::Symbol,
                               postprocessor::PostProcessor)
    ctx = colorctx(io)
    rail = with_bufferring(ctx) do io
        printstyled(io, "│ "; color)
    end

    # For top-level reports, :none and :minimal don't make sense, so treat them as :compact
    style = config.sourceinfo
    if style === :none || style === :minimal
        style = :compact
    end
    filepath = format_path(report.file, style)
    printlnstyled(io, "┌ @ ", filepath, ':', report.line, ' '; color)

    rows, cols = displaysize(io)
    # the message is printed after the two-column rail
    lines = with_bufferring(ctx, :displaysize => (rows, cols - 2)) do io
        print_report(io, report)
    end |> strip
    message = join(string.(rail, split(lines, '\n')), '\n')
    println(io, message)
    print_box_bottom(io, message_width(message, postprocessor), 1, 1, color)
    return nothing
end

function print_reports(io::IO,
                       reports::Vector{Union{ToplevelWarningReport,InferenceErrorReport}},
                       postprocessor::PostProcessor = PostProcessor();
                       jetconfigs...)
    warnings = ToplevelWarningReport[r for r in reports if r isa ToplevelWarningReport]
    errors = InferenceErrorReport[r for r in reports if r isa InferenceErrorReport]
    isempty(warnings) || print_reports(io, warnings, postprocessor; jetconfigs...)
    # "No errors detected" after warnings would read as if the analysis found no problems
    if isempty(warnings) || !isempty(errors)
        print_reports(io, errors, postprocessor; jetconfigs...)
    end
    return length(reports)
end

# A top-level report message starts with a summary that makes sense on its own, followed by
# a blank line and the body if any. Consumers that show the first line on its own, such as
# the JETLS CLI, set the `:summary_line` IO property so that the summary is printed on a
# single line; otherwise it is wrapped like the body.
summary_line(io::IO) = get(io, :summary_line, false)::Bool

function print_summary(io::IO, summary::String; body::Bool = true)
    if summary_line(io)
        print(io, summary)
    else
        print_wrapped(io, summary)
    end
    body && print(io, "\n\n")
end

# A summary that joins JET's `context` with an error `message` from Julia. The message is
# kept intact, like the rest of the error output: when the summary does not fit, the
# message starts a new line instead of being wrapped.
function print_summary(io::IO, context::String, message::String; body::Bool = true)
    summary = isempty(message) ? "$context." : "$context: $message"
    if summary_line(io) || display_width(summary) ≤ wrap_width(io)
        print(io, summary)
    else
        print_wrapped(io, "$context:")
        print(io, '\n', message)
    end
    body && print(io, "\n\n")
end

# Prints the error message as Julia shows it, after the context of the error: the first line
# of the `showerror` output joins the context in the summary, and the rest of the output,
# including the stacktrace, makes the body.
function print_error_report(io::IO, context::String, @nospecialize(err),
                            st::Base.StackTraces.StackTrace)
    msg = sprint(showerror, err, st; context=io)
    firstline, rest = let i = findfirst('\n', msg)
        i === nothing ? (msg, "") : (msg[1:prevind(msg, i)], msg[nextind(msg, i):end])
    end
    print_summary(io, context, firstline; body = !isempty(rest))
    stacktrace = markdown_rendering(io) ? findfirst(r"^Stacktrace:"m, rest) : nothing
    if stacktrace === nothing
        print(io, rest)
    else
        message = rstrip(rest[1:prevind(rest, first(stacktrace))], '\n')
        isempty(message) || print(io, message, "\n\n")
        print_markdown_codeblock(io, rest[first(stacktrace):end])
    end
end

# Consumers such as language servers set the `:markdown_rendering` IO property when they
# render report messages as Markdown. Preformatted text, such as stacktraces and source
# excerpts, then has to be printed in code blocks to keep Markdown from reflowing it.
markdown_rendering(io::IO) = get(io, :markdown_rendering, false)::Bool

# the fence is longer than any backtick run in `code`, which would otherwise close it
function print_markdown_codeblock(io::IO, code::AbstractString)
    fence = '`'^max(3, maximum(m -> length(m.match) + 1, eachmatch(r"`+", code); init=0))
    print(io, fence, '\n', code, '\n', fence, '\n')
end

# Messages are wrapped at `MESSAGE_WIDTH` columns, which keeps prose readable and fits in 92
# columns with a two-column line prefix. Displays that know their width, such as the REPL,
# pass it to `print_report` as the `:displaysize` IO property, so that narrower displays get
# narrower lines.
const MESSAGE_WIDTH = 90

function wrap_width(io::IO)
    displaysize = get(io, :displaysize, nothing)
    displaysize === nothing && return MESSAGE_WIDTH
    # keep narrow displays legible
    return clamp(last(displaysize::Tuple{Int,Int}), 40, MESSAGE_WIDTH)
end

# Prints `text` starting with `prefix` and wrapped at `wrap_width(io)` columns, indenting
# continuation lines by the width of `prefix`. Inline code spans are not broken.
function print_wrapped(io::IO, text::String; prefix::String = "")
    words = message_words(text)
    indent = textwidth(prefix)
    print(io, prefix)
    for (k, line) in enumerate(wrap_lines(map(display_width, words), wrap_width(io) - indent))
        k == 1 || print(io, '\n', ' '^indent)
        join(io, @view(words[line]), ' ')
    end
end
println_wrapped(io::IO, text::String; prefix::String = "") =
    (print_wrapped(io, text; prefix); println(io))

# Breaks words with the given widths into lines of at most `width` columns, choosing the
# breaks that minimize the sum of squared trailing spaces of the lines. The last line
# weighs a quarter as much as the others: it may be shorter, but it is not left with only
# a few words as in greedy wrapping. A word wider than `width` takes a line of its own.
function wrap_lines(widths::Vector{Int}, width::Int)
    n = length(widths)
    # `cost[j+1]` is the minimum cost for the first `j` words, whose last line starts at
    # word `start[j+1]`
    cost = fill(typemax(Int), n+1)
    start = zeros(Int, n+1)
    cost[1] = 0
    for j = 1:n
        linewidth = -1
        for i = j:-1:1
            linewidth += widths[i] + 1
            linewidth > width && i < j && break
            slack = max(width - linewidth, 0)
            c = cost[i] + (j == n ? slack^2 : 4slack^2)
            if c < cost[j+1]
                cost[j+1] = c
                start[j+1] = i
            end
        end
    end
    lines = UnitRange{Int}[]
    j = n + 1
    while j > 1
        pushfirst!(lines, start[j]:j-1)
        j = start[j]
    end
    return lines
end

# space-separated words of `text`, where an inline code span counts as a single word
function message_words(text::String)
    words = String[]
    start = firstindex(text)
    in_code = false
    for (i, c) in pairs(text)
        if c == ' ' && !in_code
            start < i && push!(words, text[start:prevind(text, i)])
            start = nextind(text, i)
        elseif c == '`'
            in_code = !in_code
        end
    end
    start ≤ lastindex(text) && push!(words, text[start:end])
    return words
end

# inference
# =========

function print_reports(io::IO,
                       reports::Vector{InferenceErrorReport},
                       postprocessor::PostProcessor = PostProcessor();
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    n = length(reports)

    if n == 0
        if config.print_inference_success
            printlnstyled(io, "No errors detected"; color = NOERROR_COLOR)
        end
        return 0
    end

    with_bufferring(colorctx(io)) do io
        s = string(pluralize(length(reports), "possible error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        # don't duplicated virtual stack frames for reports from the same toplevel frame
        toplevel_linfo_hash = hash(:dummy)
        wrote_linfos = Set{UInt64}()
        open_depth, open_width, open_color = 0, 0, ERROR_COLOR
        for report in reports
            new_toplevel_linfo_hash = hash(first(report.vst))
            if toplevel_linfo_hash != new_toplevel_linfo_hash
                toplevel_linfo_hash = new_toplevel_linfo_hash
                wrote_linfos = Set{UInt64}()
            end
            if open_depth > 0
                # close the boxes that the next report does not share
                to = min(first_printed_depth(report, wrote_linfos), open_depth)
                print_box_bottom(io, open_width, open_depth, to, open_color)
            end
            open_width = print_stack(io, report, config, wrote_linfos, postprocessor)::Int
            open_depth, open_color = length(report.vst), report_color(report)
        end
        print_box_bottom(io, open_width, open_depth, 1, open_color)
    end |> postprocessor |> (x->print(io::IO,x))

    return n
end

# The depth of the first frame that `print_stack` prints for `report`.
function first_printed_depth(report::InferenceErrorReport, wrote_linfos::Set{UInt64})
    vst = report.vst
    for depth = 1:length(vst)-1
        hash(vst[depth]) ∉ wrote_linfos && return depth
    end
    return length(vst)
end

# traverse abstract call stack, print frames, and return the width of the error message
function print_stack(io, report, config, wrote_linfos, postprocessor, depth = 1)
    if length(report.vst) == depth # error here
        return print_error_frame(io, report, config, postprocessor, depth)
    end

    frame = report.vst[depth]

    # cache current frame info
    linfo_hash = hash(frame)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    # print current frame and go into deeper
    if should_print
        color = RAIL_COLORS[(depth)%N_RAILS+1]
        print_rails(io, depth-1)
        printstyled(io, "┌ "; color)
        print_frame_sig(io, frame, config)
        print(io, " ")
        print_frame_loc(io, frame, config, color)
        println(io)
    end
    print_stack(io, report, config, wrote_linfos, postprocessor, depth + 1)
end

function print_frame_sig(io, frame, config)
    mi = frame.linfo
    m = mi.def
    if m isa Module
        Base.show_mi(io, mi, #=from_stackframe=#true)
    else
        if should_limit(config.stacktrace_types_limit)
            s = with_bufferring(colorctx(io), :backtrace=>true, :limit=>true) do io
                @invokelatest Base.StackTraces.show_spec_sig(io, m, mi.specTypes)
            end
            write(io, type_depth_limit(io, s; maxtypedepth=config.stacktrace_types_limit))
        else
            @invokelatest Base.StackTraces.show_spec_sig(IOContext(io, :backtrace=>true, :limit=>true), m, mi.specTypes)
        end
    end
end

function print_frame_loc(io, frame, config, color)
    if config.sourceinfo === :none
        return
    end
    def = frame.linfo.def
    mod = def isa Module ? def : def.module
    printstyled(io, "@ "; color)
    # IDEA use the same coloring as the Base stacktrace?
    # modulecolor = get!(Base.STACKTRACE_FIXEDCOLORS, mod) do
    #     popfirst!(Base.STACKTRACE_MODULECOLORS)
    # end
    modulecolor = color
    printstyled(io, mod; color = modulecolor)
    if config.sourceinfo !== :minimal
        path = format_path(String(frame.file), config.sourceinfo)
        line = fixed_line_number(frame)
        printstyled(io, ' ', path, ':', line; color)
    end
end

function fixed_line_number(frame)
    def = frame.linfo.def
    line = frame.line
    Δ = 0
    if def isa Method
        # Avoid source lookup while printing; only apply revisions already cached by Revise.
        key = CodeTracking.MethodInfoKey(def)
        locdefs = get(CodeTracking.method_info, key, nothing)
        if locdefs isa Vector{Tuple{LineNumberNode,Expr}} && !isempty(locdefs)
            newline = Int(last(locdefs)[1].line)
            if newline != 0
                Δ = newline - Int(def.line)
            end
        end
    end
    return line + Δ
end

function print_error_frame(io, report, config, postprocessor, depth)
    frame = report.vst[depth]
    color = report_color(report)

    print_rails(io, depth-1)
    printstyled(io, "┌ "; color)
    print_frame_sig(io, frame, config)
    print(io, " ")
    print_frame_loc(io, frame, config, color)
    println(io)

    message = with_bufferring(colorctx(io)) do io
        print_rails(io, depth-1)
        printstyled(io, "│ "; color)
        print_report(io, report, config)
    end
    println(io, message)
    return message_width(message, postprocessor)
end

function print_report(io::IO, report::InferenceErrorReport, config::PrintConfig=PrintConfig())
    color = report_color(report)
    msg = with_bufferring() do io
        print_report_message(io, report)
    end
    printstyled(io, msg; color)
    if print_signature(report)
        printstyled(io, ": "; color)
        print_signature(io, report.sig, config; bold=true)
    end
end

function print_signature(io, sig::Signature, config; kwargs...)
    for a in sig
        if should_limit(config.stacktrace_types_limit)
            s = with_bufferring(colorctx(io)) do io
                _print_signature(io, a; kwargs...)
            end
            write(io, type_depth_limit(io, s; maxtypedepth=config.stacktrace_types_limit))
        else
            _print_signature(io, a; kwargs...)
        end
    end
end
function _print_signature(io, @nospecialize(x); kwargs...)
    if isa(x, Type)
        if x == pairs(NamedTuple)
            # special case common verbose types related to keyword arguments
            printstyled(io, "::@Kwargs{…}"; color = TYPE_ANNOTATION_COLOR, kwargs...)
        elseif x !== Union{}
            io = IOContext(io, :backtrace=>true)
            printstyled(io, "::", x; color = TYPE_ANNOTATION_COLOR, kwargs...)
        end
    elseif isa(x, Repr)
        printstyled(io, sprint(show, x.val); kwargs...)
    elseif isa(x, AnnotationMaker)
        printstyled(io, x.switch ? '(' : ')'; kwargs...)
    elseif isa(x, ApplyTypeResult)
        printstyled(io, x.typ; kwargs...)
    elseif isa(x, IgnoreMarker)
        return
    elseif isa(x, QuoteNode)
        printstyled(io, "[quote]"; kwargs...)
    elseif isa(x, MethodInstance)
        printstyled(io, sprint(show_mi, x); kwargs...)
    elseif isa(x, GlobalRef) && (x.mod === Main || Base.isexported(x.mod, x.name))
        printstyled(io, x.name; kwargs...)
    else
        printstyled(io, x; kwargs...)
    end
end

# for printing Julia-representations
struct Repr
    val
    Repr(@nospecialize val) = new(val)
end
# for printing `x.y` -> `(x::T).y`, `f(x + y)` -> `f((x + y)::T)`
struct AnnotationMaker
    switch::Bool
end
# for printing `Core.apply_type(...)::Const(T)` -> `T`
struct ApplyTypeResult
    typ # ::Type
    ApplyTypeResult(@nospecialize typ) = new(typ)
end
struct IgnoreMarker end

# adapted from https://github.com/JuliaLang/julia/blob/0f11a7bb07d2d0d8413da05dadd47441705bf0dd/base/show.jl#L989-L1011
function show_mi(io::IO, l::MethodInstance)
    def = l.def
    if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            # print(io, "MethodInstance generator for ")
            show(io, def)
        else
            # print(io, "MethodInstance for ")
            Base.show_tuple_as_call(io, def.name, l.specTypes; qualified=true)
        end
    else
        # print(io, "Toplevel MethodInstance thunk")
        # # `thunk` is not very much information to go on. If this
        # # MethodInstance is part of a stacktrace, it gets location info
        # # added by other means.  But if it isn't, then we should try
        # # to print a little more identifying information.
        # if !from_stackframe
        #     linetable = l.uninferred.linetable
        #     line = isempty(linetable) ? "unknown" : (lt = linetable[1]; string(lt.file) * ':' * string(lt.line))
        #     print(io, " from ", def, " starting at ", line)
        # end
        print(io, "toplevel")
    end
end
