function Base_type_depth_limit(io::IO; maxtypedepth=1)
    sz = get(io, :displaysize, displaysize(io))::Tuple{Int, Int}
    return Base.type_depth_limit(String(take!(io)), max(sz[2], 120); maxdepth = maxtypedepth)
end

# entry
# =====

Base.show(io::IO, res::JETToplevelResult) = print_reports(io, res)
function print_reports(io::IO, res::JETToplevelResult)
    io = IOContext(io, :limit => true)
    return print_reports(io,
                         get_reports(res),
                         gen_postprocess(res.res.actual2virtual);
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
The configurations below will be active whenever `show`ing [JET's analysis result](@ref analysis-result) within REPL.

---
- `fullpath::Bool = false` \\
  Controls whether or not expand a file path to full path when printing analyzed call stack.
  Note that paths of Julia's `Base` files will also be expanded when set to `true`.
---
- `print_toplevel_success::Bool = false` \\
  If `true`, prints a message when there is no toplevel errors found.
---
- `print_inference_success::Bool = true` \\
  If `true`, print a message when there is no errors found in abstract interpretation based analysis pass.
---
"""
struct PrintConfig
    print_toplevel_success::Bool
    print_inference_success::Bool
    fullpath::Bool
    function PrintConfig(; print_toplevel_success::Bool  = false,
                           print_inference_success::Bool = true,
                           fullpath::Bool                = false,
                           __jetconfigs...)
        return new(print_toplevel_success,
                   print_inference_success,
                   fullpath)
    end
end

# utility
# =======

const ERROR_COLOR = :light_red
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

function with_bufferring(f, args...)
    buf = IOBuffer()
    io = IOContext(buf, args...)
    f(io)
    return String(take!(buf))
end

# toplevel
# ========

function print_reports(io::IO,
                       reports::Vector{ToplevelErrorReport},
                       @nospecialize(postprocess = identity);
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    n = length(reports)
    if n == 0
        if config.print_toplevel_success
            printlnstyled(io, "No toplevel errors detected"; color = NOERROR_COLOR)
        end
        return 0
    end

    arg = :color => get(io, :color, false)
    with_bufferring(arg) do io
        s = string(pluralize(n, "toplevel error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        color = ERROR_COLOR

        rail = with_bufferring(arg) do io
            printstyled(io, "│ "; color)
        end

        for report in reports
            filepath = (config.fullpath ? tofullpath : identity)(report.file)
            printlnstyled(io, "┌ @ ", filepath, ':', report.line, ' '; color)

            errlines = with_bufferring(arg) do io
                print_report(io, report)
            end |> strip
            join(io, string.(rail, split(errlines, '\n')), '\n')
            println(io)

            printlnstyled(io, '└', '─'^(length(s)-1); color)
        end
    end |> postprocess |> (x->print(io::IO,x))

    return n
end

# inference
# =========

function print_reports(io::IO,
                       reports::Vector{InferenceErrorReport},
                       @nospecialize(postprocess = identity);
                       jetconfigs...)
    config = PrintConfig(; jetconfigs...)

    n = length(reports)

    if n == 0
        if config.print_inference_success
            printlnstyled(io, "No errors detected"; color = NOERROR_COLOR)
        end
        return 0
    end

    with_bufferring(:color => get(io, :color, false)) do io
        s = string(pluralize(length(reports), "possible error"), " found")
        printlnstyled(io, LEFT_ROOF, s, RIGHT_ROOF; color = HEADER_COLOR)

        # don't duplicated virtual stack frames for reports from the same toplevel frame
        toplevel_linfo_hash = hash(:dummy)
        wrote_linfos = Set{UInt64}()
        for report in reports
            new_toplevel_linfo_hash = hash(first(report.vst))
            if toplevel_linfo_hash != new_toplevel_linfo_hash
                toplevel_linfo_hash = new_toplevel_linfo_hash
                wrote_linfos = Set{UInt64}()
            end
            print_stack(io, report, config, wrote_linfos)
        end
    end |> postprocess |> (x->print(io::IO,x))

    return n
end

# traverse abstract call stack, print frames
function print_stack(io, report, config, wrote_linfos, depth = 1)
    if length(report.vst) == depth # error here
        return print_error_frame(io, report, config, depth)
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
        print_frame_sig(io, frame)
        print(io, " ")
        print_frame_loc(io, frame, config, color)
        println(io)
    end
    print_stack(io, report, config, wrote_linfos, depth + 1)
end

function print_frame_sig(io, frame)
    mi = frame.linfo
    m = mi.def
    if m isa Module
        Base.show_mi(io, mi, #=from_stackframe=#true)
    else
        buf = IOBuffer()
        ioc = IOContext(buf, :backtrace=>true, :limit=>true)
        Base.StackTraces.show_spec_sig(ioc, m, mi.specTypes)
        io = IOContext(io, :backtrace=>true, :limit=>true)
        write(io, Base_type_depth_limit(buf; maxtypedepth=2));
    end
end

function print_frame_loc(io, frame, config, color)
    def = frame.linfo.def
    mod = def isa Module ? def : def.module
    path = String(frame.file)
    if config.fullpath
        path = tofullpath(path)
    elseif !isabspath(path)
        path = "./" * path
    end
    printstyled(io, "@ "; color)
    # IDEA use the same coloring as the Base stacktrace?
    # modulecolor = get!(Base.STACKTRACE_FIXEDCOLORS, mod) do
    #     popfirst!(Base.STACKTRACE_MODULECOLORS)
    # end
    modulecolor = color
    printstyled(io, mod; color = modulecolor)
    printstyled(io, ' ', path, ':', frame.line; color)
end

function print_error_frame(io, report, config, depth)
    frame = report.vst[depth]
    color = report_color(report)

    print_rails(io, depth-1)
    printstyled(io, "┌ "; color)
    print_frame_sig(io, frame)
    print(io, " ")
    print_frame_loc(io, frame, config, color)
    println(io)

    print_rails(io, depth-1)
    printstyled(io, "│ "; color)
    print_report(io, report)
    println(io)

    print_rails(io, depth-1)
    printlnstyled(io, '└', '─'^20; color)
end

function print_report(io::IO, report::InferenceErrorReport)
    color = report_color(report)
    msg = with_bufferring() do io
        print_report_message(io, report)
    end
    printstyled(io, msg; color)
    if print_signature(report)
        printstyled(io, ": "; color)
        print_signature(io, report.sig, (;); bold=true)
    end
end

function print_signature(io, sig::Signature, config; kwargs...)
    for a in sig
        buf = IOBuffer()
        _print_signature(buf, a, config; kwargs...)
        # Let's use maxtypedepth=2, so that unnamed
        # functions still show types we recognize.
        write(io, Base_type_depth_limit(buf; maxtypedepth=2));
    end
end
function _print_signature(io, @nospecialize(x), config; kwargs...)
    if isa(x, Type)
        if x == pairs(NamedTuple)
            if isdefined(Base, Symbol("@Kwargs"))
                printstyled(io, "::@Kwargs{…}"; color = TYPE_ANNOTATION_COLOR, kwargs...)
            else
                printstyled(io, "::Base.Pairs{…}"; color = TYPE_ANNOTATION_COLOR, kwargs...)
            end
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
