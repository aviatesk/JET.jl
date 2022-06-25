# entry
# =====

Base.show(io::IO, res::JETToplevelResult) = print_reports(io, res)
function print_reports(io::IO, res::JETToplevelResult)
    return print_reports(io,
                         get_reports(res),
                         gen_postprocess(res.res.actual2virtual);
                         res.jetconfigs...)
end

Base.show(io::IO, res::JETCallResult) = print_reports(io, res)
function print_reports(io::IO, res::JETCallResult)
    return print_reports(io,
                         get_reports(res);
                         res.jetconfigs...)
end

# entry for test
print_reports(args...; jetconfigs...) =
    return print_reports(stdout::IO, args...; jetconfigs...)

# configuration
# =============

"""
Configurations for report printing.
The configurations below will be active whenever `show`ing [JET's analysis result](@ref analysis-result) within REPL.

---
- `annotate_types::Bool = false` \\
  When set to `true`, annotates types when printing analyzed call stack.

  > Examples:
  * with `annotate_types = false` (default):
    ```julia-repl
    julia> @report_call sum("julia")
    ═════ 2 possible errors found ═════
    ┌ @ reduce.jl:549 Base.:(var"#sum#281")(pairs(NamedTuple()), #self#, a)
    │┌ @ reduce.jl:549 sum(identity, a)
    ││┌ @ reduce.jl:520 Base.:(var"#sum#280")(pairs(NamedTuple()), #self#, f, a)
    │││┌ @ reduce.jl:520 mapreduce(f, Base.add_sum, a)
    ││││┌ @ reduce.jl:294 Base.:(var"#mapreduce#277")(pairs(NamedTuple()), #self#, f, op, itr)
    │││││┌ @ reduce.jl:294 mapfoldl(f, op, itr)
    ││││││┌ @ reduce.jl:162 Base.:(var"#mapfoldl#273")(Base._InitialValue(), #self#, f, op, itr)
    │││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f, op, init, itr)
    ││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′, nt, itr′)
    │││││││││┌ @ reduce.jl:48 v = Base._foldl_impl(op, nt, itr)
    ││││││││││┌ @ reduce.jl:62 v = op(v, y[1])
    │││││││││││┌ @ reduce.jl:81 op.rf(acc, x)
    ││││││││││││┌ @ reduce.jl:24 x + y
    │││││││││││││ no matching method found for `+(::Char, ::Char)`: (x::Char + y::Char)::Union{}
    ││││││││││││└────────────────
    │││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op, itr)
    ││││││││││┌ @ reduce.jl:370 Base.reduce_empty_iter(op, itr, Base.IteratorEltype(itr))
    │││││││││││┌ @ reduce.jl:371 Base.reduce_empty(op, eltype(itr))
    ││││││││││││┌ @ reduce.jl:347 Base.reduce_empty(op.rf, T)
    │││││││││││││┌ @ reduce.jl:339 Base.reduce_empty(+, T)
    ││││││││││││││┌ @ reduce.jl:330 zero(T)
    │││││││││││││││ no matching method found for `zero(::Type{Char})`: zero(T::Type{Char})::Union{}
    ││││││││││││││└─────────────────
    ```
  * with `annotate_types = true`
    ```julia-repl
    julia> @report_call annotate_types = true sum("julia")
    ═════ 2 possible errors found ═════
    ┌ @ reduce.jl:549 Base.:(var"#sum#281")(pairs(NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(sum), a::String)::Union{}
    │┌ @ reduce.jl:549 sum(identity, a::String)::Union{}
    ││┌ @ reduce.jl:520 Base.:(var"#sum#280")(pairs(NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(sum), f::typeof(identity), a::String)::Union{}
    │││┌ @ reduce.jl:520 mapreduce(f::typeof(identity), Base.add_sum, a::String)::Union{}
    ││││┌ @ reduce.jl:294 Base.:(var"#mapreduce#277")(pairs(NamedTuple()::NamedTuple{(), Tuple{}})::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}}, #self#::typeof(mapreduce), f::typeof(identity), op::typeof(Base.add_sum), itr::String)::Union{}
    │││││┌ @ reduce.jl:294 mapfoldl(f::typeof(identity), op::typeof(Base.add_sum), itr::String)::Union{}
    ││││││┌ @ reduce.jl:162 Base.:(var"#mapfoldl#273")(Base._InitialValue()::Base._InitialValue, #self#::typeof(mapfoldl), f::typeof(identity), op::typeof(Base.add_sum), itr::String)::Union{}
    │││││││┌ @ reduce.jl:162 Base.mapfoldl_impl(f::typeof(identity), op::typeof(Base.add_sum), init::Base._InitialValue, itr::String)::Union{}
    ││││││││┌ @ reduce.jl:44 Base.foldl_impl(op′::Union{}, nt::Base._InitialValue, itr′::Union{})::Union{}
    │││││││││┌ @ reduce.jl:48 v = Base._foldl_impl(op::Base.BottomRF{typeof(Base.add_sum)}, nt::Base._InitialValue, itr::String)::Union{}
    ││││││││││┌ @ reduce.jl:62 v = op::Base.BottomRF{typeof(Base.add_sum)}(v::Char, (y::Tuple{Char, Int64})[1]::Char)::Union{}
    │││││││││││┌ @ reduce.jl:81 (op::Base.BottomRF{typeof(Base.add_sum)}).rf::typeof(Base.add_sum)(acc::Char, x::Char)::Union{}
    ││││││││││││┌ @ reduce.jl:24 (x::Char + y::Char)::Union{}
    │││││││││││││ no matching method found for `+(::Char, ::Char)`: (x::Char + y::Char)::Union{}
    ││││││││││││└────────────────
    │││││││││┌ @ reduce.jl:49 Base.reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String)::Union{}
    ││││││││││┌ @ reduce.jl:370 Base.reduce_empty_iter(op::Base.BottomRF{typeof(Base.add_sum)}, itr::String, Base.IteratorEltype(itr::String)::Base.HasEltype)::Union{}
    │││││││││││┌ @ reduce.jl:371 Base.reduce_empty(op::Base.BottomRF{typeof(Base.add_sum)}, eltype(itr::String)::Type{Char})::Union{}
    ││││││││││││┌ @ reduce.jl:347 Base.reduce_empty((op::Base.BottomRF{typeof(Base.add_sum)}).rf::typeof(Base.add_sum), T::Type{Char})::Union{}
    │││││││││││││┌ @ reduce.jl:339 Base.reduce_empty(+, T::Type{Char})::Union{}
    ││││││││││││││┌ @ reduce.jl:330 zero(T::Type{Char})::Union{}
    │││││││││││││││ no matching method found for `zero(::Type{Char})`: zero(T::Type{Char})::Union{}
    ││││││││││││││└─────────────────
    ```

  !!! note
      JET always annotates types when printing the error point, e.g. in the example above,
      the error points below are always type-annotated regardless of this configuration:
      - `no matching method found for call signature: Base.zero(_::Type{Char})`
      - `no matching method found for call signature: Base.+(x::Char, y::Char)`
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
    annotate_types::Bool
    fullpath::Bool
    @jetconfigurable PrintConfig(; print_toplevel_success::Bool  = false,
                                   print_inference_success::Bool = true,
                                   annotate_types::Bool          = false,
                                   fullpath::Bool                = false,
                                   ) =
        return new(print_toplevel_success,
                   print_inference_success,
                   annotate_types,
                   fullpath,
                   )
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
            s = string("┌ @ ", (config.fullpath ? tofullpath : identity)(string(report.file)), ':', report.line, ' ')
            printlnstyled(io, s; color)

            errlines = with_bufferring(arg) do io
                print_report(io, report)
            end |> strip
            println(io, join(string.(rail, split(errlines, '\n')), '\n'))

            s = string('└', '─'^(length(s)-1))
            printlnstyled(io, s; color)
        end
    end |> postprocess |> (x->print(io::IO,x))

    return n
end

# don't show stacktrace for syntax errors
print_report(io, report::SyntaxErrorReport) = showerror(io, report.err)
function print_report(io, report::RecursiveIncludeErrorReport)
    printstyled(io, "ERROR: "; bold = true, color = ERROR_COLOR)
    println(io, "recursive `include` call detected:")
    println(io, " ⚈ duplicated file: ", report.duplicated_file)
    println(io, " ⚈  included files: ", join(report.files, ' '))
end
# TODO: add context information, i.e. during macroexpansion, defining something
print_report(io, report::ActualErrorWrapped) = showerror(io, report.err, report.st)
function print_report(io, report::MissingConcretization)
    printstyled(io, "HINT: "; bold = true, color = HINT_COLOR)
    printlnstyled(io, """
    the following error happened mostly because of the missing concretization of global variables,
    and this could be fixed with the `concretization_patterns` configuration.
    Check https://aviatesk.github.io/JET.jl/dev/config/#JET.ToplevelConfig for the details.
    ---"""; color = HINT_COLOR)

    showerror(io, report.err, report.st)
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
    should_print && print_frame(io, frame, config, depth)
    print_stack(io, report, config, wrote_linfos, depth + 1)
end

function print_frame(io, frame, config, depth, color = RAIL_COLORS[(depth)%N_RAILS+1])
    print_rails(io, depth-1)

    s = string("┌ @ ", (config.fullpath ? tofullpath : identity)(string(frame.file)), ':', frame.line)
    printstyled(io, s, ' '; color)
    print_signature(io, frame.sig, config)
    println(io)

    return length(s) # the length of frame info string
end

function print_error_frame(io, report, config, depth)
    frame = report.vst[depth]

    color = report_color(report)
    len = print_frame(io, frame, config, depth, color)
    print_rails(io, depth-1)
    printstyled(io, "│ "; color)
    print_report(io, report)
    println(io)

    print_rails(io, depth-1)
    printlnstyled(io, '└', '─'^len; color)
end

function print_report(io::IO, report::InferenceErrorReport)
    color = report_color(report)
    msg = with_bufferring() do io
        print_report_message(io, report)
    end
    printstyled(io, msg; color)
    if print_signature(report)
        printstyled(io, ": "; color)
        print_signature(io, report.sig, (; annotate_types=true); bold=true)
    end
end

function print_signature(io, sig::Signature, config; kwargs...)
    for a in sig
        _print_signature(io, a, config; kwargs...)
    end
end
function _print_signature(io, @nospecialize(x), config; kwargs...)
    if isa(x, Type)
        if config.annotate_types
            printstyled(io, "::", x; color = TYPE_ANNOTATION_COLOR, kwargs...)
        end
    elseif isa(x, Repr)
        printstyled(io, sprint(show, x.val); kwargs...)
    elseif isa(x, AnnotationMaker)
        if config.annotate_types
            printstyled(io, x.switch ? '(' : ')'; kwargs...)
        end
    elseif isa(x, ApplyTypeResult)
        printstyled(io, x.typ; kwargs...)
    elseif isa(x, QuoteNode)
        printstyled(io, "[quote]"; kwargs...)
    elseif isa(x, MethodInstance)
        printstyled(io, sprint(show_mi, x); kwargs...)
    elseif isa(x, GlobalRef) && (x.mod === Main || isexported(x.mod, x.name))
        printstyled(io, x.name; kwargs...)
    else
        printstyled(io, x; kwargs...)
    end
end

function isexported(mod::Module, name::Symbol)
    Base.isexported(mod, name) && return true
    isdefined(mod, name) || return false
    v = (@static @isdefined(getglobal) ? getglobal : getfield)(mod, name)
    return Base.isexported(parentmodule(v), name)
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

# adapted from https://github.com/JuliaLang/julia/blob/0f11a7bb07d2d0d8413da05dadd47441705bf0dd/base/show.jl#L989-L1011
function show_mi(io::IO, l::MethodInstance)
    def = l.def
    if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            # print(io, "MethodInstance generator for ")
            show(io, def)
        else
            # print(io, "MethodInstance for ")
            show_tuple_as_call(io, def.name, l.specTypes)
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

@inline function show_tuple_as_call(io::IO, name::Symbol, @nospecialize(sig::Type))
    @static if hasmethod(Base.show_tuple_as_call, (IO, Symbol, Type), (:demangle, :kwargs, :argnames, :qualified))
        Base.show_tuple_as_call(io, name, sig; qualified = true)
    else
        Base.show_tuple_as_call(io, name, sig, false, nothing, nothing, true)
    end
end
