# types
# -----

abstract type InferenceErrorReport <: ErrorReport end

const AbstractCallStack = Vector{MethodInstance}

"""
    @reportdef struct SomeErrorReport <: InferenceErrorReport
        acs::AbstractCallStack
        ...
    end

Asserts a declaration of concrete `InferenceErrorReport` structs, and adds its inner constructor
  that accepts `sv::InferenceState` as a first argument and collect `AbstractCallStack` from it.
"""
macro reportdef(structex)
    @assert isexpr(structex, :struct, 3) "struct expression should be given"
    typedecl, body = structex.args[2:3]
    @assert isexpr(typedecl, :<:, 2) && __module__.eval(last(typedecl.args)) <: InferenceErrorReport "error report should be declared as subtype of ErrorReport"
    T = first(typedecl.args)

    flds = filter(x->!isa(x, LineNumberNode), body.args)
    @assert first(flds) == :(acs::AbstractCallStack) "the first field of error report should be `acs::AbstractCallStack`"

    args = flds[2:end]
    sigs = _get_sig.(args)
    nospecialize_sigs = sigs[findall(_should_not_specialize, args)]
    nospecialize_ex = isempty(nospecialize_sigs) ? quote end : :(@nospecialize $(nospecialize_sigs...))
    constructor = :(
        # we give up hygiene here because `@nospecialize` only works on escaped signatures
        function $(T)(sv::InferenceState, $(map(esc, sigs)...))
            $(nospecialize_ex)
            acs = _track_abstract_call_stack!(sv)
            return new(acs, $(map(esc, sigs)...))
        end
    )
    push!(body.args, constructor)

    return structex
end

_get_sig(x) = isexpr(x, :(::)) ? first(x.args) : x
_should_not_specialize(x) = isexpr(x, :(::)) && last(x.args) in (:Type, :Function)

# traces the current abstract call stack
function _track_abstract_call_stack!(sv, acs = MethodInstance[])
    isnothing(sv.parent) || _track_abstract_call_stack!(sv.parent, acs) # prewalk
    push!(acs, sv.linfo)
    return acs
end

@reportdef struct NoMethodErrorReport <: InferenceErrorReport
    acs::AbstractCallStack
    tt::Type
    unionsplit::Bool
end

@reportdef struct InvalidBuiltinCallErrorReport <: InferenceErrorReport
    acs::AbstractCallStack
    tt::Type
end

@reportdef struct UndefVarErrorReport <: InferenceErrorReport
    acs::AbstractCallStack
    mod::Union{Nothing,Module}
    name::Symbol
end

@reportdef struct NonBooleanCondErrorReport <: InferenceErrorReport
    acs::AbstractCallStack
    t::Type
end

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other "real" `ErrorReport`s.
"""
@reportdef struct NativeRemark <: InferenceErrorReport
    acs::AbstractCallStack
    s::String
end

# print
# -----

function print_report(io, report::InferenceErrorReport, wrote_linfos; kwargs...)
    print_calltrace(io, report.acs, wrote_linfos)
    n = length(report.acs) - 1
    print_rails(io, n)
    printstyled(io, "│ ", report_string(report), '\n'; color = ERROR_COLOR)
    print_rails(io, n)
    printstyled(io, '└', '\n'; color = ERROR_COLOR)

    return
end

# traverse abstract call stack, collect locations
# - TODO: maybe we want to show callsites instead of method sigs in a calltrace
# - TODO: method sigs or callsites can be type annotated with profiled types

function print_calltrace(io, linfos, wrote_linfos, depth = 0)
    i = depth + 1
    linfo = linfos[i]

    linfo_hash = hash(linfo)
    should_print = linfo_hash ∉ wrote_linfos
    push!(wrote_linfos, linfo_hash)

    if length(linfos) == i # error here
        print_location(io, linfo, depth, true)
        return
    end

    # print current frame adn go into deeper
    should_print && print_location(io, linfo, depth, false)
    print_calltrace(io, linfos, wrote_linfos, depth + 1)
    return
end

function print_location(io, linfo, depth, is_err)
    file, line = get_file_line(linfo)

    # rail
    print_rails(io, depth)

    color = is_err ? ERROR_COLOR : RAIL_COLORS[(depth+1)%length(RAIL_COLORS)+1]
    printstyled(io, "┌ @ ", file, ":", line; color)

    # source
    path = fullpath(string(file))
    source_line = if isfile(path)
        strip(readlines(path)[line])
    else
        string("within `", linfo.def, ''') # when the file doesn't exist, e.g. defined in REPL
    end
    println(io, ' ', source_line)

    return
end

report_string(er::NoMethodErrorReport) = er.unionsplit ?
    "for one of the union split cases, no matching method found for signature: $(tt_to_signature_str(er.tt))" :
    "no matching method found for signature: $(tt_to_signature_str(er.tt))"
report_string(er::InvalidBuiltinCallErrorReport) =
    "invalid builtin function call: $(tt_to_signature_str(er.tt))"
report_string(er::UndefVarErrorReport) = isnothing(er.mod) ?
    "variable $(er.name) is not defined" :
    "variable $(er.mod).$(er.name) is not defined"
report_string(er::NonBooleanCondErrorReport) =
    "non-boolean ($(er.t)) used in boolean context"
report_string(r::NativeRemark) = r.s

# returns a call signature string from tt
function tt_to_signature_str(@nospecialize(tt::Type{<:Tuple}))
    fn = ft_to_fname(tt.parameters[1])
    args = join("::" .* string.(tt.parameters[2:end]), ", ")
    return string(fn, '(', args, ')')
end

# returns function name from its type
function ft_to_fname(@nospecialize(ft))
    return if isconstType(ft)
        ft.parameters[1]
    elseif ft isa DataType && isdefined(ft, :instance)
        ft.instance
    else
        ft
    end |> string
end

get_file_line(mi::MethodInstance) = get_file_line(mi.def)
get_file_line(m::Method) = (; m.file, m.line)
get_file_line(m::Module) = error("get_file_line(::Module) unimplemented")
