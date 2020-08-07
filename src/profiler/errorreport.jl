# error reports
# -------------

"""
    abstract type ErrorReport end

Abstract type of all the error reports that TypeProfiler.jl states.
All `ErrorReport`s are required to:
- have `acs::Vector{MethodInstance}` field -- keeps abstract call stack of this error (from call site to error location)
- provide `report_string` method -- converts this `ErrorReport` into readable text

!!! note
    All concrete `ErrorReport` types are supposed to be declared with `@reportdef` macro.
"""
abstract type ErrorReport end

const AbstractCallStack = Vector{MethodInstance}

macro reportdef(structex)
    @assert isexpr(structex, :struct, 3) "struct expression should be given"
    typedecl, body = structex.args[2:3]
    @assert isexpr(typedecl, :<:, 2) && __module__.eval(last(typedecl.args)) <: ErrorReport "error report should be declared as subtype of ErrorReport"
    T = first(typedecl.args)

    flds = filter(x->!isa(x, LineNumberNode), body.args)
    @assert first(flds) == :(acs::AbstractCallStack) "the first field of error report should be `acs::AbstractCallStack`"

    sigs = map(flds[2:end]) do x
        isexpr(x, :(::)) ? first(x.args) : x
    end
    constructor = :(
        function $(T)(sv::InferenceState, $(sigs...))
            @nospecialize $(sigs...)
            acs = trace_abstract_call_stack!(sv)
            return new(acs, $(sigs...))
        end
    )
    push!(body.args, constructor)

    return structex
end

# traces the current abstract call stack
function trace_abstract_call_stack!(sv, acs = MethodInstance[])
    isnothing(sv.parent) || trace_abstract_call_stack!(sv.parent, acs) # prewalk
    push!(acs, sv.linfo)
    return acs
end

@reportdef struct NoMethodErrorReport <: ErrorReport
    acs::AbstractCallStack
    tt::Type
    unionsplit::Bool
end

@reportdef struct InvalidBuiltinCallErrorReport <: ErrorReport
    acs::AbstractCallStack
    tt::Type
end

@reportdef struct UndefVarErrorReport <: ErrorReport
    acs::AbstractCallStack
    mod::Module
    name::Symbol
end

@reportdef struct NonBooleanCondErrorReport <: ErrorReport
    acs::AbstractCallStack
    t::Type
end

"""
    NativeRemark <: ErrorReport

This special `ErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other "real" `ErrorReport`s.
"""
@reportdef struct NativeRemark <: ErrorReport
    acs::AbstractCallStack
    s::String
end

# report string
# -------------

report_string(er::ErrorReport) =
    error("report_string(::$(typeof(er))) should be implemented")
report_string(er::NoMethodErrorReport) = er.unionsplit ?
    "for one of the union split cases, no matching method found for signature: $(tt_to_signature_str(er.tt))" :
    "no matching method found for signature: $(tt_to_signature_str(er.tt))"
# report_string(er::InvalidBuiltinCallErrorReport) =
#     "invalid builtin function call: $(tt_to_signature_str(er.tt))"
report_string(er::UndefVarErrorReport) =
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
