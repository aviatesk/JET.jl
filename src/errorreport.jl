# error reports
# -------------

abstract type ErrorReport end

const AbstractCallStack = Vector{MethodInstance}

"""
    abstract type ErrorReport end
    const AbstractCallStack = Vector{MethodInstance}

Abstract type of all the error reports that TypeProfiler.jl states.
All `ErrorReport`s are required to:
- have `acs::AbstractCallStack` field -- keeps abstract call stack of this error
  (from call site to error location), which can be collected by "tracking up"
  the `InferenceState` chain.
- provide `report_string` method -- converts this `ErrorReport` into readable text.

!!! note
    All concrete `ErrorReport` are supposed to be declared via [`@reportdef`](@ref) macro.
"""
ErrorReport, AbstractCallStack

"""
    @reportdef struct SomeErrorReport <: ErrorReport
        acs::AbstractCallStack
        ...
    end

Asserts a declaration of concrete `ErrorReport` structs, and adds its inner constructor that
  accepts `sv::InferenceState` as a first argument and collect `AbstractCallStack` from it.
"""
macro reportdef(structex)
    @assert isexpr(structex, :struct, 3) "struct expression should be given"
    typedecl, body = structex.args[2:3]
    @assert isexpr(typedecl, :<:, 2) && __module__.eval(last(typedecl.args)) <: ErrorReport "error report should be declared as subtype of ErrorReport"
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
