# error reports
# -------------

@nospecialize

"""
    abstract type ErrorReport end

Abstract type of all the error reports that TypeProfiler.jl states.
All `ErrorReport`s are required to:
- have `linfo::MethodInstance` field -- keeps its location
- provides `report_string` method -- converts this `ErrorReport` into readable text
"""
abstract type ErrorReport end

struct NoMethodErrorReport <: ErrorReport
    linfo::MethodInstance
    tt::Type
    unionsplit::Bool
end

struct InvalidBuiltinCallErrorReport <: ErrorReport
    linfo::MethodInstance
    tt::Type
end

struct UndefVarErrorReport <: ErrorReport
    linfo::MethodInstance
    mod::Module
    name::Symbol
end

struct NonBooleanCondErrorReport <: ErrorReport
    linfo::MethodInstance
    t::Type
end

"""
    NativeRemark <: ErrorReport

This special `ErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other "real" `ErrorReport`s.
"""
struct NativeRemark <: ErrorReport
    linfo::MethodInstance
    s::String
end

@specialize

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
