# toplevel
# --------

abstract type ToplevelErrorReport end

struct SyntaxErrorReport <: ToplevelErrorReport
    err::ErrorException
    file::String
    line::Int
    SyntaxErrorReport(msg::AbstractString, file, line) = new(ErrorException(msg), file, line)
end

# wraps general errors from actual Julia process
struct ActualErrorWrapped <: ToplevelErrorReport
    err
    bt::Vector{Union{Ptr{Nothing},Base.InterpreterIP}}
    file::String
    line::Int

    # default constructor
    ActualErrorWrapped(err, bt, file, line) = new(err, bt, file, line)

    # bypass syntax error
    function ActualErrorWrapped(err::ErrorException, bt, file, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, file, line)
        else
            new(err, bt, file, line)
        end
    end
end

# inference
# ---------

abstract type InferenceErrorReport end

const AbstractCallStack = Vector{MethodInstance}

"""
    @reportdef struct SomeInferenceErrorReport <: InferenceErrorReport
        acs::AbstractCallStack
        ...
    end

Asserts a declaration of concrete `InferenceErrorReport` structs, and adds its inner constructor
  that accepts `sv::InferenceState` as a first argument and collect `AbstractCallStack` from it.
"""
macro reportdef(structex)
    @assert isexpr(structex, :struct, 3) "struct expression should be given"
    typedecl, body = structex.args[2:3]
    @assert isexpr(typedecl, :<:, 2) && __module__.eval(last(typedecl.args)) <: InferenceErrorReport "error report should be declared as subtype of InferenceErrorReport"
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
Ideally all of them should be covered by the other `InferenceErrorReport`s.
"""
@reportdef struct NativeRemark <: InferenceErrorReport
    acs::AbstractCallStack
    s::String
end
