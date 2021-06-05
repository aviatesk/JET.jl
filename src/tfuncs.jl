# XXX tfunc implementations in Core.Compiler are really not enough to catch invalid calls
# TODO set up our own checks and enable sound analysis

function CC.builtin_tfunction(analyzer::AbstractAnalyzer, @nospecialize(f), argtypes::Array{Any,1},
                              sv::InferenceState) # `AbstractAnalyzer` isn't overloaded on `return_type`
    ret = @invoke builtin_tfunction(analyzer::AbstractInterpreter, f, argtypes::Array{Any,1},
                                    sv::Union{InferenceState,Nothing})

    if f === throw
        # here we only report a selection of "serious" exceptions, i.e. those that should be
        # reported even if they may be caught in actual execution;
        report_pass!(SeriousExceptionReport, analyzer, sv, argtypes)

        # other general `throw` calls will be reported within `_typeinf(analyzer::AbstractAnalyzer, frame::InferenceState)`
        # only when they are not caught by control flow, which is judged by whether if the
        # final return type of `sv` is annotated as `Bottom` or not, thus early return now
        return ret
    end

    # report pass for invalid builtin function calls
    # XXX for the meanwhile, we rely on the implementation of native tfuncs thus pass `ret` here as well
    # XXX dynamic dispatch, is this okay in terms of performance ?
    report_pass!(InvalidBuiltinCallErrorReport, analyzer, sv, f, argtypes, ret)

    if f === getfield && 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const)
                    mod = obj.val
                    if isa(mod, Module)
                        if isdefined(mod, name)
                            if istoplevel_globalref(analyzer, sv)
                                # when accessing to a global variable in a module
                                # concretized by `analyzer`, eagerly propagate its type
                                # NOTE logic here should be synced with that of `abstract_eval_special_value(::AbstractAnalyzer, ::Any, ::VarTable, ::InferenceState)`
                                val = getfield(mod, name)
                                return isa(val, AbstractGlobal) ? val.t : Const(val)
                            end
                        end
                    end
                end
            end
        end
    elseif f === fieldtype
        # the valid widest possible return type of `fieldtype_tfunc` is `Union{Type,TypeVar}`
        # because fields of unwrapped `DataType`s can legally be `TypeVar`s,
        # but this will cause lots of false positive `NoMethodErrorReport`s for inference
        # with accessing to abstract fields since most methods don't expect `TypeVar`
        # (e.g. `@report_call readuntil(stdin, 'c')`)
        # JET.jl further widens this case to `Any` and give up further analysis rather than
        # trying hard to do sound and noisy analysis
        # xref: https://github.com/JuliaLang/julia/pull/38148
        if ret === Union{Type, TypeVar}
            return Any
        end
    end

    return ret
end

"""
    SeriousExceptionReport <: InferenceErrorReport

The abstract type for "serious" errors that are invoked by `throw` calls but should be
reported even if they may be caught in actual execution.
In order to avoid duplicated reports for the `throw` call, any subtype of `SeriousExceptionReport`
should keep `lin::LineInfoNode` field, which represents where the report gets collected.
"""
abstract type SeriousExceptionReport <: InferenceErrorReport end

# # NOTE: this mixin implementation is cleaner but doesn't help inference,
# # because inference on the `getproperty` interface relies on constant prop' and currently
# # constant prop' isn't supported for `invoke`d calls
# function Base.getproperty(er::SeriousExceptionReport, sym::Symbol)
#     sym === :lin && return getfield(er, :lin)::LineInfoNode
#     return @invoke getproperty(er::InferenceErrorReport, sym::Symbol)
# end

@reportdef struct UndefKeywordErrorReport <: SeriousExceptionReport
    err::UndefKeywordError
    lin::LineInfoNode
end
get_msg(::Type{UndefKeywordErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, err::UndefKeywordError, lin::LineInfoNode) = sprint(showerror, err)

function (::SoundBasicPass)(::Type{SeriousExceptionReport}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    if length(argtypes) ≥ 1
        a = first(argtypes)
        if isa(a, Const)
            v = a.val
            if isa(v, UndefKeywordError)
                report!(UndefKeywordErrorReport, analyzer, sv, v, get_lin((sv, get_currpc(sv))))
            end
        end
    end
end

"""
    InvalidBuiltinCallErrorReport

Represents errors caused by invalid builtin-function calls.
Technically they're defined as those error points that should be caught within
`Core.Compiler.builtin_tfunction`.
"""
abstract type InvalidBuiltinCallErrorReport <: InferenceErrorReport end

@reportdef struct NoFieldErrorReport <: InvalidBuiltinCallErrorReport
    @nospecialize(typ::Type)
    name::Symbol
end
get_msg(::Type{NoFieldErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(typ::Type), name::Symbol) =
    "type $(typ) has no field $(name)"

@reportdef struct DivideErrorReport <: InferenceErrorReport end
let s = sprint(showerror, DivideError())
    global get_msg(::Type{DivideErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState) = s
end

@reportdef struct UnimplementedBuiltinCallErrorReport <: InvalidBuiltinCallErrorReport
    argtypes::Vector{Any}
end
get_msg(::Type{UnimplementedBuiltinCallErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(args...)) =
    "invalid builtin function call"

# TODO we do need sound versions of these functions

# XXX: for general case, JET just relies on the (maybe too persmissive) return type
# from native tfuncs to report invalid builtin calls and probably there're lots of
# false negatives
function handle_unimplemented_builtins!(analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(ret))
    if ret === Bottom
        @assert !(f === throw) "`throw` calls shuold be handled by report passes of `SeriousExceptionReport` or `UncaughtExceptionReport`"

        report!(UnimplementedBuiltinCallErrorReport, analyzer, sv, argtypes)
    end
end

# fallback for unimplemented tfuncs, eliminate me in the future
function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(f::Builtin), argtypes::Vector{Any}, @nospecialize(ret))
    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, f::typeof(getfield), argtypes::Vector{Any}, @nospecialize(ret))
    if 2 ≤ length(argtypes) ≤ 3
        obj, fld = argtypes
        if isa(fld, Const)
            name = fld.val
            if isa(name, Symbol)
                if isa(obj, Const) && (mod = obj.val; isa(mod, Module))
                    if !isdefined(mod, name)
                        # bypass for report pass for undefined global reference
                        report_pass!(GlobalUndefVarErrorReport, analyzer, sv, mod, name)
                        return
                    end
                elseif ret === Bottom
                    # general case when an error is detected by the native `getfield_tfunc`
                    typ = widenconst(obj)
                    report!(NoFieldErrorReport, analyzer, sv, typ, name)
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end

function (::SoundBasicPass)(::Type{InvalidBuiltinCallErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, f::IntrinsicFunction, argtypes::Vector{Any}, @nospecialize(ret))
    # TODO this check might be better in its own report pass, say `NumericalPass`
    if length(argtypes) == 2
        if f === Intrinsics.checked_sdiv_int ||
           f === Intrinsics.checked_srem_int ||
           f === Intrinsics.checked_udiv_int ||
           f === Intrinsics.checked_urem_int ||
           f === Intrinsics.sdiv_int ||
           f === Intrinsics.srem_int ||
           f === Intrinsics.udiv_int ||
           f === Intrinsics.urem_int
            a = argtypes[2]
            t = widenconst(a)
            if isprimitivetype(t) && t <: Number
                if isa(a, Const) && a.val === zero(t)
                    report!(DivideErrorReport, analyzer, sv)
                    return
                end
            end
        end
    end

    handle_unimplemented_builtins!(analyzer, sv, f, argtypes, ret)
end

# check if this frame is for `getproperty(::Module, ::Symbol)`, which accesses to a global
# variable traced by `analyzer`
function istoplevel_globalref(analyzer::AbstractAnalyzer, sv::InferenceState)
    def = sv.linfo.def
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = sv.parent
    return !isnothing(parent) && istoplevel(analyzer, parent)
end

# `return_type_tfunc` internally uses `abstract_call` to model `Core.Compiler.return_type`
# and here we shouldn't pass `AbstractAnalyzer` to it; otherwise we may get false error reports
# from the `abstract_call`, which will simulate the call and isn't any abstraction of actual
# execution of it
function CC.return_type_tfunc(analyzer::AbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    # report pass for invalid `Core.Compiler.return_type` call
    report_pass!(InvalidReturnTypeCall, analyzer, sv, argtypes)

    # don't recursively pass on `AbstractAnalyzer` via `@invoke` here, and make sure
    # JET's analysis enter into the simulated call
    return return_type_tfunc(get_native(analyzer), argtypes, sv)
end

@reportdef struct InvalidReturnTypeCall <: InferenceErrorReport end
get_msg(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState) = "invalid `Core.Compiler.return_type` call"

function (::SoundBasicPass)(::Type{InvalidReturnTypeCall}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    # here we just check if the call of `return_type` is valid or not by very simple analysis
    # we don't take (possible, but very unexpected) overloads into account here, just as
    # `NativeInterpreter`'s `return_type_tfunc` hard-codes its return type to `Type`
    if length(argtypes) ≠ 3
        # invalid argument number, let's report and return error result (i.e. `Bottom`)
        report!(InvalidReturnTypeCall, analyzer, sv)
    end
end
