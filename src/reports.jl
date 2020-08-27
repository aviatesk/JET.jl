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
    st::Vector{Base.StackTraces.StackFrame}
    file::String
    line::Int

    # default constructor
    ActualErrorWrapped(err, st, file, line) = new(err, st, file, line)

    # bypass syntax error
    function ActualErrorWrapped(err::ErrorException, st, file, line)
        return if startswith(err.msg, "syntax: ")
            SyntaxErrorReport(err.msg, file, line)
        else
            new(err, st, file, line)
        end
    end
end

# inference
# ---------

# TODO: maybe we want to use https://github.com/aviatesk/Mixin.jl
abstract type InferenceErrorReport end

const VirtualFrame = @NamedTuple begin
    file::Symbol
    line::Int
    sig::String
end
const VirtualStackTrace = Vector{VirtualFrame}

# helps inference
function Base.getproperty(er::InferenceErrorReport, sym::Symbol)
    return if sym === :st
        getfield(er, sym)::VirtualStackTrace
    elseif sym === :msg
        getfield(er, sym)::String
    elseif sym === :sig
        getfield(er, sym)::String
    else
        getfield(er, sym) # fallback
    end
end

# macro reportdef(structex)
#     @assert isexpr(structex, :struct, 3) "struct expression should be given"
#     typedecl, body = structex.args[2:3]
#     @assert isexpr(typedecl, :<:, 2) && __module__.eval(last(typedecl.args)) <: InferenceErrorReport "error report should be declared as subtype of InferenceErrorReport"
#     T = first(typedecl.args)
#
#     flds = filter(x->!isa(x, LineNumberNode), body.args)
#     @assert first(flds) == :(st::VirtualStackTrace) "the first field of error report should be `st::VirtualStackTrace`"
#
#     args = flds[2:end]
#     sigsyms = _get_sigsym.(args)
#     nospecialize_sigs = sigsyms[findall(_should_not_specialize, args)]
#     nospecialize_ex = isempty(nospecialize_sigs) ? quote end : :(@nospecialize $(nospecialize_sigs...))
#     return quote
#         struct $(T) <: InferenceErrorReport
#             st::VirtualStackTrace
#             msg::String
#             sig::String
#
#             # we give up hygiene here because `@nospecialize` only works on escaped signatures
#             function $(T)(sv::InferenceState, $(map(esc, sigsyms)...))
#                 $(nospecialize_ex)
#                 st = track_abstract_call_stack!(sv)
#                 msg = get_msg($(T), sv, $(map(esc, sigsyms)...))
#                 sig = get_sig(sv)
#                 return new(st, msg, sig)
#             end
#         end
#     end
# end
#
# _get_sigsym(x) = isexpr(x, :(::)) ? first(x.args) : x
# _should_not_specialize(x) = isexpr(x, :(::)) && last(x.args) in (:Type, :Function)

struct NoMethodErrorReport <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::String

    function NoMethodErrorReport(sv::InferenceState, unionsplit)
        st = track_abstract_call_stack!(sv)
        msg = get_msg(NoMethodErrorReport, sv, unionsplit)
        sig = get_sig(sv)
        return new(st, msg, sig)
    end
end

struct InvalidBuiltinCallErrorReport <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::String

    function InvalidBuiltinCallErrorReport(sv::InferenceState)
        st = track_abstract_call_stack!(sv)
        msg = get_msg(InvalidBuiltinCallErrorReport, sv)
        sig = get_sig(sv)
        return new(st, msg, sig)
    end
end

struct UndefVarErrorReport <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::String

    function UndefVarErrorReport(sv::InferenceState, mod, name)
        st = track_abstract_call_stack!(sv)
        msg = get_msg(UndefVarErrorReport, sv, mod, name)
        sig = get_sig(sv)
        return new(st, msg, sig)
    end
end

struct NonBooleanCondErrorReport <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::String

    function NonBooleanCondErrorReport(sv::InferenceState, @nospecialize(t))
        st = track_abstract_call_stack!(sv)
        msg = get_msg(NonBooleanCondErrorReport, sv, t)
        sig = get_sig(sv)
        return new(st, msg, sig)
    end
end

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other `InferenceErrorReport`s.
"""
struct NativeRemark <: InferenceErrorReport
    st::VirtualStackTrace
    msg::String
    sig::String

    function NativeRemark(sv::InferenceState, s)
        st = track_abstract_call_stack!(sv)
        msg = get_msg(NativeRemark, sv, s)
        sig = get_sig(sv)
        return new(st, msg, sig)
    end
end

# traces the current abstract call stack
function track_abstract_call_stack!(sv, st = VirtualFrame[])::VirtualStackTrace
    istoplevelframe(sv) || track_abstract_call_stack!(sv.parent, st) # prewalk
    sig = get_sig(sv)
    file, line = get_file_line(sv)
    push!(st, (; file, line, sig))
    return st
end

function get_file_line(frame::InferenceState)
    loc = frame.src.codelocs[get_cur_pc(frame)]
    linfo = frame.src.linetable[loc]
    return linfo.file, linfo.line
end

# for the top frame
# adapted from https://github.com/JuliaLang/julia/blob/58febaaf2fe38d90d41c170bc2f416a76eac46f5/base/show.jl#L945-L958
function get_sig(linfo::MethodInstance)
    io = IOBuffer()
    def = linfo.def
    if isa(def, Method)
        if isdefined(def, :generator) && linfo === def.generator
            show(io, def)
        else
            Base.show_tuple_as_call(io, def.name, linfo.specTypes)
        end
    else
        print(io, "Toplevel MethodInstance thunk")
    end
    return String(take!(io))
end

# FIXME: obviously these implementations are not exhaustive
get_sig(sv::InferenceState) = get_sig(sv, get_cur_stmt(sv))
function get_sig(sv::InferenceState, expr::Expr)
    head = expr.head
    return if head === :call
        f = get_sig(sv, first(expr.args))
        sig = join(get_sig.(Ref(sv), expr.args[2:end]), ", ")
        string(f, '(', sig, ')')
    elseif head === :(=)
        # lhs = get_sig(sv, first(expr.args), false)
        rhs = get_sig(sv, last(expr.args))
        string(rhs)
    elseif head === :static_parameter
        string("::", widenconst(sv.sptypes[first(expr.args)]))
    else
        string(expr)
    end
end
function get_sig(sv::InferenceState, ssa::SSAValue)
    ssa_sig = get_sig(sv, sv.src.code[ssa.id])
    typ     = widenconst(sv.src.ssavaluetypes[ssa.id])
    return string(ssa_sig, "::", typ)
end
function get_sig(sv::InferenceState, slot::SlotNumber)
    slot_sig = string(sv.src.slotnames[slot.id])
    isempty(slot_sig) && (slot_sig = string(slot)) # fallback if no explicit slotname
    typ = widenconst(get_cur_varstates(sv)[slot.id].typ)
    return string(slot_sig, "::", typ)
end
get_sig(::InferenceState, gr::GlobalRef) = string(gr.mod, '.', gr.name)
get_sig(sv::InferenceState, gotoifnot::GotoIfNot) = string("goto %", gotoifnot.dest, " if not ", get_sig(sv, gotoifnot.cond))
get_sig(sv::InferenceState, ret::ReturnNode) = string("return ", get_sig(sv, ret.val))
get_sig(::InferenceState, qn::QuoteNode) = string(qn, "::", typeof(qn.value))
get_sig(::InferenceState, @nospecialize(x)) = repr(x; context = :compact => true)

get_msg(::Type{NoMethodErrorReport}, sv, unionsplit) = unionsplit ?
    "for one of the union split cases, no matching method found for signature" :
    "no matching method found for call signature"
get_msg(::Type{InvalidBuiltinCallErrorReport}, sv) =
    "invalid builtin function call"
get_msg(::Type{UndefVarErrorReport}, sv, mod, name) = isnothing(mod) ?
    "variable $(name) is not defined" :
    "variable $(mod).$(name) is not defined"
get_msg(::Type{NonBooleanCondErrorReport}, sv, @nospecialize(t)) =
    "non-boolean ($(t)) used in boolean context"
get_msg(::Type{NativeRemark}, sv, s) = s
