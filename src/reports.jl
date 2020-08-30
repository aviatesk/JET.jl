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
const ViewedVirtualStackTrace = typeof(view(VirtualStackTrace(), :))

"""
    const VirtualFrame = NamedTuple{(:file,:line,:sig),Tuple{Symbol,Int,String}}
    const VirtualStackTrace = Vector{VirtualFrame}
    const ViewedVirtualStackTrace = typeof(view(VirtualStackTrace(), :))

- `VirtualStackTrace` represents virtual back trace of profiled errors (supposed to be
  ordered from call site to error point
- `ViewedVirtualStackTrace` is view of `VirtualStackTrace` and will be kept in [`TPCACHE`](@ref)
"""
VirtualFrame, VirtualStackTrace, ViewedVirtualStackTrace

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

macro reportdef(ex)
    T = first(ex.args)
    args = map(ex.args) do x
        # unwrap @nospecialize
        isexpr(x, :macrocall) && first(x.args) === Symbol("@nospecialize") && (x = last(x.args))
        # handle default arguments
        isexpr(x, :(=)) && return first(x.args)
        return x
    end

    constructor_body = quote
        msg = get_msg(#= T, interp, sv, ... =# $(args...))
        sig = get_sig(#= sv =# $(args[3]))

        cache_report! = let msg = msg, sig = sig, interp = #= interp =# $(args[2])
            function (sv, st)
                # key = hash(sv.linfo)
                if haskey(TPCACHE, sv.linfo)
                    _, cached_reports = TPCACHE[sv.linfo]
                else
                    id = get_id(interp)
                    cached_reports = InferenceReportCache[]
                    TPCACHE[sv.linfo] = id => cached_reports
                end

                push!(cached_reports, InferenceReportCache{$(T)}(view(st, :), msg, sig))
            end
        end

        st = track_abstract_call_stack!(cache_report!, #= sv =# $(args[3]))

        return new(reverse(st), msg, sig)
    end
    constructor_ex = Expr(:function, ex, constructor_body)

    return quote
        struct $(T) <: InferenceErrorReport
            st::VirtualStackTrace
            msg::String
            sig::String

            # inner constructor (from abstract interpretation)
            $(constructor_ex)

            # inner constructor (from cache)
            function $(T)(st::VirtualStackTrace, msg::AbstractString, sig::AbstractString)
                new(reverse(st), msg, sig)
            end
        end
    end
end

@reportdef NoMethodErrorReport(interp, sv, unionsplit)

@reportdef InvalidBuiltinCallErrorReport(interp, sv)

@reportdef UndefVarErrorReport(interp, sv, mod, name)

@reportdef NonBooleanCondErrorReport(interp, sv, @nospecialize(t))

"""
    NativeRemark <: InferenceErrorReport

This special `InferenceErrorReport` is just for wrapping remarks from `NativeInterpreter`.
Ideally all of them should be covered by the other `InferenceErrorReport`s.
"""
@reportdef NativeRemark(interp, sv, s)

# traces the current abstract call stack
function track_abstract_call_stack!(f::Function, sv, st = VirtualFrame[])
    sig = get_sig(sv)
    file, line = get_file_line(sv)
    frame = (; file, line, sig)

    push!(st, frame)
    f(sv, st)

    isroot(sv) || track_abstract_call_stack!(f, sv.parent, st) # postwalk

    return st
end

function get_file_line(frame::InferenceState)
    linfo = get_cur_linfo(frame)
    return linfo.file, linfo.line
end

get_sig(sv::InferenceState) = return get_sig(sv, get_cur_stmt(sv))

# # adapted from https://github.com/JuliaLang/julia/blob/58febaaf2fe38d90d41c170bc2f416a76eac46f5/base/show.jl#L945-L958
# function get_sig(linfo::MethodInstance)
#     io = IOBuffer()
#     def = linfo.def
#     if isa(def, Method)
#         if isdefined(def, :generator) && linfo === def.generator
#             show(io, def)
#         else
#             Base.show_tuple_as_call(io, def.name, linfo.specTypes)
#         end
#     else
#         print(io, "Toplevel MethodInstance thunk")
#     end
#     return String(take!(io))
# end

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
    typ     = string(widenconst(sv.src.ssavaluetypes[ssa.id]))
    return endswith(ssa_sig, typ) ? ssa_sig : string(ssa_sig, "::", typ)
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

get_msg(::Type{NoMethodErrorReport}, interp, sv, unionsplit) = unionsplit ?
    "for one of the union split cases, no matching method found for signature" :
    "no matching method found for call signature"
get_msg(::Type{InvalidBuiltinCallErrorReport}, interp, sv) =
    "invalid builtin function call"
get_msg(::Type{UndefVarErrorReport}, interp, sv, mod, name) = isnothing(mod) ?
    "variable $(name) is not defined" :
    "variable $(mod).$(name) is not defined"
get_msg(::Type{NonBooleanCondErrorReport}, interp, sv, @nospecialize(t)) =
    "non-boolean ($(t)) used in boolean context"
get_msg(::Type{NativeRemark}, interp, sv, s) = s

# utils
# -----

# NOTE: these methods assume `frame` is not inlined

get_cur_pc(frame::InferenceState) = return frame.currpc
get_cur_stmt(frame::InferenceState) = frame.src.code[get_cur_pc(frame)]
get_cur_loc(frame::InferenceState) = frame.src.codelocs[get_cur_pc(frame)]
get_cur_linfo(frame::InferenceState) = frame.src.linetable[get_cur_loc(frame)]
get_cur_varstates(frame::InferenceState) = frame.stmt_types[get_cur_pc(frame)]
get_result(frame::InferenceState) = frame.result.result
isroot(frame::InferenceState) = isnothing(frame.parent)
