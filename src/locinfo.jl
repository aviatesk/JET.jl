# virtual frame
# =============

# get location information at the given program counter (or a current counter if not specified)
function get_virtual_frame(analyzer::AbstractAnalyzer, state::StateAtPC)
    sig = get_sig(analyzer, state)
    file, line = get_file_line(state)
    linfo = isa(state, MethodInstance) ? state : first(state).linfo
    return VirtualFrame(file, line, sig, linfo)
end
get_virtual_frame(analyzer::AbstractAnalyzer, sv::InferenceState) = get_virtual_frame(analyzer, (sv, get_currpc(sv)))
function get_virtual_frame(analyzer::AbstractAnalyzer, linfo::MethodInstance)
    sig = get_sig(analyzer, linfo)
    file, line = get_file_line(linfo)
    return VirtualFrame(file, line, sig, linfo)
end

get_file_line(s::StateAtPC)          = get_file_line(get_lin(s))
get_file_line(lin::LineInfoNode)     = lin.file, lin.line
get_file_line(linfo::MethodInstance) = begin
    def = linfo.def

    isa(def, Method) && return def.file, Int(def.line)

    # top-level
    src = linfo.uninferred::CodeInfo
    return get_file_line(first(src.linetable::Vector)::LineInfoNode)
end

# signature
# =========

# adapted from https://github.com/JuliaLang/julia/blob/0f11a7bb07d2d0d8413da05dadd47441705bf0dd/base/show.jl#L989-L1011
function get_sig(analyzer::AbstractAnalyzer, l::MethodInstance)
    def = l.def
    ret = if isa(def, Method)
        if isdefined(def, :generator) && l === def.generator
            # print(io, "MethodInstance generator for ")
            # show(io, def)
            sprint(show, def)
        else
            # print(io, "MethodInstance for ")
            sprint(show_tuple_as_call, def.name, l.specTypes)
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
        "toplevel"
    end
    return Any[ret]
end

@inline function show_tuple_as_call(io::IO, name::Symbol, @nospecialize(sig::Type))
    @static if hasmethod(Base.show_tuple_as_call, (IO, Symbol, Type), (:demangle, :kwargs, :argnames, :qualified))
        Base.show_tuple_as_call(io, name, sig; qualified = true)
    else
        Base.show_tuple_as_call(io, name, sig, false, nothing, nothing, true)
    end
end

@inline get_sig(analyzer::AbstractAnalyzer, s::StateAtPC) = return _get_sig(analyzer, s, get_stmt(s))

@inline _get_sig(analyzer::AbstractAnalyzer, s::StateAtPC, @nospecialize(x)) = return first(_get_sig_type(analyzer, s, x))::Vector{Any}

function _get_callsig(analyzer::AbstractAnalyzer, s::StateAtPC, @nospecialize(f), args::Vector{Any};
                      splat::Bool = false)
    sig = _get_sig(analyzer, s, f)
    push!(sig, '(')

    nargs = length(args)
    for (i, arg) in enumerate(args)
        arg_sig = _get_sig(analyzer, s, arg)
        append!(sig, arg_sig)
        if i ≠ nargs
            push!(sig, ", ")
        else
            splat && push!(sig, "...")
        end
    end
    push!(sig, ')')

    return sig
end

function _get_sig_type(analyzer::AbstractAnalyzer, s::StateAtPC, expr::Expr)
    head = expr.head
    if head === :call
        f = first(expr.args)
        args = expr.args[2:end]

        # special case splat call signature
        if isa(f, GlobalRef) && f.name === :_apply_iterate && begin
                itf = first(args)
                isa(itf, GlobalRef) && itf.name === :iterate
            end
            f = args[2]
            args = args[3:end]
            return _get_callsig(analyzer, s, f, args; splat = true), nothing
        else
            return _get_callsig(analyzer, s, f, args), nothing
        end
    elseif head === :invoke
        f = expr.args[2]
        args = expr.args[3:end]
        return _get_callsig(analyzer, s, f, args), nothing
    elseif head === :(=)
        return _get_sig_type(analyzer, s, last(expr.args))
    elseif head === :static_parameter
        typ = widenconst(first(s).sptypes[first(expr.args)])
        return Any['_', typ], typ
    else
        return Any[string(expr)], nothing
    end
end
function _get_sig_type(analyzer::AbstractAnalyzer, (sv, _)::StateAtPC, ssa::SSAValue)
    news = (sv, ssa.id)
    if isa(sv, OptimizationState)
        # when working on `OptimizationState`, the SSA traverse could be really long because
        # of inlining, so just give up for such a case
        typ = widenconst(ignorelimited(ignorenotfound(get_ssavaluetype(news))))
        sig = Any["%$(ssa.id)", typ]
    else
        # XXX the same problem _may_ happen for `InferenceState` too ?
        sig, sig_typ = _get_sig_type(analyzer, news, get_stmt(news))
        typ = widenconst(ignorelimited(ignorenotfound(get_ssavaluetype(news))))
        sig_typ == typ || push!(sig, typ) # XXX I forgot why I added this line ...
    end    
    return sig, typ
end
function _get_sig_type(analyzer::AbstractAnalyzer, s::StateAtPC, slot::SlotNumber)
    sv = first(s)
    name = get_slotname(sv, slot)
    sig = string(name)
    if isempty(sig)
        sig = string(slot) # fallback if no explicit slotname
    end
    if istoplevel(analyzer, sv)
        # this is a abstract global variable, form the global reference
        return _get_sig_type(analyzer, s, GlobalRef(get_toplevelmod(analyzer), name))
    else
        # we can use per-program counter type after inference
        t = (isa(sv, InferenceState) && sv.inferred) ? get_slottype(sv, slot) : get_slottype(s, slot)
        typ = widenconst(ignorelimited(t))
        return Any[sig, typ], typ
    end
end
# NOTE `Argument` is introduced by optimization, and so we don't need to handle abstract global variable here, etc.
function _get_sig_type(analyzer::AbstractAnalyzer, (sv, _)::StateAtPC, arg::Argument)
    name = get_slotname(sv, arg.n)
    sig = string(name)
    typ = widenconst(ignorelimited(get_slottype(sv, arg))) # after optimization we shouldn't use `get_slottype(::StateAtPC, ::Any)`
    return Any[sig, typ], typ
end
_get_sig_type(analyzer::AbstractAnalyzer, _::StateAtPC, gr::GlobalRef) = Any[string(gr.mod, '.', gr.name)], nothing
function _get_sig_type(analyzer::AbstractAnalyzer, s::StateAtPC, name::Symbol)
    if istoplevel(analyzer, first(s))
        # this is concrete global variable, form the global reference
        return _get_sig_type(analyzer, s, GlobalRef(get_toplevelmod(analyzer), name))
    else
        return Any[repr(name; context = :compact => true)], nothing
    end
end
function _get_sig_type(analyzer::AbstractAnalyzer, s::StateAtPC, gotoifnot::GotoIfNot)
    sig  = Any[string("goto %", gotoifnot.dest, " if not "), _get_sig(analyzer, s, gotoifnot.cond)...]
    return sig, nothing
end
function _get_sig_type(analyzer::AbstractAnalyzer, s::StateAtPC, rn::ReturnNode)
    sig = is_unreachable(rn) ? Any["unreachable"] : Any["return ", _get_sig(analyzer, s, rn.val)...]
    return sig, nothing
end
function _get_sig_type(analyzer::AbstractAnalyzer, ::StateAtPC, qn::QuoteNode)
    typ = typeof(qn.value)
    return Any[string(qn), typ], typ
end
_get_sig_type(analyzer::AbstractAnalyzer, ::StateAtPC, @nospecialize(x)) = Any[repr(x; context = :compact => true)], nothing
