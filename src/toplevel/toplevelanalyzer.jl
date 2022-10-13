# top-level bridge
# ================

# types
# -----

struct ToplevelAnalyzer{Analyzer<:AbstractAnalyzer} <: AbstractAnalyzer
    analyzer::Analyzer
end

# AbstractAnalyzer API requirements
# forward to the inner analyzer
ToplevelAnalyzer{Analyzer}(; jetconfigs...) where Analyzer<:AbstractAnalyzer = ToplevelAnalyzer(Analyzer(; jetconfigs...))
AnalyzerState(analyzer::ToplevelAnalyzer) = AnalyzerState(analyzer.analyzer)
AbstractAnalyzer(analyzer::ToplevelAnalyzer, state::AnalyzerState) = ToplevelAnalyzer(AbstractAnalyzer(analyzer.analyzer, state))
ReportPass(analyzer::ToplevelAnalyzer) = ToplevelAnalysisPass(ReportPass(analyzer.analyzer))
get_cache_key(analyzer::ToplevelAnalyzer) = get_cache_key(analyzer.analyzer)
vscode_source(analyzer::ToplevelAnalyzer, args...) = vscode_source(analyzer.analyzer, args...)
vscode_diagnostics_order(analyzer::ToplevelAnalyzer, args...) = vscode_diagnostics_order(analyzer.analyzer, args...)

struct ToplevelAnalysisPass{RP<:ReportPass} <: ReportPass
    report_pass::RP
end

# forward to the inner report pass
(rp::ToplevelAnalysisPass)(args...) = rp.report_pass(args...)

@jetreport struct InvalidConstantRedefinition <: InferenceErrorReport
    mod::Module
    name::Symbol
    @nospecialize t′
    @nospecialize t
end
function print_report_message(io::IO, report::InvalidConstantRedefinition)
    print(io, "invalid redefinition of constant `", report.mod, '.', report.name, "` (from `", report.t′, "` to `", report.t, "`)")
end
print_signature(::InvalidConstantRedefinition) = false
function (::ToplevelAnalysisPass)(::Type{InvalidConstantRedefinition}, analyzer::ToplevelAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    add_new_report!(analyzer, sv.result, InvalidConstantRedefinition(sv, mod, name, prev_t, t))
    return true
end

@jetreport struct InvalidConstantDeclaration <: InferenceErrorReport
    mod::Module
    name::Symbol
end
function print_report_message(io::IO, report::InvalidConstantDeclaration)
    print(io, "cannot declare a constant `", report.mod, '.', report.name, "`; it already has a value")
end
print_signature(::InvalidConstantDeclaration) = false
function (::ToplevelAnalysisPass)(::Type{InvalidConstantDeclaration}, analyzer::ToplevelAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(analyzer, sv.result, InvalidConstantDeclaration(sv, mod, name))
    return true
end

"""
    mutable struct AbstractGlobal
        t::Any     # analyzed type
        iscd::Bool # is this abstract global variable declarared as constant or not
    end

Wraps a global variable whose type is analyzed by abtract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
analysis may refer to or alter its type on future load and store operations.

!!! note
    The type of the wrapped global variable will be propagated only when in a toplevel frame,
    and thus we don't care about the analysis cache invalidation on a refinement of the
    wrapped global variable, since JET doesn't cache the toplevel frame.
"""
mutable struct AbstractGlobal
    t::Any     # analyzed type
    iscd::Bool # is this abstract global variable declarared as constant or not
    AbstractGlobal(@nospecialize(t), iscd::Bool) = new(t, iscd)
end

# overloads
# ---------

@doc """
    bail_out_toplevel_call(analyzer::AbstractAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::AbstractAnalyzer, ...)`, which keeps
inference on non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
CC.bail_out_toplevel_call(analyzer::ToplevelAnalyzer, @nospecialize(sig), sv::InferenceState) = false

function CC.abstract_eval_special_value(analyzer::ToplevelAnalyzer{Analyzer},
    @nospecialize(e), vtypes::VarTable, sv::InferenceState) where Analyzer<:AbstractAnalyzer
    istoplevel = JET.istoplevel(sv)

    if istoplevel
        if isa(e, Slot) && is_global_slot(analyzer, e)
            if get_slottype((sv, get_currpc(sv)), e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract intepret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(get_toplevelmod(analyzer), get_slotname(sv, e))
            end
        end
    end

    ret = @invoke CC.abstract_eval_special_value(analyzer::Analyzer,
        e::Any, vtypes::VarTable, sv::InferenceState)

    if istoplevel
        if isa(e, GlobalRef)
            mod, name = e.mod, e.name
            if isdefined(mod, name)
                # eagerly propagate the type of this global variable:
                # of course the traced type might be difference from its type in actual execution
                # e.g. we don't track global variable assignments that can happen somewhere
                # in this call graph, but it's highly possible this is a toplevel callsite
                # and we take a risk here since we can't enter the analysis otherwise
                val = getglobal(mod, name)
                ret = isa(val, AbstractGlobal) ? val.t : Const(val)
            end
        end
    end

    return ret
end

function CC.abstract_eval_value(analyzer::ToplevelAnalyzer{Analyzer},
    @nospecialize(e), vtypes::VarTable, sv::InferenceState) where Analyzer<:AbstractAnalyzer
    ret = @invoke CC.abstract_eval_value(analyzer::Analyzer,
        e::Any, vtypes::VarTable, sv::InferenceState)

    # HACK if we encounter `_INACTIVE_EXCEPTION`, it means `ConcreteInterpreter` tried to
    # concretize an exception which was not actually thrown – yet the actual error hasn't
    # happened thanks to JuliaInterpreter's implementation detail, i.e. JuliaInterpreter
    # could retrieve `FrameData.last_exception`, which is initialized with
    # `_INACTIVE_EXCEPTION.instance` – but it's obviously not a sound approximation of an
    # actual execution and so here we will fix it to `Any`, since we don't analyze types of
    # exceptions in general
    if ret ⊑ _INACTIVE_EXCEPTION
        ret = Any
    end

    return ret
end

function CC.abstract_eval_statement(analyzer::ToplevelAnalyzer{Analyzer},
    @nospecialize(e), vtypes::VarTable, sv::InferenceState) where Analyzer<:AbstractAnalyzer
    if istoplevel(sv)
        if get_concretized(analyzer)[get_currpc(sv)]
            return Any # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end

    return @invoke CC.abstract_eval_statement(analyzer::Analyzer,
        e::Any, vtypes::VarTable, sv::InferenceState)
end

function CC.builtin_tfunction(analyzer::ToplevelAnalyzer{Analyzer},
    @nospecialize(f), argtypes::Array{Any,1}, sv::InferenceState # NOTE `ToplevelAnalyzer` isn't overloaded on `return_type`
    ) where Analyzer<:AbstractAnalyzer
    ret = @invoke CC.builtin_tfunction(analyzer::Analyzer,
        f::Any, argtypes::Array{Any,1}, sv::Union{InferenceState,Nothing})

    if f === getglobal
        if istoplevel_getproperty(sv)
            ret = maybe_narrow_toplevel_getglobal(argtypes, ret)
        end
    elseif (@static isdefined(Core, :get_binding_type) ? (f === Core.get_binding_type) : false)
        if istoplevel(sv)
            ret = maybe_narrow_toplevel_binding_type(argtypes, ret)
        end
    end

    return ret
end

# check if this frame is for `getproperty(::Module, ::Symbol)`,
# that may access to abstract global variable traced by `analyzer`
function istoplevel_getproperty(sv::InferenceState)
    def = sv.linfo.def
    isa(def, Method) || return false
    def.name === :getproperty || return false
    def.sig === Tuple{typeof(getproperty), Module, Symbol} || return false
    parent = sv.parent
    parent === nothing && return false
    return istoplevel(parent)
end

# if this `getglobal` access to a global variable in a module concretized by `AbstractAnalyzer`,
# eargely propagate its type (NOTE the logic here should be synced with the implementation
# of `abstract_eval_special_value(::AbstractAnalyzer, ...)`)
function maybe_narrow_toplevel_getglobal(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. constant or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    isdefined(gr.mod, gr.name) || return ret
    val = getglobal(gr.mod, gr.name)
    return isa(val, AbstractGlobal) ? val.t : Const(val)
end

# if the type for a variable hasn't been declared explicitly, return the narrower type
# declaration (`Const(Any)`) to allow JET to analyze the first assignment for the variable
# (, that would follow most of the time as generated by the frontend) more precisely
function maybe_narrow_toplevel_binding_type(argtypes::Vector{Any}, @nospecialize ret)
    (isa(ret, Const) || ret === Bottom) && return ret # i.e. already declared or error
    gr = constant_globalref(argtypes)
    gr === nothing && return ret
    @assert !isdefined(gr.mod, gr.name) "`get_binding_type` should resolve the already-defined variable type"
    return Const(Any)
end

function constant_globalref(argtypes::Vector{Any})
    length(argtypes) ≥ 2 || return nothing
    mod = argtypes[1]
    isa(mod, Const) || return nothing
    mod = mod.val
    isa(mod, Module) || return nothing
    sym = argtypes[2]
    isa(sym, Const) || return nothing
    sym = sym.val
    isa(sym, Symbol) || return nothing
    return GlobalRef(mod, sym)
end

function CC.finish(me::InferenceState, analyzer::ToplevelAnalyzer{Analyzer}) where Analyzer<:AbstractAnalyzer
    ret = @invoke CC.finish(me::InferenceState, analyzer::Analyzer)

    if istoplevel(me)
        # find assignments of abstract global variables, and assign types to them,
        # so that later analysis can refer to them

        stmts = me.src.code
        cfg = compute_basic_blocks(stmts)
        assigns = Dict{Int,Bool}() # slot id => is this deterministic
        for (pc, stmt) in enumerate(stmts)
            if isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, Slot)
                    slot = slot_id(lhs)
                    if is_global_slot(analyzer, slot)
                        isnd = is_assignment_nondeterministic(cfg, pc)

                        # COMBAK this approach is really not true when there're multiple
                        # assignments in different basic blocks
                        if haskey(assigns, slot)
                            assigns[slot] |= isnd
                        else
                            assigns[slot] = isnd
                        end
                    end
                end
            end
        end

        if !isempty(assigns)
            slottypes = collect_slottypes(me)
            for (slot, isnd) in assigns
                slotname = get_global_slots(analyzer)[slot]
                typ = slottypes[slot]
                set_abstract_global!(analyzer, get_toplevelmod(analyzer), slotname, typ, isnd, me)
            end
        end
    end

    return ret
end

# simple cfg analysis to check if the assignment at `pc` will happen non-deterministically
function is_assignment_nondeterministic(cfg::CFG, pc::Int)
    isnd = false

    blocks = cfg.blocks
    for (idx, block) in enumerate(blocks)
        if pc in rng(block)
            for block′ in blocks
                succs = block′.succs
                if idx in succs
                    isnd |= length(succs) > 1
                end
            end
        end
    end

    return isnd
end

# at this point all the types of SSA values are iterated to maximum fixed point,
# and we can compute types of slot as least upper bound of types of all the possible
# assignment of the slot (the type of assignment statement is available as SSA value type)
# the implementation is almost same as `record_slot_assign!(sv::InferenceState)`,
# but we don't `widenconst` each SSA value type
@static if hasfield(InferenceState, :stmt_types)
function collect_slottypes(sv::InferenceState)
    states = sv.stmt_types
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    stmts = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in 1:length(sv.slottypes)]
    for i = 1:length(stmts)
        stmt = stmts[i]
        state = states[i]
        # find all reachable assignments to locals
        if isa(state, VarTable) && isexpr(stmt, :(=))
            lhs = first(stmt.args)
            if isa(lhs, Slot)
                vt = ssavaluetypes[i] # don't widen const
                @assert vt !== NOT_FOUND "active slot in unreached region"
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    slottypes[id] = tmerge(otherTy, vt)
                end
            end
        end
    end
    return slottypes
end
else
function collect_slottypes(sv::InferenceState)
    body = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in 1:length(sv.slottypes)]
    ssavaluetypes = sv.ssavaluetypes
    for i = 1:length(body)
        expr = body[i]
        # find all reachable assignments to locals
        if CC.was_reached(sv, i) && isexpr(expr, :(=))
            lhs = expr.args[1]
            if isa(lhs, SlotNumber)
                typ = ssavaluetypes[i]
                @assert typ !== NOT_FOUND "active slot in unreached region"
                vt = typ
                if vt !== Bottom
                    id = slot_id(lhs)
                    otherTy = slottypes[id]
                    if otherTy === Bottom
                        slottypes[id] = vt
                    elseif otherTy === Any
                        slottypes[id] = Any
                    else
                        slottypes[id] = tmerge(otherTy, vt)
                    end
                end
            end
        end
    end
    return slottypes
end
end

function set_abstract_global!(analyzer::ToplevelAnalyzer, mod::Module, name::Symbol, @nospecialize(t), isnd::Bool, sv::InferenceState)
    prev_agv = nothing
    prev_t = nothing
    iscd = is_constant_declared(name, sv)

    # check if this global variable is already assigned previously
    if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, AbstractGlobal)
            prev_t = val.t
            if val.iscd && (prev_t′ = widenconst(prev_t)) !== (t′ = widenconst(t))
                warn_invalid_const_global!(name)
                ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t′, t′)
                return
            end
            prev_agv = val
        else
            prev_t = Core.Typeof(val)
            if isconst(mod, name)
                invalid = prev_t !== (t′ = widenconst(t))
                if invalid || !isa(t, Const)
                    warn_invalid_const_global!(name)
                    if invalid
                        ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t, t′) # ignored by default
                    end
                    return
                end
                # otherwise, we can just redefine this constant, and Julia will warn it
                ex = iscd ? :(const $name = $(QuoteNode(t.val))) : :($name = $(QuoteNode(t.val)))
                return Core.eval(mod, ex)
            end
        end
    end

    isnew = isnothing(prev_t)

    # if this constant declaration is invalid, just report it and bail out
    if iscd && !isnew
        warn_invalid_const_global!(name)
        ReportPass(analyzer)(InvalidConstantDeclaration, analyzer, sv, mod, name) # ignored by default
        return
    end

    # if this assignment happens non-deterministically, we need to take the previous type into account
    if isnd
        if !isnew # if this assignment is an initialization, we just need to use `t`
            t = tmerge(prev_t, t)
        end
    else
        # if this assignment happens deterministically, and the assigned value is known to be
        # constant statically, let's concretize it for good reasons;
        # we will be able to use it in concrete interpretation and so this allows to define
        # structs with type aliases, etc.
        local v
        if isa(t, Const)
            v = t.val
        elseif isconstType(t)
            v = t.parameters[1]
        elseif issingletontype(t)
            v = t.instance
        end
        if @isdefined v
            if iscd
                @assert isnew # means, this is a valid constant declaration
                return Core.eval(mod, :(const $name = $(QuoteNode(v))))
            else
                # we've checked `mod.name` wasn't declared as constant previously
                return Core.eval(mod, :($name = $(QuoteNode(v))))
            end
        end
    end

    # okay, we will define new abstract global variable from here on
    if isa(prev_agv, AbstractGlobal)
        return Core.eval(mod, :(let name = $name::$AbstractGlobal
            name.t = $t
            name
        end))
    else
        return Core.eval(mod, :($name = $(AbstractGlobal(t, iscd))))
    end
end

warn_invalid_const_global!(name::Symbol) = @warn """
JET.jl can't update the definition of this constant declared global variable: `$name`
This may fail, cause incorrect analysis, or produce unexpected errors.
"""

function is_constant_declared(name::Symbol, sv::InferenceState)
    return any(sv.src.code) do @nospecialize(x)
        if isexpr(x, :const)
            arg = first(x.args)
            # `transform_abstract_global_symbols!` replaces all the global symbols in this toplevel frame with `Slot`s
            if isa(arg, Slot)
                return get_slotname(sv, arg) === name
            end
        end
        return false
    end
end
