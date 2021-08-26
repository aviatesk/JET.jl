"""
    mutable struct AbstractGlobal
        t::Any     # analyzed type
        iscd::Bool # whether this abstract global variable is declarared as constant or not
    end

Wraps a global variable whose type is analyzed by abtract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
  analysis may refer to its type or alter it on another assignment.

!!! note
    The type of the wrapped global variable will be propagated only when in a toplevel frame,
      and thus we don't care about the analysis cache invalidation on a refinement of the
      wrapped global variable, since JET doesn't cache the toplevel frame.
"""
mutable struct AbstractGlobal
    # analyzed type
    t::Any
    iscd::Bool

    function AbstractGlobal(@nospecialize(t),
                            iscd::Bool,
                            )
        return new(t,
                   iscd,
                   )
    end
end

@reportdef struct NoMethodErrorReport <: InferenceErrorReport
    @nospecialize(t::Union{Type,Vector{Type}})
end
# TODO count invalid union split case
get_msg(::Type{NoMethodErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(t::Type)) =
    "no matching method found for call signature ($t)"
get_msg(::Type{NoMethodErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, ts::Vector{Type}) =
    "for $(length(ts)) of union split cases, no matching method found for call signatures ($(join(ts, ", "))))"

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

# branching on https://github.com/JuliaLang/julia/pull/41020
@static if isdefined(CC, :MethodCallResult)

import .CC:
    bail_out_toplevel_call,
    bail_out_call,
    add_call_backedges!,
    MethodCallResult,
    ConstCallInfo,
    const_prop_entry_heuristic,
    const_prop_rettype_heuristic

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.

function CC.abstract_call_gf_by_type(analyzer::AbstractAnalyzer, @nospecialize(f),
                                     fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                     sv::InferenceState, max_methods::Int = InferenceParams(analyzer).MAX_METHODS)
    ret = @invoke abstract_call_gf_by_type(analyzer::AbstractInterpreter, @nospecialize(f),
                                           fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                           sv::InferenceState, max_methods::Int)

    info = ret.info
    if isa(info, ConstCallInfo)
        info = info.call # unwrap to `MethodMatchInfo` or `UnionSplitInfo`
    end
    # report passes for no matching methods error
    if isa(info, UnionSplitInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, argtypes)
    elseif isa(info, MethodMatchInfo)
        ReportPass(analyzer)(NoMethodErrorReport, analyzer, sv, info, atype)
    end

    analyze_task_parallel_code!(analyzer, f, argtypes, sv)

    return ret
end

function (::SoundBasicPass)(::Type{NoMethodErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, info::UnionSplitInfo, argtypes::Vector{Any})
    # check each match for union-split signature
    split_argtypes = nothing
    ts = nothing

    for (i, matchinfo) in enumerate(info.matches)
        if is_empty_match(matchinfo)
            isnothing(split_argtypes) && (split_argtypes = switchtupleunion(argtypes))
            isnothing(ts) && (ts = Type[])
            sig_n = argtypes_to_type(split_argtypes[i])
            push!(ts, sig_n)
        end
    end

    if !isnothing(ts)
        add_new_report!(NoMethodErrorReport(analyzer, sv, ts), analyzer)
    end
end
function (::SoundBasicPass)(::Type{NoMethodErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, info::MethodMatchInfo, @nospecialize(atype))
    if is_empty_match(info)
        add_new_report!(NoMethodErrorReport(analyzer, sv, atype), analyzer)
    end
end

@doc """
    bail_out_toplevel_call(analyzer::AbstractAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::AbstractAnalyzer, ...)`, which keeps
inference on non-concrete call sites in a toplevel frame created by [`virtual_process`](@ref).
"""
CC.bail_out_toplevel_call(analyzer::AbstractAnalyzer, @nospecialize(sig), sv) = false

@doc """
    bail_out_call(analyzer::AbstractAnalyzer, ...)

With this overload, `abstract_call_gf_by_type(analyzer::AbstractAnalyzer, ...)` doesn't bail out
  inference even after the current return type grows up to `Any` and collects as much error
  points as possible.
Of course this slows down inference performance, but hoopefully it stays to be "practical"
  speed since the number of matching methods are limited beforehand.
"""
CC.bail_out_call(analyzer::AbstractAnalyzer, @nospecialize(t), sv) = false

@doc """
    add_call_backedges!(analyzer::AbstractAnalyzer, ...)

An overload for `abstract_call_gf_by_type(analyzer::AbstractAnalyzer, ...)`, which always add
  backedges (even if a new method can't refine the return type grew up to `Any`).
This is because a new method definition always has a potential to change the JET analysis result.
"""
:(add_call_backedges!)
# branch on https://github.com/JuliaLang/julia/pull/41633
@static if isdefined(CC, :find_matching_methods)
import .CC:
    MethodMatches,
    UnionSplitMethodMatches
function CC.add_call_backedges!(analyzer::AbstractAnalyzer, @nospecialize(rettype), edges::Vector{MethodInstance},
                                matches::Union{MethodMatches,UnionSplitMethodMatches}, @nospecialize(atype),
                                sv::InferenceState)
    # NOTE a new method may refine analysis, so we always add backedges
    # # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine (widen) this type
    # rettype === Any && return
    # for edge in edges
    #     add_backedge!(edge, sv)
    # end
    # also need an edge to the method table in case something gets
    # added that did not intersect with any existing method
    if isa(matches, MethodMatches)
        matches.fullmatch || add_mt_backedge!(matches.mt, atype, sv)
    else
        for (thisfullmatch, mt) in zip(matches.fullmatches, matches.mts)
            thisfullmatch || add_mt_backedge!(mt, atype, sv)
        end
    end
end
else # @static if isdefined(CC, :find_matching_methods)
function CC.add_call_backedges!(analyzer::AbstractAnalyzer, @nospecialize(rettype), edges::Vector{MethodInstance},
                                fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                                sv::InferenceState)
    # NOTE a new method may refine analysis, so we always add backedges
    # if rettype === Any
    #     # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine
    #     # (widen) this type
    #     return
    # end
    for edge in edges
        add_backedge!(edge, sv)
    end
    for (thisfullmatch, mt) in zip(fullmatch, mts)
        if !thisfullmatch
            # also need an edge to the method table in case something gets
            # added that did not intersect with any existing method
            add_mt_backedge!(mt, atype, sv)
        end
    end
end
end # @static if isdefined(CC, :find_matching_methods)

function CC.abstract_call_method_with_const_args(analyzer::AbstractAnalyzer, result::MethodCallResult,
                                                 @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                                 sv::InferenceState, va_override::Bool)
    const_result =
        @invoke abstract_call_method_with_const_args(analyzer::AbstractInterpreter, result::MethodCallResult,
                                                     @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                                     sv::InferenceState, va_override::Bool)
    # update reports if constant prop' was successful
    # branch on https://github.com/JuliaLang/julia/pull/41697/
    @static if VERSION ≥ v"1.8.0-DEV.282"
        if !isnothing(const_result)
            # successful constant prop', we also need to update reports
            update_reports!(analyzer, sv)
        end
    else
        if !isnothing(getfield(const_result, 2))
            # successful constant prop', we also need to update reports
            update_reports!(analyzer, sv)
        end
    end
    return const_result
end

@doc """
    const_prop_entry_heuristic(analyzer::AbstractAnalyzer, result::MethodCallResult, sv::InferenceState)

This overload for `abstract_call_method_with_const_args(analyzer::AbstractAnalyzer, ...)` forces
constant prop' even if an inference result can't be improved anymore _with respect to the
return type_, e.g. when `result.rt` is already `Const`.
Especially, this overload implements an heuristic to force constant prop' when any error points
have been reported while the previous abstract method call without constant arguments.
The reason we want much more aggressive constant propagation by that heuristic is that it's
highly possible constant prop' can produce more accurate analysis result, by throwing away
false positive error reports by cutting off the unreachable control flow or detecting
must-reachable `throw` calls.
"""
function CC.const_prop_entry_heuristic(analyzer::AbstractAnalyzer, result::MethodCallResult, sv::InferenceState)
    edge = result.edge # edge associated with the previous non-constant analysis
    if !isnothing(edge)
        # if any error has been reported within the previous `abstract_call_method` (associated with `edge`),
        # force constant prop' and hope it can cut-off false positives
        any(is_from_same_frame(sv.linfo, edge), get_reports(analyzer)) && return true
    end
    return @invoke const_prop_entry_heuristic(analyzer::AbstractInterpreter, result::MethodCallResult, sv::InferenceState)
end

else # @static if isdefined(CC, :MethodCallResult)

include("legacy/abstractinterpretation")

end # @static if isdefined(CC, :MethodCallResult)

"""
    analyze_task_parallel_code!(analyzer::AbstractAnalyzer, @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState)

Adds special cased analysis pass for task parallelism (xref: <https://github.com/aviatesk/JET.jl/issues/114>).
In Julia's task parallelism implementation, parallel code is represented as closure and it's
  wrapped in a `Task` object. `NativeInterpreter` doesn't run type inference nor optimization
  on the body of those closures when compiling code that creates parallel tasks, but JET will
  try to run additional analysis pass by recurring into the closures.

!!! note
    JET won't do anything other than doing JET analysis, e.g. won't annotate return type
    of wrapped code block in order to not confuse the original `AbstractInterpreter` routine
    track <https://github.com/JuliaLang/julia/pull/39773> for the changes in native abstract
    interpretation routine.
"""
function analyze_task_parallel_code!(analyzer::AbstractAnalyzer, @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState)
    # TODO ideally JET should analyze a closure wrapped in a `Task` only when it's `schedule`d
    # but the `Task` construction may not happen in the same frame where it's `schedule`d
    # and so we may not be able to access to the closure at that point
    # as a compromise, JET now invokes the additional analysis on `Task` construction,
    # regardless of whether it's really `schedule`d or not
    if f === Task &&
       length(argtypes) ≥ 2 &&
       (v = argtypes[2]; v ⊑ Function)
        # if we encounter `Task(::Function)`, try to get its inner function and run analysis on it
        # the closure can be a nullary lambda that really doesn't depend on
        # the captured environment, and in that case we can retrieve it as
        # a function object, otherwise we will try to retrieve the type of the closure
        ft = (isa(v, Const) ? Core.Typeof(v.val) :
              isa(v, Core.PartialStruct) ? v.typ :
              isa(v, DataType) ? v :
              return)::Type
        analyze_additional_pass_by_type!(analyzer, Tuple{ft}, sv)
        return
    end
    return
end

# run additional interpretation with a new interpreter,
# and then append the reports to the original interpreter
function analyze_additional_pass_by_type!(analyzer::AbstractAnalyzer, @nospecialize(tt::Type{<:Tuple}), sv::InferenceState)
    newanalyzer = AbstractAnalyzer(analyzer)

    # in order to preserve the inference termination, we keep to use the current frame
    # and borrow the `AbstractInterpreter`'s cycle detection logic
    # XXX the additional analysis pass by `abstract_call_method` may involve various site-effects,
    # but what we're doing here is essentially equivalent to modifying the user code and inlining
    # the threaded code block as a usual code block, and thus the side-effects won't (hopefully)
    # confuse the abstract interpretation, which is supposed to terminate on any kind of code
    mm = get_single_method_match(tt, InferenceParams(newanalyzer).MAX_METHODS, get_world_counter(newanalyzer))
    result = abstract_call_method(newanalyzer, mm.method, mm.spec_types, mm.sparams, false, sv)

    rt = @static @isdefined(MethodCallResult) ? result.rt : first(result)

    # corresponding to the same logic in `analyze_frame!`
    if rt === Bottom
        if !isempty(get_uncaught_exceptions(newanalyzer))
            append!(get_reports(newanalyzer), get_uncaught_exceptions(newanalyzer))
        end
    end

    append!(get_reports(analyzer), get_reports(newanalyzer))
end

# works within inter-procedural context
function CC.abstract_call_method(analyzer::AbstractAnalyzer, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    ret = @invoke abstract_call_method(analyzer::AbstractInterpreter, method::Method, sig, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)

    update_reports!(analyzer, sv)

    return ret
end

function update_reports!(analyzer::AbstractAnalyzer, sv::InferenceState)
    rs = get_to_be_updated(analyzer)
    if !isempty(rs)
        vf = get_virtual_frame(sv)
        for r in rs
            pushfirst!(r.vst, vf)
        end
        empty!(rs)
    end
end

@static if isdefined(CC, :abstract_invoke)

import .CC:
    abstract_invoke,
    InvokeCallInfo,
    instanceof_tfunc

function CC.abstract_invoke(analyzer::AbstractAnalyzer, argtypes::Vector{Any}, sv::InferenceState)
    ret = @invoke abstract_invoke(analyzer::AbstractInterpreter, argtypes::Vector{Any}, sv::InferenceState)

    # if the `abstract_invoke(::AbstractInterpreter)` was successful, we need to update reports
    # since it's an inter-procedural inference that internally uses `typeinf_edge`
    info = ret.info
    if isa(info, InvokeCallInfo)
        update_reports!(analyzer, sv)
    end

    ReportPass(analyzer)(InvalidInvokeErrorReport, analyzer, sv, ret, argtypes)

    return ret
end

@reportdef struct InvalidInvokeErrorReport <: InferenceErrorReport
    argtypes::Vector{Any}
end
function get_msg(::Type{InvalidInvokeErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, argtypes::Vector{Any})
    fallback_msg = "invalid invoke" # mostly because of runtime unreachable

    ft = widenconst(argtype_by_index(argtypes, 2))
    ft === Bottom && return fallback_msg
    t = argtype_by_index(argtypes, 3)
    (types, isexact, isconcrete, istype) = instanceof_tfunc(t)
    if types === Bottom
        if isa(t, Const)
            type = typeof(t.val)
            return "argument type should be `Type`-object (given `$type`)"
        end
        return fallback_msg
    end

    argtype = argtypes_to_type(argtype_tail(argtypes, 4))
    nargtype = typeintersect(types, argtype)
    @assert nargtype === Bottom
    return "actual argument type (`$argtype`) doesn't intersect with specified argument type (`$types`)"
end

function (::SoundBasicPass)(::Type{InvalidInvokeErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, ret::CallMeta, argtypes::Vector{Any})
    if ret.rt === Bottom
        # here we report error that happens at the call of `invoke` itself.
        # if the error type (`Bottom`) is propagated from the `invoke`d call, the error has
        # already been reported within `typeinf_edge`, so ignore that case
        if !isa(ret.info, InvokeCallInfo)
            add_new_report!(InvalidInvokeErrorReport(analyzer, sv, argtypes), analyzer)
        end
    end
end

end # @static if isdefined(CC, :abstract_invoke)

function CC.abstract_eval_special_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    toplevel = istoplevel(sv)
    if toplevel
        if isa(e, Slot) && is_global_slot(analyzer, e)
            if get_slottype((sv, get_currpc(sv)), e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract intepret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(get_toplevelmod(analyzer), get_slotname(sv, e))
            end
        elseif isa(e, Symbol)
            # (already concretized) toplevel global symbols
            e = GlobalRef(get_toplevelmod(analyzer), e)
        end
    end

    ret = @invoke abstract_eval_special_value(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    if isa(e, GlobalRef)
        mod, name = e.mod, e.name
        if isdefined(mod, name)
            if name === :Main
                # special case and propagate `Main` module as constant
                # XXX this was somewhat critical for accuracy and performance, but I'm not sure this still holds
                ret = Const(Main)
            elseif toplevel
                # here we will eagerly propagate the type of this global variable
                # of course the traced type might be difference from its type in actual execution
                # e.g. we don't track a global variable assignment wrapped in a function,
                # but it's highly possible this is a toplevel callsite and we need to take a
                # risk here, otherwise we can't enter the analysis !
                val = getfield(mod, name)
                ret = isa(val, AbstractGlobal) ? val.t : Const(val)
            else
                # TODO: add report pass to detect non-constant global variable here (for performance linting)
            end
        else
            # report pass for undefined global reference
            ReportPass(analyzer)(GlobalUndefVarErrorReport, analyzer, sv, mod, name)

            # `ret` at this point should be annotated as `Any` by `NativeInterpreter`, and we
            # just pass it as is to collect as much error points as possible within this frame
            # IDEA we can change it to `Bottom` to suppress any further abstract interpretation
            # with this variable, but at the same time we need to make sure we will invalidate
            # the cache for this frame on the future definition of this (currently) undefined binding
            # return Bottom
        end
    end

    return ret
end

@reportdef struct GlobalUndefVarErrorReport <: InferenceErrorReport
    mod::Module
    name::Symbol
end
get_msg(::Type{GlobalUndefVarErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    "variable $(mod).$(name) is not defined"

function (::SoundPass)(::Type{GlobalUndefVarErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(GlobalUndefVarErrorReport(analyzer, sv, mod, name), analyzer)
end

function (::BasicPass)(::Type{GlobalUndefVarErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    is_corecompiler_undefglobal(mod, name) && return
    add_new_report!(GlobalUndefVarErrorReport(analyzer, sv, mod, name), analyzer)
end

"""
    is_corecompiler_undefglobal

Returns `true` if this global reference is undefined inside `Core.Compiler`, but the
  corresponding name exists in the `Base` module.
`Core.Compiler` reuses the minimum amount of `Base` code and there're some of missing
definitions, and `BasicPass` will exclude reports on those undefined names since they
usually don't matter and `Core.Compiler`'s basic functionality is battle-tested and validated
exhausively by its test suite and real-world usages
"""
is_corecompiler_undefglobal(mod::Module, name::Symbol) =
    return mod === CC ? isdefined(Base, name) :
           mod === CC.Sort ? isdefined(Base.Sort, name) :
           false

function CC.abstract_eval_value(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke abstract_eval_value(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

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

    # report non-boolean condition error
    stmt = get_stmt((sv, get_currpc(sv)))
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom
            ReportPass(analyzer)(NonBooleanCondErrorReport, analyzer, sv, t)
            # if this condition leads to an "non-boolean (t) used in boolean context" error,
            # we can turn it into Bottom and bail out early
            # TODO upstream this
            if typeintersect(Bool, t) !== Bool
                ret = Bottom
            end
        end
    end

    return ret
end

@reportdef struct NonBooleanCondErrorReport <: InferenceErrorReport
    @nospecialize(t::Union{Type,Vector{Type}})
end
get_msg(::Type{NonBooleanCondErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(t::Type)) =
    "non-boolean ($t) used in boolean context"
get_msg(::Type{NonBooleanCondErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, ts::Vector{Type}) =
    "for $(length(ts)) of union split cases, non-boolean ($(join(ts, ", "))) used in boolean context"

function (::SoundPass)(::Type{NonBooleanCondErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if !(t ⊑ Bool)
                push!(ts, t)
            end
        end
        if !isempty(ts)
            add_new_report!(NonBooleanCondErrorReport(analyzer, sv, ts), analyzer)
        end
    else
        if !(t ⊑ Bool)
            add_new_report!(NonBooleanCondErrorReport(analyzer, sv, t), analyzer)
        end
    end
end

function (::BasicPass)(::Type{NonBooleanCondErrorReport}, analyzer::AbstractAnalyzer, sv::InferenceState, @nospecialize(t))
    if isa(t, Union)
        ts = Type[]
        for t in Base.uniontypes(t)
            if typeintersect(Bool, t) !== Bool
                # TODO move this to abstractinterpretation.jl
                if JETAnalysisParams(analyzer).strict_condition_check ||
                   !(t <: Function || # !(::Function)
                     t === Missing || # ==(::Missing, ::Any), ==(::Any, ::Missing), ...
                     false)
                    push!(ts, t)
                end
            end
        end
        if !isempty(ts)
            add_new_report!(NonBooleanCondErrorReport(analyzer, sv, ts), analyzer)
        end
    else
        if typeintersect(Bool, t) !== Bool
            add_new_report!(NonBooleanCondErrorReport(analyzer, sv, t), analyzer)
        end
    end
end

function CC.abstract_eval_statement(analyzer::AbstractAnalyzer, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if get_concretized(analyzer)[get_currpc(sv)]
            return Any # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end

    return @invoke abstract_eval_statement(analyzer::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)
end

function CC.finish(me::InferenceState, analyzer::AbstractAnalyzer)
    @invoke finish(me::InferenceState, analyzer::AbstractInterpreter)

    if istoplevel(me)
        # find assignments of abstract global variables, and assign types to them,
        # so that later analysis can refer to them

        stmts = me.src.code
        cfg = compute_basic_blocks(stmts)
        assigns = Dict{Int,Bool}() # slot id => is this deterministic
        for (pc, stmt) in enumerate(stmts)
            if @isexpr(stmt, :(=))
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
# the implementation is mostly same as `record_slot_assign!(sv::InferenceState)`, but
# we don't `widenconst` each SSA value type
function collect_slottypes(sv::InferenceState)
    states = sv.stmt_types
    ssavaluetypes = sv.src.ssavaluetypes::Vector{Any}
    stmts = sv.src.code::Vector{Any}
    slottypes = Any[Bottom for _ in sv.src.slottypes::Vector{Any}]
    for i = 1:length(stmts)
        stmt = stmts[i]
        state = states[i]
        # find all reachable assignments to locals
        if isa(state, VarTable) && @isexpr(stmt, :(=))
            lhs = first(stmt.args)
            if isa(lhs, Slot)
                vt = ssavaluetypes[i] # don't widen const
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

function set_abstract_global!(analyzer::AbstractAnalyzer, mod::Module, name::Symbol, @nospecialize(t), isnd::Bool, sv::InferenceState)
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
                        ReportPass(analyzer)(InvalidConstantRedefinition, analyzer, sv, mod, name, prev_t, t′)
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
        ReportPass(analyzer)(InvalidConstantDeclaration, analyzer, sv, mod, name)
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
        if isa(t, Const)
            if iscd
                @assert isnew # means, this is a valid constant declaration
                return Core.eval(mod, :(const $name = $(QuoteNode(t.val))))
            else
                # we've checked `mod.name` wasn't declared as constant previously
                return Core.eval(mod, :($name = $(QuoteNode(t.val))))
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

# IDEA we may want to hoist `InvalidConstXXX` errors into top-level errors

@reportdef struct InvalidConstantRedefinition <: InferenceErrorReport
    mod::Module
    name::Symbol
    @nospecialize(t′::Any)
    @nospecialize(t::Any)
end
get_msg(::Type{InvalidConstantRedefinition}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(t′::Any), @nospecialize(t::Any)) =
    "invalid redefinition of constant $(mod).$(name) (from $(t′) to $(t))"

function (::SoundBasicPass)(::Type{InvalidConstantRedefinition}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol, @nospecialize(prev_t), @nospecialize(t))
    add_new_report!(InvalidConstantRedefinition(analyzer, sv, mod, name, prev_t, t), analyzer)
end

@reportdef struct InvalidConstantDeclaration <: InferenceErrorReport
    mod::Module
    name::Symbol
end
get_msg(::Type{InvalidConstantDeclaration}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol) =
    "cannot declare a constant $(mod).$(name); it already has a value"

function (::SoundBasicPass)(::Type{InvalidConstantDeclaration}, analyzer::AbstractAnalyzer, sv::InferenceState, mod::Module, name::Symbol)
    add_new_report!(InvalidConstantDeclaration(analyzer, sv, mod, name), analyzer)
end

function is_constant_declared(name::Symbol, sv::InferenceState)
    return any(sv.src.code) do @nospecialize(x)
        if @isexpr(x, :const)
            arg = first(x.args)
            # `transform_global_symbols!` replaces all the global symbols in this toplevel frame with `Slot`s
            if isa(arg, Slot)
                return get_slotname(sv, arg) === name
            end
        end
        return false
    end
end
