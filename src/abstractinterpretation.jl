mutable struct VirtualGlobalVariable
    # actual profiled type
    t::Any
    # keeps `id` of `JETInterpreter` that defined this
    id::Symbol
    # `Symbol` of a dummy generic function that generates dummy backedge (i.e. `li`)
    edge_sym::Symbol
    # dummy backedge, which will be invalidated on update of `t`
    li::MethodInstance
    # whether this virtual global variable is declarared as constant or not
    iscd::Bool

    function VirtualGlobalVariable(@nospecialize(t),
                                   id::Symbol,
                                   edge_sym::Symbol,
                                   li::MethodInstance,
                                   iscd::Bool,
                                   )
        return new(t, id, edge_sym, li, iscd)
    end
end

@static if isdefined(CC, :bail_out_call)

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.

function abstract_call_gf_by_type(interp::JETInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    ret = @invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                           max_methods::Int)

    if isa(ret, CallMeta)
        info = ret.info
        if isa(info, UnionSplitInfo)
            for match in info.matches
                if is_empty_match(match)
                    report!(interp, NoMethodErrorReport(interp, sv, true, atype))
                end
            end
        elseif isa(info, MethodMatchInfo)
            if is_empty_match(info)
                report!(interp, NoMethodErrorReport(interp, sv, false, atype))
            end
        end
    end

    return ret
end

import .CC:
    bail_out_toplevel_call,
    bail_out_call,
    add_call_backedges!

function CC.bail_out_toplevel_call(interp::JETInterpreter, @nospecialize(sig), sv)
    istoplevel(sv) && return false # keep going for "our" toplevel frame
    return @invoke bail_out_toplevel_call(interp::AbstractInterpreter, sig, sv)
end

# keep going and collect as much error reports as possible
CC.bail_out_call(interp::JETInterpreter, @nospecialize(t), sv) = false

function CC.add_call_backedges!(interp::JETInterpreter,
                                @nospecialize(rettype),
                                edges::Vector{MethodInstance},
                                fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                                sv::InferenceState)
    @invoke add_call_backedges!(interp::AbstractInterpreter,
                                Bottom, # always add backedges !
                                edges::Vector{MethodInstance},
                                fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, atype,
                                sv::InferenceState)
end

else # if isdefined(CC, :bail_out_call)

include("legacy/abstract_call_gf_by_type.jl")

end # if isdefined(CC, :bail_out_call)

function is_empty_match(info::MethodMatchInfo)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

"""
    function overload_abstract_call_method_with_const_args!()
        ...
    end
    push_inithook!(overload_abstract_call_method_with_const_args!)

the aim of this overloads is:
1. force constant prop' even if the inference result can't be improved anymore when `rettype`
   is already `Const`; this is because constant prop' can still produce more "correct"
   analysis by throwing away the error reports in the callee frames
"""
function overload_abstract_call_method_with_const_args!()

# %% for easier interactive update of abstract_call_method_with_const_args
Core.eval(CC, quote

function abstract_call_method_with_const_args(interp::$(JETInterpreter), @nospecialize(rettype), @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch, sv::InferenceState, edgecycle::Bool)
    method = match.method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return Any
    haveconst = false
    allconst = true
    # see if any or all of the arguments are constant and propagating constants may be worthwhile
    for a in argtypes
        a = widenconditional(a)
        if allconst && !isa(a, Const) && !isconstType(a) && !isa(a, PartialStruct)
            allconst = false
        end
        if !haveconst && has_nontrivial_const_info(a) && const_prop_profitable(a)
            haveconst = true
        end
        if haveconst && !allconst
            break
        end
    end
    #=== abstract_call_method_with_const_args patch point 1 start ===#
    # force constant propagation even if it doesn't improve return type;
    # constant prop' may improve report accuracy
    haveconst || #= improvable_via_constant_propagation(rettype) || =# return Any
    #=== abstract_call_method_with_const_args patch point 1 end ===#
    force_inference = $(hasfield(Method, :aggressive_constprop) ? :(method.aggressive_constprop) : false) || InferenceParams(interp).aggressive_constant_propagation
    if !force_inference && nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                return Any
            elseif arrty ⊑ Array
                return Any
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if itrty ⊑ Array
                return Any
            end
        end
    end
    if !force_inference && !allconst &&
        (istopfunction(f, :+) || istopfunction(f, :-) || istopfunction(f, :*) ||
         istopfunction(f, :(==)) || istopfunction(f, :!=) ||
         istopfunction(f, :<=) || istopfunction(f, :>=) || istopfunction(f, :<) || istopfunction(f, :>) ||
         istopfunction(f, :<<) || istopfunction(f, :>>))
         # it is almost useless to inline the op of when all the same type,
         # but highly worthwhile to inline promote of a constant
         length(argtypes) > 2 || return Any
         t1 = widenconst(argtypes[2])
         all_same = true
         for i in 3:length(argtypes)
             if widenconst(argtypes[i]) !== t1
                 all_same = false
                 break
             end
         end
         all_same && return Any
    end
    if istopfunction(f, :getproperty) || istopfunction(f, :setproperty!)
        force_inference = true
    end
    force_inference |= allconst
    mi = specialize_method(match, !force_inference)
    mi === nothing && return Any
    mi = mi::MethodInstance
    # decide if it's likely to be worthwhile
    if !force_inference && !const_prop_heuristic(interp, method, mi)
        return Any
    end
    inf_cache = get_inference_cache(interp)
    inf_result = cache_lookup(mi, argtypes, inf_cache)
    if inf_result === nothing
        if edgecycle
            # if there might be a cycle, check to make sure we don't end up
            # calling ourselves here.
            infstate = sv
            cyclei = 0
            while !(infstate === nothing)
                if method === infstate.linfo.def && any(infstate.result.overridden_by_const)
                    return Any
                end
                if cyclei < length(infstate.callers_in_cycle)
                    cyclei += 1
                    infstate = infstate.callers_in_cycle[cyclei]
                else
                    cyclei = 0
                    infstate = infstate.parent
                end
            end
        end
        inf_result = InferenceResult(mi, argtypes)
        frame = InferenceState(inf_result, #=cache=#false, interp)
        frame === nothing && return Any # this is probably a bad generated function (unsound), but just ignore it
        $(isdefined(CC, :LimitedAccuracy) || :(frame.limited = true))
        frame.parent = sv
        push!(inf_cache, inf_result)
        typeinf(interp, frame) || return Any
    end
    result = inf_result.result
    # if constant inference hits a cycle, just bail out
    isa(result, InferenceState) && return Any
    add_backedge!(inf_result.linfo, sv)
    return result
end

end) # Core.eval(CC, quote
# %% for easier interactive update of abstract_call_method_with_const_args

end # function overload_abstract_call_method_with_const_args!()
push_inithook!(overload_abstract_call_method_with_const_args!)

function CC.abstract_eval_special_value(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke abstract_eval_special_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    if isa(ret, Const)
        # unwrap virtual global variable to actual type
        val = ret.val
        if isa(val, VirtualGlobalVariable)
            # add dummy backedge, which will be invalidated on update of this vitual global variable
            add_backedge!(val.li, sv)

            ret = val.t
        end
    elseif isa(e, GlobalRef)
        mod, name = e.mod, e.name
        if isdefined(mod, name)
            # we don't track types of global variables except when we're in toplevel frame,
            # and here we just annotate this as `Any`; NOTE: this is becasue:
            # - we can't track side-effects of assignments of global variables that happen in
            #   (possibly deeply nested) callees, and it might be possible if we just ignore
            #   assignments happens in callees that aren't reached by type inference by
            #   the widening heuristics
            # - consistency with Julia's native type inference
            # - it's hard to track side effects for cached frames

            # TODO: add report pass here (for performance linting)

            # special case and propagate `Main` module as constant adn this is somewhat
            # critical for performance when self-profiling
            if name === :Main
                ret = Const(Main)
            end
        else
            # report access to undefined global variable
            report!(interp, GlobalUndefVarErrorReport(interp, sv, mod, name))

            # `ret` at this point should be annotated as `Any` by `NativeInterpreter`, and
            # we just pass it as is to collect as much error points as possible within this
            # frame
            # IDEA: we can change it to `Bottom` to suppress any further abstract interpretation
            # with this variable, and the later analysis after the update on this (currently)
            # undefined variable just works because we will invalidate the cache for this frame
            # anyway
        end
    end

    return ret
end

function CC.abstract_eval_value(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke abstract_eval_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # report non-boolean condition error
    stmt = get_stmt(sv)
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom && !⊑(Bool, t)
            report!(interp, NonBooleanCondErrorReport(interp, sv, t))
            ret = Bottom
        end
    end

    return ret
end

function CC.abstract_eval_statement(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if interp.concretized[get_currpc(sv)]
            return Any # bail out if it has been interpreted by `ConcreteInterpreter`
        end
    end

    ret = @invoke abstract_eval_statement(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # assign virtual global variable
    # for toplevel frames, we do virtual global variable assignments, whose types are
    # propagated even if they're non-constant
    if istoplevel(sv)
        stmt = get_stmt(sv)
        if @isexpr(stmt, :(=))
            lhs = first(stmt.args)

            if isa(lhs, GlobalRef)
                set_virtual_globalvar!(interp, lhs.mod, lhs.name, ignorelimited(ret), sv)
            end
        end
    end

    return ret
end

function set_virtual_globalvar!(interp, mod, name, @nospecialize(t), sv)
    local update::Bool = false
    id = get_id(interp)

    iscd = is_constant_declared(mod, name, sv.src.code)

    t′, id′, (edge_sym, li) = if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, VirtualGlobalVariable)
            t′ = val.t
            if val.iscd && widenconst(t′) !== widenconst(t)
                report!(interp, InvalidConstantRedefinition(interp, sv, mod, name, widenconst(t′), widenconst(t)))
                return
            end

            # update previously-defined virtual global variable
            update = true
            t′, val.id, (val.edge_sym, val.li)
        else
            if isconst(mod, name)
                t′ = typeof(val)
                if t′ !== widenconst(t)
                    report!(interp, InvalidConstantRedefinition(interp, sv, mod, name, t′, widenconst(t)))
                    return
                end
            end

            # this pass hopefully won't happen within the current design
            @warn "JET.jl can't trace updates of global variable that already have values" mod name val
            return
        end
    else
        # define new virtual global variable
        Bottom, id, gen_dummy_backedge(mod)
    end

    # if this is constant declared and it's value is known to be constant, let's concretize
    # it for good reasons; this will help us analyse on code with global type aliases, etc.
    if !(t ⊑ VirtualGlobalVariable) && isa(t, Const) && iscd
        return Core.eval(mod, :(const $(name) = $(t.val)))
    end

    # at this point undefined slots may still be annotated as `NOT_FOUND` (e.g. when there is
    # undefined slot, etc.), which can't be used as abstract value for later profiling,
    # so replace it with Bottom
    if t === NOT_FOUND
        t = Bottom
    end

    if begin
            # if the previous virtual global variable assignment happened in the same inference process,
            # JET needs to perform type merge
            id === id′ ||
            # if this assignment happens in an non-deterministic way, we still need to perform type merge
            # NOTE: this may happen multiple times for the same statement (within an iteration for
            # maximum fixed point computation), so pre-computing basic blocks before entering a toplevel
            # inference frame might be better
            is_nondeterministic(sv)
        end
        t = tmerge(t′, t)
    end

    if id !== id′
        # invalidate the dummy backedge that is bound to this virtual global variable,
        # so that depending `MethodInstance` will run fresh type inference on the next hit
        li = force_invalidate!(mod, edge_sym)
    end

    ex = if update
        quote
            local name = $(name)
            name.t = $(t)
            name.id = $(QuoteNode(id))
            name.edge_sym = $(QuoteNode(edge_sym))
            name.li = $(li)
            name
        end
    else
        vgv = VirtualGlobalVariable(t, id, edge_sym, li, iscd)
        :(const $(name) = $(vgv))
    end
    return Core.eval(mod, ex)::VirtualGlobalVariable
end

function is_constant_declared(mod, name, stmts)
    # `fix_global_symbols!` replaces all the symbols in a toplevel frame with `GlobalRef`
    gr = GlobalRef(mod, name)

    return any(stmts) do @nospecialize(x)
        if @isexpr(x, :const)
            arg = first(x.args)
            isa(arg, GlobalRef) && return arg == gr
        end
        return false
    end
end

is_nondeterministic(sv) = is_nondeterministic(get_currpc(sv), compute_basic_blocks(sv.src.code))

# XXX: does this approach really cover all the control flow ?
function is_nondeterministic(pc, bbs)
    isnd = false

    for (idx, bb) in enumerate(bbs.blocks)
        if pc in bb.stmts
            for bb′ in bbs.blocks
                if idx in bb′.succs
                    isnd |= length(bb′.succs) > 1
                end
            end
        end
    end

    return isnd
end

function gen_dummy_backedge(mod)
    @gensym edge_sym
    return edge_sym, force_invalidate!(mod, edge_sym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(mod, edge_sym)
    λ = Core.eval(mod, :($(edge_sym)() = return))::Function
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, svec())::MethodInstance
end
