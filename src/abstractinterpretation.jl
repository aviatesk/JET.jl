# this file is horrible because we don't have a stable `AbstractInterpreter` API yet
# TODO once https://github.com/JuliaLang/julia/pull/39305 gets merged, we can remove the
# overloadings of `abstract_call_gf_by_type` and `abstract_call_method_with_const_args`
# and keep their legacy definitions in a separate file for compatbility with older Julia versions

const IS_LATEST = isdefined(Core, :InterConditional)

"""
    mutable struct AbstractGlobal
        # analyzed type
        t::Any
        # `id` of `JETInterpreter` that defined this
        id::Symbol
        # `Symbol` of a dummy generic function that generates dummy backedge (i.e. `li`)
        edge_sym::Symbol
        # dummy backedge, which will be invalidated on update of `t`
        li::MethodInstance
        # whether this abstract global variable is declarared as constant or not
        iscd::Bool
    end

Wraps a global variable whose type is analyzed by abtract interpretation.
`AbstractGlobal` object will be actually evaluated into the context module, and a later
  analysis may refer to its type or alter it on another assignment.
On the refinement of the abstract global variable, the dummy backedge associated with it
  will be invalidated, and inference depending on that will be re-run on the next analysis.
"""
mutable struct AbstractGlobal
    # analyzed type
    t::Any
    # `id` of `JETInterpreter` that lastly assigned this global variable
    id::Symbol
    # the name of a dummy generic function that generates dummy backedge `li`
    edge_sym::Symbol
    # dummy backedge associated to this global variable, which will be invalidated on update of `t`
    li::MethodInstance
    # whether this abstract global variable is declarared as constant or not
    iscd::Bool

    function AbstractGlobal(@nospecialize(t),
                            id::Symbol,
                            edge_sym::Symbol,
                            li::MethodInstance,
                            iscd::Bool,
                            )
        return new(t, id, edge_sym, li, iscd)
    end
end

"""
    function overload_abstract_call_gf_by_type!()
        ...
    end
    push_inithook!(overload_abstract_call_gf_by_type!)

the aims of this overload are:
1. report `NoMethodErrorReport` on empty method signature matching
2. keep inference on non-concrete call sites in toplevel frame created by [`virtual_process!`](@ref)
3. don't bail out even after the current return type grows up to `Any` and collect as much
   error points as possible; of course it slows down inference performance, but hopefully it
   stays to be "practical" speed (because the number of matching methods is limited beforehand)
4. force constant prop' even if the inference result can't be improved anymore when `rettype`
   is already `Const`; this is because constant prop' can still produce more "correct"
   analysis by throwing away the error reports in the callee frames
5. always add backedges (even if a new method can't refine the return type grew up to`Any`),
   because a new method always may change the JET analysis result
"""
function overload_abstract_call_gf_by_type!()

# %% for easier interactive update of abstract_call_gf_by_type

ex = @static if IS_LATEST; quote

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.

function abstract_call_gf_by_type(interp::$JETInterpreter, @nospecialize(f),
                                  fargs::Union{Nothing,Vector{Any}}, argtypes::Vector{Any}, @nospecialize(atype),
                                  sv::InferenceState, max_methods::Int = InferenceParams(interp).MAX_METHODS)
    if sv.params.unoptimize_throw_blocks && sv.currpc in sv.throw_blocks
        return CallMeta(Any, false)
    end
    valid_worlds = WorldRange()
    atype_params = unwrap_unionall(atype).parameters
    splitunions = 1 < unionsplitcost(atype_params) <= InferenceParams(interp).MAX_UNION_SPLITTING
    mts = Core.MethodTable[]
    fullmatch = Bool[]
    if splitunions
        splitsigs = switchtupleunion(atype)
        applicable = Any[]
        infos = MethodMatchInfo[]
        for sig_n in splitsigs
            mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
            if mt === nothing
                add_remark!(interp, sv, "Could not identify method table for call")
                return CallMeta(Any, false)
            end
            mt = mt::Core.MethodTable
            matches = findall(sig_n, method_table(interp); limit=max_methods)
            if matches === missing
                add_remark!(interp, sv, "For one of the union split cases, too many methods matched")
                return CallMeta(Any, false)
            end
            #=== abstract_call_gf_by_type patch point 1-1 start ===#
            info = MethodMatchInfo(matches)
            if $is_empty_match(info)
                # report `NoMethodErrorReport` for union-split signatures
                $report!(interp, $NoMethodErrorReport(interp, sv, true, atype))
            end
            push!(infos, info)
            #=== abstract_call_gf_by_type patch point 1-1 end ===#
            append!(applicable, matches)
            valid_worlds = intersect(valid_worlds, matches.valid_worlds)
            thisfullmatch = _any(match->(match::MethodMatch).fully_covers, matches)
            found = false
            for (i, mt′) in enumerate(mts)
                if mt′ === mt
                    fullmatch[i] &= thisfullmatch
                    found = true
                    break
                end
            end
            if !found
                push!(mts, mt)
                push!(fullmatch, thisfullmatch)
            end
        end
        info = UnionSplitInfo(infos)
    else
        mt = ccall(:jl_method_table_for, Any, (Any,), atype)
        if mt === nothing
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(Any, false)
        end
        mt = mt::Core.MethodTable
        matches = findall(atype, method_table(interp, sv); limit=max_methods)
        if matches === missing
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            add_remark!(interp, sv, "Too many methods matched")
            return CallMeta(Any, false)
        end
        push!(mts, mt)
        push!(fullmatch, _any(match->(match::MethodMatch).fully_covers, matches))
        info = MethodMatchInfo(matches)
        #=== abstract_call_gf_by_type patch point 1-2 start ===#
        if $is_empty_match(info)
            # report `NoMethodErrorReport` for this call signature
            $report!(interp, $NoMethodErrorReport(interp, sv, false, atype))
        end
        #=== abstract_call_gf_by_type patch point 1-2 end ===#
        applicable = matches.matches
        valid_worlds = matches.valid_worlds
    end
    update_valid_age!(sv, valid_worlds)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edgecycle = false
    edges = MethodInstance[]
    conditionals = nothing # keeps refinement information of call argument types when the return type is boolean
    nonbot = 0             # the index of the only non-Bottom inference result if > 0
    seen = 0               # number of signatures actually inferred
    multiple_matches = napplicable > 1

    if f !== nothing && napplicable == 1 && is_method_pure(applicable[1]::MethodMatch)
        val = pure_eval_call(f, argtypes)
        if val !== false
            # TODO: add some sort of edge(s)
            return CallMeta(val, MethodResultPure())
        end
    end

    #=== abstract_call_gf_by_type patch point 4-1 start ===#
    nreports = length(interp.reports)
    #=== abstract_call_gf_by_type patch point 4-1 end ===#

    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if bail_out_toplevel_call(interp, sig, sv)
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            rettype = Any
            break
        end
        sigtuple = unwrap_unionall(sig)::DataType
        this_rt = Bottom
        splitunions = false
        # TODO: splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).MAX_UNION_SPLITTING
        # this used to trigger a bug in inference recursion detection, and is unmaintained now
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1, edge = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, sv)
                edgecycle |= edgecycle1::Bool
                if edge !== nothing
                    push!(edges, edge)
                end
                this_rt = tmerge(this_rt, rt)
                if bail_out_call(interp, this_rt, sv)
                    break
                end
            end
        else
            this_rt, edgecycle1, edge = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, sv)
            edgecycle |= edgecycle1::Bool
            if edge !== nothing
                push!(edges, edge)
            end
        end
        this_conditional = ignorelimited(this_rt)
        this_rt = widenwrappedconditional(this_rt)
        @assert !(this_conditional isa Conditional) "invalid lattice element returned from inter-procedural context"
        if this_rt !== Bottom
            if nonbot === 0
                nonbot = i
            else
                nonbot = -1
            end
        end
        seen += 1
        rettype = tmerge(rettype, this_rt)
        if bail_out_call(interp, rettype, sv)
            break
        end
        if this_conditional !== Bottom && is_lattice_bool(rettype) && fargs !== nothing
            if conditionals === nothing
                conditionals = Any[Bottom for _ in 1:length(argtypes)],
                               Any[Bottom for _ in 1:length(argtypes)]
            end
            condval = maybe_extract_const_bool(this_conditional)
            for i = 1:length(argtypes)
                fargs[i] isa Slot || continue
                if this_conditional isa InterConditional && this_conditional.slot == i
                    vtype = this_conditional.vtype
                    elsetype = this_conditional.elsetype
                else
                    elsetype = vtype = tmeet(argtypes[i], fieldtype(sig, i))
                    condval === true && (elsetype = Union{})
                    condval === false && (vtype = Union{})
                end
                conditionals[1][i] = tmerge(conditionals[1][i], vtype)
                conditionals[2][i] = tmerge(conditionals[2][i], elsetype)
            end
        end
    end

    #=== abstract_call_gf_by_type patch point 4-2 start ===#
    # check if constant propagation can improve analysis by throwing away possibly false positive reports
    has_been_reported = (length(interp.reports) - nreports) > 0
    #=== abstract_call_gf_by_type patch point 4-2 end ===#

    # try constant propagation if only 1 method is inferred to non-Bottom
    # this is in preparation for inlining, or improving the return result
    is_unused = call_result_unused(sv)
    #=== abstract_call_gf_by_type patch point 4-3 start ===#
    if nonbot > 0 && seen == napplicable && (!edgecycle || !is_unused) &&
            (is_improvable(rettype) || has_been_reported) && InferenceParams(interp).ipo_constant_propagation
    #=== abstract_call_gf_by_type patch point 4-3 end ===#
        # if there's a possibility we could constant-propagate a better result
        # (hopefully without doing too much work), try to do that now
        # TODO: refactor this, enable constant propagation for each (union-split) signature
        match = applicable[nonbot]::MethodMatch
        const_rettype, result = abstract_call_method_with_const_args(interp, rettype, f, argtypes, applicable[nonbot]::MethodMatch, sv, edgecycle)
        const_conditional = ignorelimited(const_rettype)
        @assert !(const_conditional isa Conditional) "invalid lattice element returned from inter-procedural context"
        const_rettype = widenwrappedconditional(const_rettype)
        if ignorelimited(const_rettype) ⊑ rettype
            # use the better result, if it is a refinement of rettype
            rettype = const_rettype
            if const_conditional isa InterConditional && conditionals === nothing && fargs !== nothing
                arg = fargs[const_conditional.slot]
                if arg isa Slot
                    rettype = Conditional(arg, const_conditional.vtype, const_conditional.elsetype)
                    if const_rettype isa LimitedAccuracy
                        rettype = LimitedAccuracy(rettype, const_rettype.causes)
                    end
                end
            end
        end
        if result !== nothing
            info = ConstCallInfo(info, result)
        end
        # and update refinements with the InterConditional info too
        # (here we ignorelimited, since there isn't much below this in the
        # lattice, particularly when we're already using tmeet)
        if const_conditional isa InterConditional && conditionals !== nothing
            let i = const_conditional.slot,
                vtype = const_conditional.vtype,
                elsetype = const_conditional.elsetype
                if !(vtype ⊑ conditionals[1][i])
                    vtype = tmeet(conditionals[1][i], widenconst(vtype))
                end
                if !(elsetype ⊑ conditionals[2][i])
                    elsetype = tmeet(conditionals[2][i], widenconst(elsetype))
                end
                conditionals[1][i] = vtype
                conditionals[2][i] = elsetype
            end
        end
    end
    if rettype isa LimitedAccuracy
        union!(sv.pclimitations, rettype.causes)
        rettype = rettype.typ
    end
    # if we have argument refinement information, apply that now to get the result
    if is_lattice_bool(rettype) && conditionals !== nothing && fargs !== nothing
        slot = 0
        vtype = elsetype = Any
        condval = maybe_extract_const_bool(rettype)
        for i in 1:length(fargs)
            # find the first argument which supports refinment,
            # and intersect all equvalent arguments with it
            arg = fargs[i]
            arg isa Slot || continue # can't refine
            old = argtypes[i]
            old isa Type || continue # unlikely to refine
            id = slot_id(arg)
            if slot == 0 || id == slot
                new_vtype = conditionals[1][i]
                if condval === false
                    vtype = Union{}
                elseif new_vtype ⊑ vtype
                    vtype = new_vtype
                else
                    vtype = tmeet(vtype, widenconst(new_vtype))
                end
                new_elsetype = conditionals[2][i]
                if condval === true
                    elsetype = Union{}
                elseif new_elsetype ⊑ elsetype
                    elsetype = new_elsetype
                else
                    elsetype = tmeet(elsetype, widenconst(new_elsetype))
                end
                if (slot > 0 || condval !== false) && !(old ⊑ vtype) # essentially vtype ⋤ old
                    slot = id
                elseif (slot > 0 || condval !== true) && !(old ⊑ elsetype) # essentially elsetype ⋤ old
                    slot = id
                else # reset: no new useful information for this slot
                    vtype = elsetype = Any
                    if slot > 0
                        slot = 0
                    end
                end
            end
        end
        if vtype === Bottom && elsetype === Bottom
            rettype = Bottom # accidentally proved this call to be dead / throw !
        elseif slot > 0
            rettype = Conditional(SlotNumber(slot), vtype, elsetype) # record a Conditional improvement to this slot
        end
    end
    @assert !(rettype isa InterConditional) "invalid lattice element returned from inter-procedural context"
    if is_unused && !(rettype === Bottom)
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    add_call_backedges!(interp, rettype, edges, fullmatch, mts, atype, sv)
    if !isempty(sv.pclimitations) # remove self, if present
        delete!(sv.pclimitations, sv)
        for caller in sv.callers_in_cycle
            delete!(sv.pclimitations, caller)
        end
    end
    #print("=> ", rettype, "\n")
    return CallMeta(rettype, info)
end

end; else; quote # @static if IS_LATEST; quote

function abstract_call_gf_by_type(interp::$JETInterpreter, @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
                                  max_methods::Int = InferenceParams(interp).MAX_METHODS)
    if sv.params.unoptimize_throw_blocks && sv.currpc in sv.throw_blocks
        return CallMeta(Any, false)
    end
    valid_worlds = WorldRange()
    atype_params = unwrap_unionall(atype).parameters
    splitunions = 1 < unionsplitcost(atype_params) <= InferenceParams(interp).MAX_UNION_SPLITTING
    mts = Core.MethodTable[]
    fullmatch = Bool[]
    if splitunions
        splitsigs = switchtupleunion(atype)
        applicable = Any[]
        infos = MethodMatchInfo[]
        for sig_n in splitsigs
            mt = ccall(:jl_method_table_for, Any, (Any,), sig_n)
            if mt === nothing
                add_remark!(interp, sv, "Could not identify method table for call")
                return CallMeta(Any, false)
            end
            mt = mt::Core.MethodTable
            matches = findall(sig_n, method_table(interp); limit=max_methods)
            if matches === missing
                add_remark!(interp, sv, "For one of the union split cases, too many methods matched")
                return CallMeta(Any, false)
            end
            #=== abstract_call_gf_by_type patch point 1-1 start ===#
            info = MethodMatchInfo(matches)
            if $is_empty_match(info)
                # report `NoMethodErrorReport` for union-split signatures
                $report!(interp, $NoMethodErrorReport(interp, sv, true, atype))
            end
            push!(infos, info)
            #=== abstract_call_gf_by_type patch point 1-1 end ===#
            append!(applicable, matches)
            valid_worlds = intersect(valid_worlds, matches.valid_worlds)
            thisfullmatch = _any(match->(match::MethodMatch).fully_covers, matches)
            found = false
            for (i, mt′) in enumerate(mts)
                if mt′ === mt
                    fullmatch[i] &= thisfullmatch
                    found = true
                    break
                end
            end
            if !found
                push!(mts, mt)
                push!(fullmatch, thisfullmatch)
            end
        end
        info = UnionSplitInfo(infos)
    else
        mt = ccall(:jl_method_table_for, Any, (Any,), atype)
        if mt === nothing
            add_remark!(interp, sv, "Could not identify method table for call")
            return CallMeta(Any, false)
        end
        mt = mt::Core.MethodTable
        matches = findall(atype, method_table(interp, sv); limit=max_methods)
        if matches === missing
            # this means too many methods matched
            # (assume this will always be true, so we don't compute / update valid age in this case)
            add_remark!(interp, sv, "Too many methods matched")
            return CallMeta(Any, false)
        end
        push!(mts, mt)
        push!(fullmatch, _any(match->(match::MethodMatch).fully_covers, matches))
        info = MethodMatchInfo(matches)
        #=== abstract_call_gf_by_type patch point 1-2 start ===#
        if $is_empty_match(info)
            # report `NoMethodErrorReport` for this call signature
            $report!(interp, $NoMethodErrorReport(interp, sv, false, atype))
        end
        #=== abstract_call_gf_by_type patch point 1-2 end ===#
        applicable = matches.matches
        valid_worlds = matches.valid_worlds
    end
    update_valid_age!(sv, valid_worlds)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edgecycle = false
    edges = MethodInstance[]
    nonbot = 0  # the index of the only non-Bottom inference result if > 0
    seen = 0    # number of signatures actually inferred
    istoplevel = sv.linfo.def isa Module
    multiple_matches = napplicable > 1

    if f !== nothing && napplicable == 1 && is_method_pure(applicable[1]::MethodMatch)
        val = pure_eval_call(f, argtypes)
        if val !== false
            # TODO: add some sort of edge(s)
            return CallMeta(val, MethodResultPure())
        end
    end

    #=== abstract_call_gf_by_type patch point 4-1 start ===#
    nreports = length(interp.reports)
    #=== abstract_call_gf_by_type patch point 4-1 end ===#

    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        #=== abstract_call_gf_by_type patch point 2 start ===#
        if istoplevel && !isdispatchtuple(sig) && !$istoplevel(sv) # keep going for "our" toplevel frame
        #=== abstract_call_gf_by_type patch point 2 end ===#
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            rettype = Any
            break
        end
        sigtuple = unwrap_unionall(sig)::DataType
        splitunions = false
        this_rt = Bottom
        # TODO: splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1, edge = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, sv)
                if edge !== nothing
                    push!(edges, edge)
                end
                edgecycle |= edgecycle1::Bool
                this_rt = tmerge(this_rt, rt)
                #=== abstract_call_gf_by_type patch point 3-1 start ===#
                # this_rt === Any && break # keep going and collect as much error reports as possible
                #=== abstract_call_gf_by_type patch point 3-1 end ===#
            end
        else
            this_rt, edgecycle1, edge = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, sv)
            edgecycle |= edgecycle1::Bool
            if edge !== nothing
                push!(edges, edge)
            end
        end
        if this_rt !== Bottom
            if nonbot === 0
                nonbot = i
            else
                nonbot = -1
            end
        end
        seen += 1
        rettype = tmerge(rettype, this_rt)
        #=== abstract_call_gf_by_type patch point 3-2 start ===#
        # rettype === Any && break # keep going and collect as much error reports as possible
        #=== abstract_call_gf_by_type patch point 3-2 end ===#
    end

    #=== abstract_call_gf_by_type patch point 4-2 start ===#
    # check if constant propagation can improve analysis by throwing away possibly false positive reports
    has_been_reported = (length(interp.reports) - nreports) > 0
    #=== abstract_call_gf_by_type patch point 4-2 end ===#

    # try constant propagation if only 1 method is inferred to non-Bottom
    # this is in preparation for inlining, or improving the return result
    is_unused = call_result_unused(sv)
    #=== abstract_call_gf_by_type patch point 4-3 start ===#
    if nonbot > 0 && seen == napplicable && (!edgecycle || !is_unused) &&
            (is_improvable(rettype) || has_been_reported) && InferenceParams(interp).ipo_constant_propagation
    #=== abstract_call_gf_by_type patch point 4-3 end ===#
        # if there's a possibility we could constant-propagate a better result
        # (hopefully without doing too much work), try to do that now
        # TODO: it feels like this could be better integrated into abstract_call_method / typeinf_edge
        const_rettype = abstract_call_method_with_const_args(interp, rettype, f, argtypes, applicable[nonbot]::MethodMatch, sv, edgecycle)
        if const_rettype ⊑ rettype
            # use the better result, if it's a refinement of rettype
            rettype = const_rettype
        end
    end
    if is_unused && !(rettype === Bottom)
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    #=== abstract_call_gf_by_type patch point 5 start ===#
    # a new method may refine analysis, so we always add backedges
    if true # !(rettype === Any) # adding a new method couldn't refine (widen) this type
    #=== abstract_call_gf_by_type patch point 5 end ===#
        for edge in edges
            add_backedge!(edge::MethodInstance, sv)
        end
        for (thisfullmatch, mt) in zip(fullmatch, mts)
            if !thisfullmatch
                # also need an edge to the method table in case something gets
                # added that did not intersect with any existing method
                add_mt_backedge!(mt, atype, sv)
            end
        end
    end
    #print("=> ", rettype, "\n")
    $(isdefined(CC, :LimitedAccuracy) && quote
    if rettype isa LimitedAccuracy
        union!(sv.pclimitations, rettype.causes)
        rettype = rettype.typ
    end
    if !isempty(sv.pclimitations) # remove self, if present
        delete!(sv.pclimitations, sv)
        for caller in sv.callers_in_cycle
            delete!(sv.pclimitations, caller)
        end
    end
    end)
    return CallMeta(rettype, info)
end

end; end # @static if IS_LATEST; quote

Core.eval(CC, ex)
# %% for easier interactive update of abstract_call_gf_by_type

end # function overload_abstract_call_gf_by_type!()
push_inithook!(overload_abstract_call_gf_by_type!)

function is_empty_match(info)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

@static if IS_LATEST

import .CC:
    bail_out_call,
    bail_out_toplevel_call,
    add_call_backedges!

# keep going and collect as much error reports as possible
bail_out_call(interp::JETInterpreter, @nospecialize(t), sv) = false

function bail_out_toplevel_call(interp::JETInterpreter, @nospecialize(sig), sv)
    return isa(sv.linfo.def, Module) && !isdispatchtuple(sig) && !istoplevel(sv)
end

function add_call_backedges!(interp::JETInterpreter,
                             @nospecialize(rettype),
                             edges::Vector{MethodInstance},
                             fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                             sv::InferenceState)
    # a new method may refine analysis, so we always add backedges
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

end # @static if IS_LATEST

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

ex = @static if IS_LATEST; quote

function abstract_call_method_with_const_args(interp::$JETInterpreter, @nospecialize(rettype), @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch, sv::InferenceState, edgecycle::Bool)
    method = match.method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return Any, nothing
    haveconst = false
    allconst = true
    # see if any or all of the arguments are constant and propagating constants may be worthwhile
    for a in argtypes
        a = widenconditional(a)
        if allconst && !isa(a, Const) && !isconstType(a) && !isa(a, PartialStruct) && !isa(a, PartialOpaque)
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
    haveconst || #= improvable_via_constant_propagation(rettype) || =# return Any, nothing
    #=== abstract_call_method_with_const_args patch point 1 end ===#
    force_inference = method.aggressive_constprop || InferenceParams(interp).aggressive_constant_propagation
    if !force_inference && nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                return Any, nothing
            elseif arrty ⊑ Array
                return Any, nothing
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if itrty ⊑ Array
                return Any, nothing
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
        length(argtypes) > 2 || return Any, nothing
        t1 = widenconst(argtypes[2])
        all_same = true
        for i in 3:length(argtypes)
            if widenconst(argtypes[i]) !== t1
                all_same = false
                break
            end
        end
        all_same && return Any, nothing
    end
    if istopfunction(f, :getproperty) || istopfunction(f, :setproperty!)
        force_inference = true
    end
    force_inference |= allconst
    mi = specialize_method(match, !force_inference)
    mi === nothing && return Any, nothing
    mi = mi::MethodInstance
    # decide if it's likely to be worthwhile
    if !force_inference && !const_prop_heuristic(interp, method, mi)
        return Any, nothing
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
                    return Any, nothing
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
        frame === nothing && return Any, nothing # this is probably a bad generated function (unsound), but just ignore it
        frame.parent = sv
        push!(inf_cache, inf_result)
        typeinf(interp, frame) || return Any, nothing
    end
    result = inf_result.result
    # if constant inference hits a cycle, just bail out
    isa(result, InferenceState) && return Any, nothing
    #=== abstract_call_method_with_const_args patch point 3 start ===#
    add_backedge!(mi, sv)
    $update_reports!(interp, sv)
    #=== abstract_call_method_with_const_args patch point 3 end ===#
    return result, inf_result
end

end; else; quote # @static if IS_LATEST; quote

function abstract_call_method_with_const_args(interp::$JETInterpreter, @nospecialize(rettype), @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch, sv::InferenceState, edgecycle::Bool)
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
    #=== abstract_call_method_with_const_args patch point 3 start ===#
    add_backedge!(mi, sv)
    $update_reports!(interp, sv)
    #=== abstract_call_method_with_const_args patch point 3 end ===#
    return result
end

end; end # @static if IS_LATEST; quote

Core.eval(CC, ex)

# %% for easier interactive update of abstract_call_method_with_const_args

end # function overload_abstract_call_method_with_const_args!()
push_inithook!(overload_abstract_call_method_with_const_args!)

# works within inter-procedural context
function CC.abstract_call_method(interp::JETInterpreter, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    ret = @invoke abstract_call_method(interp::AbstractInterpreter, method::Method, sig, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)

    update_reports!(interp, sv)

    return ret
end

function update_reports!(interp::JETInterpreter, sv::InferenceState)
    rs = interp.to_be_updated
    if !isempty(rs)
        vf = get_virtual_frame(interp, sv)
        for r in rs
            pushfirst!(r.st, vf)
        end
        empty!(rs)
    end
end

function CC.abstract_eval_special_value(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if isa(e, Slot) && is_global_slot(interp, e)
            if get_slottype(sv, e) === Bottom
                # if this abstract global variable is not initialized, form the global
                # reference and abstract intepret it; we may have abstract interpreted this
                # variable and it may have a type
                # if it's really not defined, the error will be generated later anyway
                e = GlobalRef(interp.toplevelmod, get_slotname(sv, e))
            end
        elseif isa(e, Symbol)
            # (already concretized) toplevel global symbols
            e = GlobalRef(interp.toplevelmod, e)
        end
    end

    ret = @invoke abstract_eval_special_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    if isa(ret, Const)
        # unwrap abstract global variable to actual type
        val = ret.val
        if isa(val, AbstractGlobal)
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

            # special case and propagate `Main` module as constant
            # XXX this was somewhat critical for accuracy and performance, but I'm not sure this still holds
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

    return @invoke abstract_eval_statement(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)
end

function CC.finish(me::InferenceState, interp::JETInterpreter)
    @invoke finish(me::InferenceState, interp::AbstractInterpreter)

    if istoplevel(me)
        # find assignments of abstract global variables, and assign types to them,
        # so that later analysis can refer to them

        stmts = me.src.code
        bbs = compute_basic_blocks(stmts)
        assigns = Dict{Int,Bool}() # slot id => is this deterministic
        for (pc, stmt) in enumerate(stmts)
            if @isexpr(stmt, :(=))
                lhs = first(stmt.args)
                if isa(lhs, Slot)
                    slot = slot_id(lhs)
                    if is_global_slot(interp, slot)
                        isnd = is_nondeterministic(pc, bbs)

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
                slotname = interp.global_slots[slot]
                typ = slottypes[slot]
                set_abstract_global!(interp, interp.toplevelmod, slotname, typ, isnd, me)
            end
        end
    end
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

# XXX and TODO this is a super coarse version of what Julia's type inference routine does
# for local function body (types of variables are computed as a fixed point of the abstract
# interpretation algorithm); `set_abstract_global!` assumes global variable assignments
# happen sequentially and deterministically, which is obviously not always correct.
# Currently `set_abstract_global!` only handles super simple branching, but ideally we
# want to implement a "global version" of the type inference routine which should handle all
# the control flows correctly
function set_abstract_global!(interp, mod, name, @nospecialize(t), isnd, sv)
    local update::Bool = false
    id = get_id(interp)

    iscd = is_constant_declared(name, sv)

    t′, id′, (edge_sym, li) = if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, AbstractGlobal)
            t′ = val.t
            if val.iscd && widenconst(t′) !== widenconst(t)
                report!(interp, InvalidConstantRedefinition(interp, sv, mod, name, widenconst(t′), widenconst(t)))
                return
            end

            # update previously-defined abstract global variable
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
        # define new abstract global variable
        Bottom, id, gen_dummy_backedge(mod)
    end

    # if this is constant declared and it's value is known to be constant, let's concretize
    # it for good reasons; we will be able to use it in concrete interpretation and so
    # this allows us to define structs with global type aliases, etc.
    # XXX maybe check for constant declaration is not necessary
    if isa(t, Const) && iscd
        return Core.eval(mod, :(const $(name) = $(t.val)))
    end

    # if this assignment happens in an non-deterministic way, we need to perform type merge
    isnd && (t = tmerge(t′, t))

    if id !== id′
        # invalidate the dummy backedge that is bound to this abstract global variable,
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
        :(const $(name) = $(AbstractGlobal(t, id, edge_sym, li, iscd)))
    end
    return Core.eval(mod, ex)::AbstractGlobal
end

function is_constant_declared(name, sv)
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

function gen_dummy_backedge(mod)
    edge_sym = gensym(:dummy_edge_sym)
    return edge_sym, force_invalidate!(mod, edge_sym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(mod, edge_sym)
    λ = Core.eval(mod, :($(edge_sym)() = return))::Function
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, svec())::MethodInstance
end
