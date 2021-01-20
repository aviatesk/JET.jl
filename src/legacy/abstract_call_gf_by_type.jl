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
Core.eval(CC, quote

function abstract_call_gf_by_type(interp::$(JETInterpreter), @nospecialize(f), argtypes::Vector{Any}, @nospecialize(atype), sv::InferenceState,
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
            if $(is_empty_match)(info)
                # report `NoMethodErrorReport` for union-split signatures
                $(report!)(interp, $(NoMethodErrorReport)(interp, sv, true, atype))
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
        if $(is_empty_match)(info)
            # report `NoMethodErrorReport` for this call signature
            $(report!)(interp, $(NoMethodErrorReport)(interp, sv, false, atype))
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
    edges = Any[]
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
        if istoplevel && !isdispatchtuple(sig) && !$(istoplevel)(sv) # keep going for "our" toplevel frame
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
    :(isdefined(CC, :LimitedAccuracy) && quote
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

end) # Core.eval(CC, quote
# %% for easier interactive update of abstract_call_gf_by_type

end # function overload_abstract_call_gf_by_type!()
push_inithook!(overload_abstract_call_gf_by_type!)
