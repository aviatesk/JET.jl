mutable struct VirtualGlobalVariable
    # actually profiled type
    t::Any
    # `id` of `JETInterpreter` that defined this
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

# NOTE we're going to adopt unmerged PR https://github.com/JuliaLang/julia/pull/39305 below
# JET will overload the entire body of `abstract_call_gf_by_type` so that it enables
# constant propagation for each union-split signature, which should improve the inference accuracy
# at the cost of possible performance reduction.
# Once the PR is merged, we can remove the monkey patch overloading, but the code can also be
# used as a "legacy code" for older versions of Julia.

# take a Tuple where one or more parameters are Unions
# and return an array such that those Unions are removed
# and `Union{return...} == ty`
function switchtupleunion(@nospecialize(ty))
    tparams = (unwrap_unionall(ty)::DataType).parameters
    return _switchtupleunion(Any[tparams...], length(tparams), [], ty)
end

switchtupleunion(t::Vector{Any}) = _switchtupleunion(t, length(t), [], nothing)

function _switchtupleunion(t::Vector{Any}, i::Int, tunion::Vector{Any}, @nospecialize(origt))
    if i == 0
        if origt === nothing
            push!(tunion, copy(t))
        else
            tpl = rewrap_unionall(Tuple{t...}, origt)
            push!(tunion, tpl)
        end
    else
        ti = t[i]
        if isa(ti, Union)
            for ty in uniontypes(ti::Union)
                t[i] = ty
                _switchtupleunion(t, i - 1, tunion, origt)
            end
            t[i] = ti
        else
            _switchtupleunion(t, i - 1, tunion, origt)
        end
    end
    return tunion
end

"""
    function overload_abstract_call_gf_by_type!()
        ...
    end
    push_inithook!(overload_abstract_call_gf_by_type!)

the aims of this overload are:
1. report `NoMethodErrorReport` on empty method signature matching
2. keep inference on non-concrete call sites in a toplevel frame created by [`virtual_process!`](@ref)
3. don't bail out even after the current return type grows up to `Any` and collect as much
   error points as possible; of course it slows down inference performance, but hopefully it
   stays to be "practical" speed (because the number of matching methods is limited beforehand)
4. always add backedges (even if a new method can't refine the return type grew up to`Any`),
   because a new method always may change the JET analysis result
"""
function overload_abstract_call_gf_by_type!()

# %% for easier interactive update of abstract_call_gf_by_type
Core.eval(CC, quote

# TODO:
# - report "too many method matched"
# - maybe "cound not identify method table for call" won't happen since we eagerly propagate bottom for e.g. undef var case, etc.

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
        splitsigs = $switchtupleunion(atype)
        split_argtypes = $switchtupleunion(argtypes)
        applicable = Any[]
        # arrays like `argtypes`, including constants, for each match
        applicable_argtypes = Vector{Any}[]
        infos = MethodMatchInfo[]
        for j in 1:length(splitsigs)
            sig_n = splitsigs[j]
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
            for _ in 1:length(matches)
                push!(applicable_argtypes, split_argtypes[j])
            end
            # @assert argtypes_to_type(split_argtypes[j]) === sig_n "invalid union split"
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
            $report!(interp, $NoMethodErrorReport(interp, sv, false, atype))
        end
        #=== abstract_call_gf_by_type patch point 1-2 end ===#
        applicable = matches.matches
        valid_worlds = matches.valid_worlds
        applicable_argtypes = nothing
    end
    update_valid_age!(sv, valid_worlds)
    applicable = applicable::Array{Any,1}
    napplicable = length(applicable)
    rettype = Bottom
    edgecycle = false
    edges = MethodInstance[]
    istoplevel = sv.linfo.def isa Module
    multiple_matches = napplicable > 1

    if f !== nothing && napplicable == 1 && is_method_pure(applicable[1]::MethodMatch)
        val = pure_eval_call(f, argtypes)
        if val !== false
            # TODO: add some sort of edge(s)
            return CallMeta(val, MethodResultPure())
        end
    end

    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if $bail_out_toplevel_call(interp, sig, sv)
            # only infer concrete call sites in top-level expressions
            add_remark!(interp, sv, "Refusing to infer non-concrete call site in top-level expression")
            rettype = Any
            break
        end
        sigtuple = unwrap_unionall(sig)::DataType
        this_rt = Bottom
        splitunions = false
        # TODO: splitunions = 1 < unionsplitcost(sigtuple.parameters) * napplicable <= InferenceParams(interp).MAX_UNION_SPLITTING
        # currently this triggers a bug in inference recursion detection
        if splitunions
            splitsigs = $switchtupleunion(sig)
            for sig_n in splitsigs
                rt, edgecycle1, edge = abstract_call_method(interp, method, sig_n, svec(), multiple_matches, sv)
                if edge !== nothing
                    push!(edges, edge)
                end
                edgecycle |= edgecycle1::Bool
                this_rt = tmerge(this_rt, rt)
                this_argtypes = applicable_argtypes === nothing ? argtypes : applicable_argtypes[i]
                const_rt = abstract_call_method_with_const_args(interp, rt, f, this_argtypes, match, sv, edgecycle)
                if const_rt !== rt && const_rt ⊑ rt
                    rt = const_rt
                end
                this_rt = tmerge(this_rt, rt)
                if $bail_out_call(interp, this_rt, sv)
                    break
                end
            end
        else
            this_rt, edgecycle1, edge = abstract_call_method(interp, method, sig, match.sparams, multiple_matches, sv)
            edgecycle |= edgecycle1::Bool
            if edge !== nothing
                push!(edges, edge)
            end
            this_argtypes = applicable_argtypes === nothing ? argtypes : applicable_argtypes[i]
            const_this_rt = abstract_call_method_with_const_args(interp, this_rt, f, this_argtypes, match, sv, edgecycle)
            if const_this_rt !== this_rt && const_this_rt ⊑ this_rt
                this_rt = const_this_rt
            end
        end
        rettype = tmerge(rettype, this_rt)
        if $bail_out_call(interp, rettype, sv)
            break
        end
    end

    if call_result_unused(sv) && !(rettype === Bottom)
        add_remark!(interp, sv, "Call result type was widened because the return value is unused")
        # We're mainly only here because the optimizer might want this code,
        # but we ourselves locally don't typically care about it locally
        # (beyond checking if it always throws).
        # So avoid adding an edge, since we don't want to bother attempting
        # to improve our result even if it does change (to always throw),
        # and avoid keeping track of a more complex result type.
        rettype = Any
    end
    $add_call_backedges!(interp, rettype, edges, fullmatch, mts, atype, sv)
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

end) # Core.eval(CC, quote
# %% for easier interactive update of abstract_call_gf_by_type

end # function overload_abstract_call_gf_by_type!()
push_inithook!(overload_abstract_call_gf_by_type!)

function is_empty_match(info)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

# interfaces introdcued in https://github.com/JuliaLang/julia/pull/39439
# but since we're overloading the entire body of `abstract_call_gf_by_type` for now,
# we don't overload them even if Julia is latest and `Core.Compiler` defines their implementation for `NativeInterpreter`

function bail_out_toplevel_call(interp::JETInterpreter, @nospecialize(sig), sv)
    #=== abstract_call_gf_by_type patch point 2 start ===#
    istoplevel(sv) && return false # keep going for "our" toplevel frame
    #=== abstract_call_gf_by_type patch point 2 end ===#
    # TODO: return @invoke bail_out_toplevel_call(interp::AbstractInterpreter, sig, sv)
    return isa(sv.linfo.def, Module) && !isdispatchtuple(sig)
end
#=== abstract_call_gf_by_type patch point 3 start ===#
bail_out_call(interp::JETInterpreter, @nospecialize(t), sv) = false # keep going and collect as much error reports as possible
#=== abstract_call_gf_by_type patch point 3 end ===#
function add_call_backedges!(interp::JETInterpreter,
                             @nospecialize(rettype),
                             edges::Vector{MethodInstance},
                             fullmatch::Vector{Bool}, mts::Vector{Core.MethodTable}, @nospecialize(atype),
                             sv::InferenceState)
    #=== abstract_call_gf_by_type patch point 4 start ===#
    # a new method may refine analysis, so we always add backedges
    # if rettype === Any
    #     # for `NativeInterpreter`, we don't add backedges when a new method couldn't refine
    #     # (widen) this type
    #     return
    # end
    #=== abstract_call_gf_by_type patch point 4 end ===#
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

function abstract_call_method_with_const_args(interp::JETInterpreter, @nospecialize(rettype),
                                              @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                              sv::InferenceState, edgecycle::Bool)
    mi = maybe_get_const_prop_profitable(interp, rettype, f, argtypes, match, sv, edgecycle)
    mi === nothing && return Any
    # try constant prop'
    inf_cache = get_inference_cache(interp)
    inf_result = cache_lookup(mi, argtypes, inf_cache)
    if inf_result === nothing
        if edgecycle
            # if there might be a cycle, check to make sure we don't end up
            # calling ourselves here.
            infstate = sv
            cyclei = 0
            while !(infstate === nothing)
                if match.method === infstate.linfo.def && CC.any(infstate.result.overridden_by_const)
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
        frame.parent = sv
        CC.push!(inf_cache, inf_result)
        typeinf(interp, frame) || return Any
    end
    result = inf_result.result
    # if constant inference hits a cycle, just bail out
    isa(result, InferenceState) && return Any
    add_backedge!(mi, sv)
    update_reports!(interp, sv)
    return result
end

# if there's a possibility we could get a better result (hopefully without doing too much work)
# returns `MethodInstance` with constant arguments, returns nothing otherwise
function maybe_get_const_prop_profitable(interp::JETInterpreter, @nospecialize(rettype),
                                         @nospecialize(f), argtypes::Vector{Any}, match::MethodMatch,
                                         sv::InferenceState, edgecycle::Bool)
    const_prop_entry_heuristic(interp, rettype, sv, edgecycle) || return nothing
    method = match.method
    nargs::Int = method.nargs
    method.isva && (nargs -= 1)
    length(argtypes) >= nargs || return nothing
    #=== abstract_call_method_with_const_args patch point start ===#
    # force constant propagation even if it doesn't improve return type;
    # constant prop' may improve JET analysis accuracy
    CC.const_prop_argument_heuristic(interp, argtypes) || #= const_prop_rettype_heuristic(interp, rettype) || =# return nothing
    #=== abstract_call_method_with_const_args patch point end ===#
    allconst = CC.is_allconst(argtypes)
    force = CC.force_const_prop(interp, f, method)
    force || CC.const_prop_function_heuristic(interp, f, argtypes, nargs, allconst) || return nothing
    force |= allconst
    mi = specialize_method(match, !force)
    mi === nothing && return nothing
    mi = mi::MethodInstance
    force || CC.const_prop_methodinstance_heuristic(interp, method, mi) || return nothing
    return mi
end

function const_prop_entry_heuristic(interp::JETInterpreter, @nospecialize(rettype), sv::InferenceState, edgecycle::Bool)
    call_result_unused(sv) && edgecycle && return false
    #=== abstract_call_method_with_const_args patch point start ===#
    # force constant propagation even if it doesn't improve return type;
    # constant prop' may improve JET analysis accuracy
    return #= is_improvable(rettype) && =# InferenceParams(interp).ipo_constant_propagation
    #=== abstract_call_method_with_const_args patch point end ===#
end

# NOTE once https://github.com/JuliaLang/julia/pull/39305 gets merged JET doesn't need to
# overload these functions, thus let's evaluate them into `Core.Compiler` and avoid
# maintaining miscellaneous imports
push_inithook!() do; Core.eval(CC, quote

# see if propagating constants may be worthwhile
function const_prop_argument_heuristic(interp::AbstractInterpreter, argtypes::Vector{Any})
    for a in argtypes
        a = widenconditional(a)
        if has_nontrivial_const_info(a) && is_const_prop_profitable_arg(a)
            return true
        end
    end
    return false
end

function is_const_prop_profitable_arg(@nospecialize(arg))
    # have new information from argtypes that wasn't available from the signature
    if isa(arg, PartialStruct)
        for b in arg.fields
            isconstType(b) && return true
            is_const_prop_profitable_arg(b) && return true
        end
    end
    isa(arg, PartialOpaque) && return true
    isa(arg, Const) || return true
    val = arg.val
    # don't consider mutable values or Strings useful constants
    return isa(val, Symbol) || isa(val, Type) || (!isa(val, String) && !ismutable(val))
end

function is_allconst(argtypes::Vector{Any})
    for a in argtypes
        a = widenconditional(a)
        if !isa(a, Const) && !isconstType(a) && !isa(a, PartialStruct) && !isa(a, PartialOpaque)
            return false
        end
    end
    return true
end

function force_const_prop(interp::AbstractInterpreter, @nospecialize(f), method::Method)
    return method.aggressive_constprop ||
           InferenceParams(interp).aggressive_constant_propagation ||
           istopfunction(f, :getproperty) ||
           istopfunction(f, :setproperty!)
end

function const_prop_function_heuristic(interp::AbstractInterpreter, @nospecialize(f), argtypes::Vector{Any}, nargs::Int, allconst::Bool)
    if nargs > 1
        if istopfunction(f, :getindex) || istopfunction(f, :setindex!)
            arrty = argtypes[2]
            # don't propagate constant index into indexing of non-constant array
            if arrty isa Type && arrty <: AbstractArray && !issingletontype(arrty)
                return false
            elseif arrty ⊑ Array
                return false
            end
        elseif istopfunction(f, :iterate)
            itrty = argtypes[2]
            if itrty ⊑ Array
                return false
            end
        end
    end
    if !allconst && (istopfunction(f, :+) || istopfunction(f, :-) || istopfunction(f, :*) ||
                     istopfunction(f, :(==)) || istopfunction(f, :!=) ||
                     istopfunction(f, :<=) || istopfunction(f, :>=) || istopfunction(f, :<) || istopfunction(f, :>) ||
                     istopfunction(f, :<<) || istopfunction(f, :>>))
        # it is almost useless to inline the op of when all the same type,
        # but highly worthwhile to inline promote of a constant
        length(argtypes) > 2 || return false
        t1 = widenconst(argtypes[2])
        all_same = true
        for i in 3:length(argtypes)
            if widenconst(argtypes[i]) !== t1
                all_same = false
                break
            end
        end
        return !all_same
    end
    return true
end

# This is a heuristic to avoid trying to const prop through complicated functions
# where we would spend a lot of time, but are probably unliekly to get an improved
# result anyway.
function const_prop_methodinstance_heuristic(interp::AbstractInterpreter, method::Method, mi::MethodInstance)
    # Peek at the inferred result for the function to determine if the optimizer
    # was able to cut it down to something simple (inlineable in particular).
    # If so, there's a good chance we might be able to const prop all the way
    # through and learn something new.
    code = get(code_cache(interp), mi, nothing)
    declared_inline = isdefined(method, :source) && ccall(:jl_ir_flag_inlineable, Bool, (Any,), method.source)
    cache_inlineable = declared_inline
    if isdefined(code, :inferred) && !cache_inlineable
        cache_inf = code.inferred
        if !(cache_inf === nothing)
            cache_src_inferred = ccall(:jl_ir_flag_inferred, Bool, (Any,), cache_inf)
            cache_src_inlineable = ccall(:jl_ir_flag_inlineable, Bool, (Any,), cache_inf)
            cache_inlineable = cache_src_inferred && cache_src_inlineable
        end
    end
    if !cache_inlineable
        return false
    end
    return true
end

end); end

# works within inter-procedural context
function CC.abstract_call_method(interp::JETInterpreter, method::Method, @nospecialize(sig), sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)
    ret = @invoke abstract_call_method(interp::AbstractInterpreter, method::Method, sig, sparams::SimpleVector, hardlimit::Bool, sv::InferenceState)

    update_reports!(interp, sv)

    return ret
end

function update_reports!(interp::JETInterpreter, sv::InferenceState)
    rs = interp.to_be_updated
    if !isempty(rs)
        vf = get_virtual_frame(sv)
        for r in rs
            pushfirst!(r.st, vf)
        end
        empty!(rs)
    end
end

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

# XXX and TODO this is a super coarse version of what Julia's type inference routine does
# for local function body (types of variables are computed as a fixed point of the abstract
# interpretation algorithm); `set_virtual_globalvar!` assumes global variable assignments
# happen sequentially and deterministically, which is obviously not always correct.
# Currently `set_virtual_globalvar!` only handles super simple branching, but ideally we
# want to implement a "global version" of the type inference routine which should handle all
# the control flows correctly
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
