"""
    @invoke f(arg::T, ...; kwargs...)

provides a convenient way to call [`invoke`](@ref);
this could be used to call down to `NativeInterpreter`'s abstract interpretation method of
  `f` while passing `JETInterpreter` so that subsequent calls of abstract interpretation
  functions overloaded against `JETInterpreter` can be called from the native method of `f`.

e.g. calls down to `NativeInterpreter`'s `abstract_call_gf_by_type` method:
```julia
@invoke abstract_call_gf_by_type(interp::AbstractInterpreter, f, argtypes::Vector{Any}, atype, sv::InferenceState,
                                 max_methods::Int)
```
"""
macro invoke(ex)
    f = first(ex.args)
    argtypes = []
    args = []
    kwargs = []
    for x in ex.args[2:end]
        if isexpr(x, :parameters)
            append!(kwargs, x.args)
        elseif isexpr(x, :kw)
            push!(kwargs, x)
        else
            arg, argtype = isexpr(x, :(::)) ? (x.args...,) : (x, Any)
            push!(args, arg)
            push!(argtypes, argtype)
        end
    end
    return if isempty(kwargs)
        :(invoke($(f), Tuple{$(argtypes...)}, $(args...))) # might not be necessary
    else
        :(invoke($(f), Tuple{$(argtypes...)}, $(args...); $(kwargs...)))
    end |> esc
end

is_constant_propagated(frame) = CC.any(frame.result.overridden_by_const)

istoplevel(sv) = sv.linfo.def == toplevel

mutable struct VirtualGlobalVariable
    # actual profiled type
    t::Any
    # keeps `id` of `JETInterpreter` that defined this
    id::Symbol
    # `Symbol` of a dummy generic function that generates dummy backedge (i.e. `li`)
    edge_sym::Symbol
    # dummy backedge, which will be invalidated on update of `t`
    li::MethodInstance
    # whether this virtual global variable is defined as constant or not
    # TODO: use this field
    isconst::Bool

    function VirtualGlobalVariable(@nospecialize(t),
                                   id::Symbol,
                                   edge_sym::Symbol,
                                   li::MethodInstance,
                                   isconst::Bool = true,
                                   )
        return new(t, id, edge_sym, li, isconst)
    end
end

# overloads abstractinterpretation.jl
# -----------------------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/abstractinterpretation.jl

# the aim of this horrible monkey patch is:
# 1. report `NoMethodErrorReport` on empty method signature matching
# 2. keep inference on non-concrete call sites in toplevel frame created by `virtual_process!`
# 3. in order to collect as much error points as possible, don't bail out if the current return
#    type grows up to `Any`; of course it slows down inference performance, but hopefully it
#    stays to be "practical" speed (because the number of matching methods are limited beforehand)
# the bail out logics are hard-coded within the original `abstract_call_gf_by_type` method,
# and so we need to overload it with `JETInterpreter`, adding the above patch points
# NOTE: the overloaded version is evaluated in `Core.Compiler` so that we don't need miscellaneous imports
push_inithook!() do; Core.eval(CC, quote

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
            #=== JET.jl monkey-patch start ===#
            info = MethodMatchInfo(matches)
            if $(is_empty_match)(info)
                # report `NoMethodErrorReport` for union-split signatures
                $(add_remark!)(interp, sv, $(NoMethodErrorReport)(interp, sv, true, atype))
            end
            push!(infos, info)
            #=== JET.jl monkey-patch end ===#
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
        #=== JET.jl monkey-patch start ===#
        if $(is_empty_match)(info)
            # report `NoMethodErrorReport` for this call signature
            $(add_remark!)(interp, sv, $(NoMethodErrorReport)(interp, sv, false, atype))
        end
        #=== JET.jl monkey-patch end ===#
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

    for i in 1:napplicable
        match = applicable[i]::MethodMatch
        method = match.method
        sig = match.spec_types
        if istoplevel && !isdispatchtuple(sig) && begin
                #=== JET.jl monkey-patch start ===#
                # keep going for "our" toplevel frame
                !$(istoplevel)(sv)
                #=== JET.jl monkey-patch end ===#
            end
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
                #=== JET.jl monkey-patch start ===#
                # keep going and collect as much error reports as possible
                # this_rt === Any && break
                #=== JET.jl monkey-patch end ===#
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
        #=== JET.jl monkey-patch start ===#
        # keep going and collect as much error reports as possible
        # rettype === Any && break
        #=== JET.jl monkey-patch end ===#
    end
    # # try constant propagation if only 1 method is inferred to non-Bottom
    # # this is in preparation for inlining, or improving the return result
    is_unused = call_result_unused(sv)
    if nonbot > 0 && seen == napplicable && (!edgecycle || !is_unused) && isa(rettype, Type) && InferenceParams(interp).ipo_constant_propagation
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
    if !(rettype === Any) # adding a new method couldn't refine (widen) this type
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
    return CallMeta(rettype, info)
end

end); end # push_inithook!() do; Core.eval(Core.Compiler, quote

function is_empty_match(info)
    res = info.results
    isa(res, MethodLookupResult) || return false # when does this happen ?
    return isempty(res.matches)
end

function CC.abstract_eval_special_value(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if isa(e, Symbol)
            e = GlobalRef(sv.mod, e)
        end
    end

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
        # report access to undefined global variable
        mod, sym = e.mod, e.name
        if isdefined(mod, sym)
            # FIXME: this might produce wrong result if the current frame is cached and the
            # global variable later be updated (both actually or abstractly, anyway);
            # 1. if we have established a way to convert a already-existing global variable
            #    to virtual global variable we can do that here and add backedge
            # 2. `NativeInterpreter` just returns `Any` for this case, so it might be okay
            #    if JET just follows that and give up profiling on non-constant global variables ...
            val = getfield(mod, sym)
            @debug "propagating non-constant global variable as constant" mod sym val
            ret = Const(val)
        else
            add_remark!(interp, sv, GlobalUndefVarErrorReport(interp, sv, mod, sym))
            # `ret` here should be annotated as `Any` by `NativeInterpreter`, but we want to
            # be more conservative and change it to `Bottom` and suppress any further abstract
            # interpretation with this
            ret = Bottom
        end
    end

    return ret
end

function CC.abstract_eval_value(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    ret = @invoke abstract_eval_value(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # report non-boolean condition error
    stmt = get_cur_stmt(sv)
    if isa(stmt, GotoIfNot)
        t = widenconst(ret)
        if t !== Bottom && !⊑(Bool, t)
            add_remark!(interp, sv, NonBooleanCondErrorReport(interp, sv, t))
            ret = Bottom
        end
    end

    return ret
end

function CC.abstract_eval_statement(interp::JETInterpreter, @nospecialize(e), vtypes::VarTable, sv::InferenceState)
    if istoplevel(sv)
        if interp.concretized[get_cur_pc(sv)]
            return Any # bail out if it has been interpreted
        end
    end

    ret = @invoke abstract_eval_statement(interp::AbstractInterpreter, e, vtypes::VarTable, sv::InferenceState)

    # assign virtual global variable
    lhs = get_global_assignment_lhs(sv)
    isa(lhs, GlobalRef) && set_virtual_globalvar!(interp, lhs.mod, lhs.name, ret)

    return ret
end

function get_global_assignment_lhs(sv::InferenceState)
    stmt = get_cur_stmt(sv)

    isexpr(stmt, :(=)) || return nothing
    lhs = first(stmt.args)

    isa(lhs, GlobalRef) && return lhs
    istoplevel(sv) && isa(lhs, Symbol) && return GlobalRef(sv.mod, lhs)

    return nothing
end

function set_virtual_globalvar!(interp, mod, name, @nospecialize(t))
    local update::Bool = false
    id = get_id(interp)

    prev_t, prev_id, (edge_sym, li) = if isdefined(mod, name)
        val = getfield(mod, name)
        if isa(val, VirtualGlobalVariable)
            # update previously-defined virtual global variable
            update = true
            val.t, val.id, (val.edge_sym, val.li)
        else
            @warn "JET.jl can't trace updates of global variable that already have values" mod name val
            return
        end
    else
        # define new virtual global variable
        Bottom, id, gen_dummy_backedge(mod)
    end

    # at this point undefined slots may still be annotated as `NOT_FOUND` (e.g. when there is
    # undefined slot, etc.), which can't be used as abstract value for later profiling,
    # so replace it with Bottom
    if t === NOT_FOUND
        t = Bottom
    end

    if id === prev_id
        # if the previous virtual global variable assignment happened in the same inference process,
        # JET needs to perform type merge, otherwise "just update"s it
        t = tmerge(prev_t, t)
    else
        # invalidate the dummy backedge that is bound to this virtual global variable,
        # so that depending `MethodInstance` will run fresh type inference on the next hit
        li = force_invalidate!(mod, edge_sym)
    end

    return if update
        Core.eval(mod, quote
            local name = $(name)
            name.t = $(t)
            name.id = $(QuoteNode(id))
            name.edge_sym = $(QuoteNode(edge_sym))
            name.li = $(li)
            name
        end)
    else
        vgv = VirtualGlobalVariable(t, id, edge_sym, li)
        Core.eval(mod, :(const $(name) = $(vgv)))
    end::VirtualGlobalVariable
end

function gen_dummy_backedge(mod)
    @gensym edge_sym
    return edge_sym, force_invalidate!(mod, edge_sym) # just generate dummy `MethodInstance` to be invalidated
end

# TODO: find a more fine-grained way to do this ? re-evaluating an entire function seems to be over-kill for this
function force_invalidate!(mod, edge_sym)
    λ = Core.eval(mod, :($(edge_sym)() = return))::Function
    m = first(methods(λ))
    return specialize_method(m, Tuple{typeof(λ)}, Core.svec())::MethodInstance
end

# overloads typeinfer.jl
# ----------------------
# ref: https://github.com/JuliaLang/julia/blob/26c79b2e74d35434737bc33bc09d2e0f6e27372b/base/compiler/typeinfer.jl

# in this overload we can work on `CodeInfo` (and also `InferenceState`) where type inference
# (and maybe also optimization) already ran on
function CC.typeinf(interp::JETInterpreter, frame::InferenceState)
    # some methods like `getproperty` can't propagate concrete types without actual values,
    # and for those cases constant propagation usually plays a somewhat critical role that
    # "overwrite"s the previously-inferred lousy inference result;
    # JET.jl also needs to follow the native interpreter's result "overwrite" logic
    # to reduce false positive reports from lousy typed profiling, and so here we will throw
    # away previously-collected error reports from frames that are lineage of this frame if
    # this frame is being re-inferred with constant-prop'ed inputs
    #
    # NOTE:
    # - constant propagation only happens after inference with abstract values (i.e. just
    #   using types)
    # - maybe upcoming change for the native constant propagation logic:
    #   https://github.com/JuliaLang/julia/blob/a108d6cb8fdc7924fe2b8d831251142386cb6525/base/compiler/abstractinterpretation.jl#L153
    # - why not always throw away lineage reports when constant propagation happens ?
    #   * constant prop' doesn't seem to happen always, especially inferring on cached frames,
    #     and then error reports can be different for uncached/cached frames, which would be
    #     super confusing
    #   * discussion: we may want to have reports for code that is cut off by constant prop'
    if is_constant_propagated(frame)
        linfo = frame.linfo
        def = linfo.def
        if isa(def, Method)
            # throw away previously-collected error reports
            def.name in CONSTANT_PROP_METHODS && filter!(!Fix1(is_lineage, linfo), interp.reports)
        end
    end

    # @info "before typeinf" frame.linfo => ret

    ret = @invoke typeinf(interp::AbstractInterpreter, frame::InferenceState)

    # @info "after typeinf" frame.linfo => ret

    # report (local) undef var error
    # this only works when optimization is enabled, just because `:throw_undef_if_not` and
    # `:(unreachable)` are introduced by `optimize`
    stmts = frame.src.code
    for (idx, stmt) in enumerate(stmts)
        if isa(stmt, Expr) && stmt.head === :throw_undef_if_not
            sym, _ = stmt.args
            next_idx = idx + 1
            if checkbounds(Bool, stmts, next_idx) && is_unreachable(@inbounds stmts[next_idx])
                # the optimization so far has found this statement is never reachable;
                # JET reports it since it will invoke undef var error at runtime, or will just
                # be dead code otherwise

                add_remark!(interp, frame, LocalUndefVarErrorReport(interp, frame, sym))
            # else
                # by excluding this pass, JET accepts some false negatives (i.e. don't report
                # those that may actually happen on execution)
            end
        end
    end

    # report `throw` calls "appropriately" by simple inter-frame analysis
    # the basic stance here is really conservative so we don't report them unless they
    # will be inevitably called and won't be caught by `try/catch` in frame at any level
    # NOTE:
    # this is better to happen here because constant propagation can reduce the chance of
    # false negative reports by excluding unreachable control flows
    if get_result(frame) === Bottom
        # report `throw`s only if there is no circumvent pass, which is represented by
        # `Bottom`-annotated return type inference with non-empty `throw` blocks
        throw_calls = istoplevel(frame) ?
                      filter(is_throw_call_in_toplevel, frame.src.code) :
                      filter(is_throw_call′, frame.src.code)
        if !isempty(throw_calls)
            push!(interp.exception_reports, length(interp.reports) => ExceptionReport(interp, frame, throw_calls))
        end
    end

    return ret
end

const CONSTANT_PROP_METHODS = Set((:getproperty, :setproperty!))

is_unreachable(@nospecialize(_)) = false
is_unreachable(rn::ReturnNode)   = !isdefined(rn, :val)

is_throw_call′(@nospecialize(_)) = false
is_throw_call′(e::Expr)          = is_throw_call(e)

is_throw_call_in_toplevel(@nospecialize(_)) = false
is_throw_call_in_toplevel(stmt::Expr)       = isexpr(stmt, :call) && first(stmt.args) === :throw

# entry
# -----

function profile_frame!(interp::JETInterpreter, frame::InferenceState)
    typeinf(interp, frame)

    # if return type is `Bottom`-annotated for this frame, this may mean some `throw`(s)
    # aren't caught by at any level and get propagated here, or there're other critical
    # inference error found
    if get_result(frame) === Bottom
        # let's report report `ExceptionReport`s only if there is no other error reported
        # TODO: change behaviour according to severity of collected report, e.g. don't take
        # into account `NativeRemark`s, etc
        isempty(interp.reports) && append!(interp.reports, last.(interp.exception_reports))

        # # just append collected `ExceptionReport`s
        # for (i, (idx, report)) in enumerate(interp.exception_reports)
        #     insert!(interp.reports, idx + i, report)
        # end
    end

    return interp, frame
end
