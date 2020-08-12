# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/e42045c39f6363aa5035da5c320587b5264ca644/src/utils.jl

moduleof(frame::Frame) = frame.mod

pc_expr(codeinfo::CodeInfo, pc) = codeinfo.code[pc]
pc_expr(frame::Frame, pc) = pc_expr(frame.codeinfo, pc)
pc_expr(frame::Frame) = pc_expr(frame, frame.pc)

function is_type_definition(stmt)
    if isa(stmt, Expr)
        head = stmt.head
        return head === :struct_type || head === :abstract_type || head === :primitive_type
    end
    return false
end

function find_used(codeinfo::CodeInfo)
    used = BitSet()
    for stmt in codeinfo.code
        scan_ssa_use!(used, stmt)
        if is_type_definition(stmt)
            # these are missed by Core.Compiler.userefs, see https://github.com/JuliaLang/julia/pull/30936
            for a in stmt.args
                scan_ssa_use!(used, a)
            end
        end
    end
    return used
end

function scan_ssa_use!(used::BitSet, @nospecialize(stmt))
    if isa(stmt, SSAValue)
        push!(used, stmt.id)
    end
    iter = Core.Compiler.userefs(stmt)
    iterval = Core.Compiler.iterate(iter)
    while iterval !== nothing
        useref, state = iterval
        val = Core.Compiler.getindex(useref)
        if isa(val, SSAValue)
            push!(used, val.id)
        end
        iterval = Core.Compiler.iterate(iter, state)
    end
end

@static if isdefined(Core, :ReturnNode)
    is_ReturnNode(@nospecialize(node)) = isa(node, Core.ReturnNode)
    is_return(@nospecialize(node)) = is_ReturnNode(node)
else
    is_ReturnNode(@nospecialize(node)) = false
    is_return(@nospecialize(node)) = isexpr(node, :return)
end

"""
    is_global_ref(g, mod, name)

Tests whether `g` is equal to `GlobalRef(mod, name)`.
"""
is_global_ref(@nospecialize(g), mod::Module, name::Symbol) = isa(g, GlobalRef) && g.mod === mod && g.name == name

"""
    is_doc_expr(ex)

Test whether expression `ex` is a `@doc` expression.
"""
function is_doc_expr(@nospecialize(ex))
    docsym = Symbol("@doc")
    if isexpr(ex, :macrocall)
        a = ex.args[1]
        is_global_ref(a, Core, docsym) && return true
        isa(a, Symbol) && a == docsym && return true
        if isexpr(a, :.)
            mod, name = (a::Expr).args[1], (a::Expr).args[2]
            return mod === :Core && isa(name, QuoteNode) && name.value == docsym
        end
    end
    return false
end
