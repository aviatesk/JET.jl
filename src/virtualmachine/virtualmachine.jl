# NOTE: known limitations
# - macro including access to global variables:
#   TP will try to expand any macro, but since macros can access to any global object and so
#   any macro expansion including access to global variables will fail as far as TP avoids
#   evaluation of them but just profiles/preserves their types.
module VirtualMachine

import Base:
    Meta.isexpr

# TODO:
# - `module`
# - `using`, `import`, `__init__`
# - special case `include` call

"""
    transform_for_profiling!(mod::Module, ex::Expr)

Transform the given toplevel `ex::Expr` so that final output can be wrapped into a lambda
  function and profiled:
- extract toplevel "defintions" and directly evaluate them in a given `mod::Module`
- expand macros
- remove `const` annotations
"""
function transform_for_profiling!(mod::Module, ex::Expr)
    @assert ex.head === :toplevel
    walk_and_transform!(ex, Symbol[]) do x, scope
        # TODO: report errors in macro expansion
        if MacroTools.isexpr(x, :macrocall)
            x = macroexpand(mod, x)
        end

        return if istopleveldef(x, scope)
            leftover = Core.eval(mod, x)
            :($(leftover))
        elseif isexpr(x, :const)
            first(x.args)
        else
            x
        end
    end
end

function walk_and_transform!(f, x, scope)
    x = f(x, scope)
    x isa Expr || return x
    push!(scope, x.head)
    foreach(enumerate(x.args)) do (i, ex)
        x.args[i] = walk_and_transform!(f, ex, scope)
    end
    pop!(scope)
    return x
end

function istopleveldef(ex, scope)
    # always hoist
    isexpr(ex, :macro) && return true
    isexpr(ex, :abstract) && return true
    isexpr(ex, :struct) && return true
    isexpr(ex, :primitive) && return true

    !islocalscope(scope) && isfuncdef(ex) && return true # only hoist function when in really toplevel

    return false
end

function isfuncdef(ex)
    isexpr(ex, :function) && return true
    isexpr(ex, :(=)) && isexpr(first(ex.args), :call) && return true # short form

    return false
end

const LOCAL_SCOPES = (:let, :quote, :if, :try, :for, :while)
function islocalscope(scope)
    for s in scope
        s in LOCAL_SCOPES && return true
    end

    return false
end

end  # module VirtualMachine
