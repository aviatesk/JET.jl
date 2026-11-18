const DISTRIBUTED_PKGID = Base.PkgId(Base.UUID("8ba89e20-285c-5b6f-9357-94700520ee1b"), "Distributed")

# `Distributed.@everywhere [procs] ex` passes `ex` as data to `Core.eval(Main, ex)` on
# `procs`, so the analysis would never make its definitions. Return the code to analyze in
# place of such a call instead: `ex`, as if `procs` included the calling process, evaluated
# in the calling module rather than `Main`.
function desugar_everywhere_macrocall(state::InterpretationState, @nospecialize x)
    isexpr(x, :macrocall) && length(x.args) in (3, 4) || return nothing
    f = Base.invoke_in_world(state.world, resolve_global, state.context, first(x.args))
    f isa Function && nameof(f) === Symbol("@everywhere") || return nothing
    Base.PkgId(parentmodule(f)) == DISTRIBUTED_PKGID || return nothing
    ex = unquote_interpolations(x.args[end])
    # `@everywhere` runs the imports of `ex` first, so `ex` can use the macros they import
    return Expr(:toplevel, everywhere_imports!(Any[], ex)..., x.args[3:end-1]..., ex)
end

# Collect the imports of `ex` as `Distributed.extract_imports` does
function everywhere_imports!(imports::Vector{Any}, @nospecialize(ex))
    if isexpr(ex, (:import, :using))
        push!(imports, ex)
    elseif isexpr(ex, :let)
        everywhere_imports!(imports, ex.args[2])
    elseif isexpr(ex, (:toplevel, :block))
        for arg in ex.args
            everywhere_imports!(imports, arg)
        end
    end
    return imports
end

# `@everywhere` quotes its code `x`, so each `$y` in `x` splices in the value of `y` from the
# calling scope. Replace such `$y` with `y`, but keep the `$` of quotes nested in `x`, e.g.
# `f() = $y + :($z)` becomes `f() = y + :($z)`.
function unquote_interpolations(@nospecialize(x), depth::Int=0)
    x isa Expr || return x
    if isexpr(x, :$, 1)
        depth == 0 && return x.args[1]
        return Expr(:$, unquote_interpolations(x.args[1], depth - 1))
    end
    depth = isexpr(x, :quote) ? depth + 1 : depth
    return Expr(x.head, Any[unquote_interpolations(arg, depth) for arg in x.args]...)
end
