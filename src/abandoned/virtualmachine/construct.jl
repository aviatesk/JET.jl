# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/e42045c39f6363aa5035da5c320587b5264ca644/src/construct.jl

"""
    frame = prepare_thunk(mod::Module, expr::Expr)

Prepare `expr` for evaluation in `mod`. `expr` should be a "straightforward" expression,
one that does not require special top-level handling (see [`JuliaInterpreter.split_expressions`](@ref)).
"""
function prepare_thunk(mod::Module, thunk::Expr, recursive::Bool = false; eval::Bool = true)
    if isexpr(thunk, :thunk)
        codeinfo = first(thunk.args)
        return Frame(Dict(
            # XXX: maybe different interpreter needed for each virtual method call ? (i.e. world age needs to be updated)
            :interp => Profiler.TPInterpreter(),
            :mod => mod,
            :codeinfo => codeinfo,
            :pc => 1,
            :used => find_used(codeinfo),
            :ssavaluetypes => similar(codeinfo.code, ProfiledType),
            :slottypes => similar(codeinfo.slotflags, ProfiledType),
            :reports => TypeProfiler.ErrorReport[],
        ))
    elseif isexpr(thunk, :error) || isexpr(thunk, :incomplete)
        # TODO: report
        error("lowering returned an error, ", thunk)
    elseif recursive
        error("inspect this case")
        # thunk = Meta.lower(mod, thunk)
        # if !isexpr(thunk, :thunk)
        #     # If on 2nd attempt to lower it's still an Expr, just evaluate it
        #     eval && Core.eval(mod, thunk)
        #     return nothing
        # end
        # framecode = FrameCode(mod, thunk.args[1])
    else
        if (lwr = Meta.lower(mod, thunk)) isa Expr
            return prepare_thunk(mod, lwr, true; eval = eval)
        end
        return nothing
    end
end
prepare_thunk((mod, ex)::Tuple{Module,Expr}) = prepare_thunk(mod, ex)

"""
    modexs, docexprs = split_expressions(mod::Module, expr::Expr; extract_docexprs=false)

Break `expr` into a list `modexs` of sequential blocks. This is often needed when `expr`
needs to be evaluated at top level.

`modexs[i]` is a `(mod::Module, ex::Expr)` tuple, where `ex` is to be evaluated in `mod`.

# Toplevel evaluation

For code that defines new structs, new methods, or new macros, it can be important to evaluate
these expressions carefully:

    stack = Frame[]
    for modex in modexs    # or use `for (mod, ex) in modexs` to split the tuple
        frame = JuliaInterpreter.prepare_thunk(modex)
        while true
            JuliaInterpreter.through_methoddef_or_done!(stack, frame) === nothing && break
        end
    end

The `while` loop here deserves some explanation. Occasionally, a frame may define new methods
(e.g., anonymous or local functions) and then call those methods. In such cases, running
the entire frame as a single block (e.g., with [`JuliaInterpreter.finish_and_return!`](@ref)
can trigger "method is too new..." errors. Instead, the approach above runs each frame,
but returns to the caller after any new method is defined. When this loop is running at
top level (e.g., in the REPL), this allows the world age to update and thus avoid
"method is too new..." errors.

Putting the above nested loop inside a function defeats its purpose, because inside a
compiled function the world age will not update. If necessary, use the following strategy:

    Core.eval(somemodule, Expr(:toplevel, quote
        body
    ))

where `body` contains the nested loop, plus any preparatory statements required to make the
necessary variables available at top level in `somemodule`.
"""
function split_expressions(mod::Module, expr::Expr; filename = nothing, kwargs...)
    modexs = Tuple{Module,Expr}[]
    docexprs = Dict{Module,Vector{Expr}}()
    if filename === nothing
        # On Julia 1.2+, the first line of a :toplevel expr may contain line info
        if length(expr.args) >= 1 && isa(expr.args[1], LineNumberNode)
            filename = expr.args[1].file
        else
            filename = "toplevel"
        end
    end
    return split_expressions!(modexs, docexprs, mod, expr; filename, kwargs...)
end

split_expressions!(modexs, docexprs, mod::Module, ex::Expr; kwargs...) =
    split_expressions!(modexs, docexprs, Expr(:block), mod, ex; kwargs...)

function split_expressions!(modexs, docexprs, lex::Expr, mod::Module, ex::Expr; extract_docexprs = false, filename = "toplevel")
    # lex is the expression we'll lower; it will accumulate LineNumberNodes and a
    # single top-level expression. We split blocks, module defs, etc.
    if ex.head === :toplevel || ex.head === :block
        split_expressions!(modexs, docexprs, lex, mod, ex.args; extract_docexprs, filename)
    elseif ex.head === :module
        newname = ex.args[2]::Symbol
        if isdefined(mod, newname)
            newmod = getfield(mod, newname)
            newmod isa Module || throw(ErrorException("invalid redefinition of constant $(newname)"))
        else
            # FIXME: don't evaluated into actual module
            if (id = Base.identify_package(mod, String(newname))) !== nothing && haskey(Base.loaded_modules, id)
                newmod = Base.root_module(id)
            else
                newmod = Core.eval(mod, :(module $newname end))
            end
        end
        split_expressions!(modexs, docexprs, lex, newmod, ex.args[3]; extract_docexprs, filename)
    elseif extract_docexprs && is_doc_expr(ex) && length(ex.args) >= 4
        body = ex.args[4]
        if isa(body, Expr) && body.head != :call
            split_expressions!(modexs, docexprs, lex, mod, body; extract_docexprs, filename)
        end
        docexs = get(docexprs, mod, nothing)
        if docexs === nothing
            docexs = docexprs[mod] = Expr[]
        end
        if isexpr(body, :module)
            # If it's a module expression, don't include the entire expression, just document the module itself.
            excopy = Expr(ex.head, ex.args[1], ex.args[2], ex.args[3])
            push!(excopy.args, body.args[2])
            if length(ex.args) > 4
                append!(excopy.args, ex.args[5:end])   # there should only be a 5th, but just for robustness
            end
            push!(docexs, excopy)
        else
            push!(docexs, ex)
        end
    else
        if isempty(lex.args) || (isexpr(ex, :global) && all(item->isa(item, LineNumberNode), lex.args))
            push!(modexs, (mod, copy(ex)))
        else
            push!(lex.args, ex)
            push!(modexs, (mod, copy(lex)))
            empty!(lex.args)
        end
    end
    return modexs, docexprs
end

function split_expressions!(frames, docexprs, lex::Expr, mod::Module, args::Vector{Any}; filename = "toplevel", kwargs...)
    for a in args
        if isa(a, Expr)
            split_expressions!(frames, docexprs, lex, mod, a; filename, kwargs...)
        elseif isa(a, LineNumberNode)
            if a.file === nothing  # happens with toplevel expressions on Julia 1.2
                push!(lex.args, LineNumberNode(a.line, Symbol(filename)))
            else
                push!(lex.args, a)
            end
        else
            push!(lex.args, a)
        end
    end
end
