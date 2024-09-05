using CassetteBase

abstract type AnalysisPass end
function getconstructor end
function getjetconfigs end

struct JETRuntimeError <: Exception
    mi::MethodInstance
    res::JETCallResult
end
function Base.showerror(io::IO, err::JETRuntimeError)
    n = length(get_reports(err.res))
    print(io, "JETRuntimeError raised by `$(err.res.source)`:")
    println(io)
    show(io, err.res)
end

function make_runtime_analysis_generator(selfname::Symbol, fargsname::Symbol)
    function runtime_analysis_generator(world::UInt, source::LineNumberNode, passtype, fargtypes)
        @nospecialize passtype fargtypes
        try
            return analyze_and_generate_ex(world, source, passtype, fargtypes,
                                           selfname, fargsname)
        catch err
            # internal error happened - return an expression to raise the special exception
            return generate_internalerr_ex(
                err, #=bt=#catch_backtrace(), #=context=#:runtime_analysis_generator, world, source,
                #=argnames=#Core.svec(selfname, fargsname), #=spnames=#Core.svec(),
                #=metadata=#(; world, source, passtype, fargtypes))
        end
    end
end

function analyze_and_generate_ex(world::UInt, source::LineNumberNode, passtype, fargtypes,
                                 selfname::Symbol, fargsname::Symbol, )
    @nospecialize passtype fargtypes
    tt = Base.to_tuple_type(fargtypes)
    match = Base._which(tt; raise=false, world)
    match === nothing && return nothing # method match failed – the fallback implementation will raise a proper MethodError
    mi = specialize_method(match)

    Analyzer = getconstructor(passtype)
    jetconfigs = getjetconfigs(passtype)
    analyzer = Analyzer(world; jetconfigs...)
    analyzer, result = analyze_method_instance!(analyzer, mi)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    src = lazy"$analyzername: $sig"
    res = JETCallResult(result, analyzer, src; jetconfigs...)
    if !isempty(get_reports(res))
        # JET found some problems - return an expression to raise it to the user
        throw_ex = :(throw($JETRuntimeError($mi, $res)))
        argnames = Core.svec(selfname, fargsname)
        return generate_lambda_ex(world, source, argnames, #=spnames=#Core.svec(), throw_ex)
    end

    src = retrieve_code_info(mi, world)
    src === nothing && return nothing # code generation failed - the fallback implementation will re-raise it
    return cassette_transform!(src, mi, length(fargtypes), selfname, fargsname)
end

macro analysispass(args...)
    isempty(args) && throw(ArgumentError("`@analysispass` expected more than one argument."))
    analyzertype = args[1]
    params = Expr(:parameters)
    append!(params.args, args[2:end])
    jetconfigs = Expr(:tuple, params)

    PassName = esc(gensym(string(analyzertype)))

    blk = quote
        let analyzertypetype = Core.Typeof($(esc(analyzertype)))
            if !(analyzertypetype <: Type{<:$(@__MODULE__).AbstractAnalyzer})
                throw(ArgumentError(
                    "`@analysispass` expected a subtype of `JET.AbstractAnalyzer`, but got object of `$analyzertypetype`."))
            end
        end

        struct $PassName <: $AnalysisPass end

        $(@__MODULE__).getconstructor(::Type{$PassName}) = $(esc(analyzertype))
        $(@__MODULE__).getjetconfigs(::Type{$PassName}) = $(esc(jetconfigs))

        @inline function (::$PassName)(f::Union{Core.Builtin,Core.IntrinsicFunction}, args...)
            @nospecialize f args
            return f(args...)
        end
        @inline function (self::$PassName)(::typeof(Core.Compiler.return_type), tt::DataType)
            # return Core.Compiler.return_type(self, tt)
            return Core.Compiler.return_type(tt)
        end
        @inline function (self::$PassName)(::typeof(Core.Compiler.return_type), f, tt::DataType)
            newtt = Base.signature_type(f, tt)
            # return Core.Compiler.return_type(self, newtt)
            return Core.Compiler.return_type(newtt)
        end
        @inline function (self::$PassName)(::typeof(Core._apply_iterate), iterate, f, args...)
            @nospecialize args
            return Core.Compiler._apply_iterate(iterate, self, (f,), args...)
        end

        function (pass::$PassName)(fargs...)
            $(Expr(:meta, :generated, make_runtime_analysis_generator(:pass, :fargs)))
            # also include a fallback implementation that will be used when this method
            # is dynamically dispatched with `!isdispatchtuple` signatures.
            return first(fargs)(Base.tail(fargs)...)
        end

        return $PassName()
    end

    return Expr(:toplevel, blk.args...)
end
