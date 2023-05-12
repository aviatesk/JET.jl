using Core.IR
using Base: to_tuple_type

abstract type AnalysisPass end
function get_constructor end
function get_jetconfigs end

# JuliaLang/julia#48611: world age is exposed to generated functions, and should be used
const has_generated_worlds = let
    v = VERSION ≥ v"1.10.0-DEV.873"
    v && @assert fieldcount(Core.GeneratedFunctionStub) == 3
    v
end

function analyze_and_generate(world::UInt, source::LineNumberNode, passtype, fargtypes)
    tt = to_tuple_type(fargtypes)
    match = _which(tt; raise=false, world)
    match === nothing && return nothing

    mi = specialize_method(match)
    Analyzer = get_constructor(passtype)
    jetconfigs = get_jetconfigs(passtype)
    analyzer = Analyzer(world; jetconfigs...)
    analyzer, result = analyze_method_instance!(analyzer, mi)
    analyzername = nameof(typeof(analyzer))
    sig = LazyPrinter(io::IO->Base.show_tuple_as_call(io, Symbol(""), tt))
    src = lazy"$analyzername: $sig"
    res = JETCallResult(result, analyzer, src; jetconfigs...)

    isempty(get_reports(res)) || return generate_report_error_ex(world, source, mi, res)

    src = @static if has_generated_worlds
        copy(retrieve_code_info(mi, world)::CodeInfo)
    else
        copy(retrieve_code_info(mi)::CodeInfo)
    end
    analysispass_transform!(src, mi, length(fargtypes))
    return src
end

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

function generate_report_error_ex(world::UInt, source::LineNumberNode,
                                  mi::MethodInstance, res::JETCallResult)
    args = Core.svec(:pass, :fargs)
    sparams = Core.svec()
    ex = :(throw($JETRuntimeError($mi, $res)))
    return generate_lambda_ex(world, source, args, sparams, ex)
end

function generate_lambda_ex(world::UInt, source::LineNumberNode,
                            args::SimpleVector, sparams::SimpleVector, body::Expr)
    stub = Core.GeneratedFunctionStub(identity, args, sparams)
    return stub(world, source, body)
end

# TODO share this code with CassetteOverlay
function analysispass_transform!(src::CodeInfo, mi::MethodInstance, nargs::Int)
    method = mi.def::Method
    mnargs = Int(method.nargs)

    src.slotnames = Symbol[Symbol("#self#"), :fargs, src.slotnames[mnargs+1:end]...]
    src.slotflags = UInt8[ 0x00,             0x00,   src.slotflags[mnargs+1:end]...]

    code = src.code
    fargsslot = SlotNumber(2)
    precode = Any[]
    local ssaid = 0
    for i = 1:mnargs
        if method.isva && i == mnargs
            args = map(i:nargs) do j
                push!(precode, Expr(:call, getfield, fargsslot, j))
                ssaid += 1
                return SSAValue(ssaid)
            end
            push!(precode, Expr(:call, tuple, args...))
        else
            push!(precode, Expr(:call, getfield, fargsslot, i))
        end
        ssaid += 1
    end
    prepend!(code, precode)
    prepend!(src.codelocs, [0 for i = 1:ssaid])
    prepend!(src.ssaflags, [0x00 for i = 1:ssaid])
    src.ssavaluetypes += ssaid

    function map_slot_number(slot::Int)
        @assert slot ≥ 1
        if 1 ≤ slot ≤ mnargs
            if method.isva && slot == mnargs
                return SSAValue(ssaid)
            else
                return SSAValue(slot)
            end
        else
            return SlotNumber(slot - mnargs + 2)
        end
    end
    map_ssa_value(id::Int) = id + ssaid
    for i = (ssaid+1:length(code))
        code[i] = transform_stmt(code[i], map_slot_number, map_ssa_value, mi.def.sig, mi.sparam_vals)
    end

    src.edges = MethodInstance[mi]
    src.method_for_inference_limit_heuristics = method

    return src
end

function transform_stmt(@nospecialize(x), map_slot_number, map_ssa_value, @nospecialize(spsig), sparams::SimpleVector)
    transform(@nospecialize x′) = transform_stmt(x′, map_slot_number, map_ssa_value, spsig, sparams)

    if isa(x, Expr)
        head = x.head
        if head === :call
            return Expr(:call, SlotNumber(1), map(transform, x.args)...)
        elseif head === :foreigncall
            # first argument of :foreigncall is a magic tuple and should be preserved
            arg2 = ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), x.args[2], spsig, sparams)
            arg3 = Core.svec(Any[
                    ccall(:jl_instantiate_type_in_env, Any, (Any, Any, Ptr{Any}), argt, spsig, sparams)
                    for argt in x.args[3]::SimpleVector ]...)
            return Expr(:foreigncall, x.args[1], arg2, arg3, map(transform, x.args[4:end])...)
        elseif head === :enter
            return Expr(:enter, map_ssa_value(x.args[1]::Int))
        elseif head === :static_parameter
            return sparams[x.args[1]::Int]
        end
        return Expr(x.head, map(transform, x.args)...)
    elseif isa(x, GotoNode)
        return GotoNode(map_ssa_value(x.label))
    elseif isa(x, GotoIfNot)
        return GotoIfNot(transform(x.cond), map_ssa_value(x.dest))
    elseif isa(x, ReturnNode)
        return ReturnNode(transform(x.val))
    elseif isa(x, SlotNumber)
        return map_slot_number(x.id)
    elseif isa(x, NewvarNode)
        return NewvarNode(map_slot_number(x.slot.id))
    elseif isa(x, SSAValue)
        return SSAValue(map_ssa_value(x.id))
    else
        return x
    end
end

function pass_generator(world::UInt, source::LineNumberNode, pass, fargs)
    src = analyze_and_generate(world, source, pass, fargs)
    if src === nothing
        # code generation failed – make it raise a proper MethodError
        stub = Core.GeneratedFunctionStub(identity, Core.svec(:pass, :fargs), Core.svec())
        return stub(world, source, :(return first(fargs)(Base.tail(fargs)...)))
    end
    return src
end

"""
    @analysispass Analyzer [jetconfigs...]

TODO docs.
"""
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

        $(@__MODULE__).get_constructor(::Type{$PassName}) = $(esc(analyzertype))
        $(@__MODULE__).get_jetconfigs(::Type{$PassName}) = $(esc(jetconfigs))

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

        @static if $has_generated_worlds
            function (pass::$PassName)(fargs...)
                $(Expr(:meta, :generated_only))
                $(Expr(:meta, :generated, pass_generator))
            end
        else
            @generated function (pass::$PassName)($(esc(:fargs))...)
                world = Base.get_world_counter()
                source = LineNumberNode(@__LINE__, @__FILE__)
                src = $analyze_and_generate(world, pass, fargs)
                if src === nothing
                    # a code generation failed – make it raise a proper MethodError
                    return :(first(fargs)(Base.tail(fargs)...))
                end
                return src
            end
        end

        return $PassName()
    end

    return Expr(:toplevel, blk.args...)
end
