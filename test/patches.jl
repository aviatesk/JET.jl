# NOTE: this file keeps patches for julia itself to make JET.jl test keep passing

# TODO remove this patch once https://github.com/aviatesk/JET.jl/issues/184 is resolved
@static if VERSION ≥ v"1.8.0-DEV.421"
    @eval Base mapreduce_empty(f, op, T) = _empty_reduce_error()
end

@static if VERSION < v"1.7.0-DEV.0"
    # take in https://github.com/JuliaLang/julia/pull/38832
    @eval Base function _split(str::AbstractString, splitter::F, limit::Integer, keepempty::Bool, strs::Vector) where F
        # Forcing specialization on `splitter` improves performance (roughly 30% decrease in runtime)
        # and prevents a major invalidation risk (1550 MethodInstances)
        i = 1 # firstindex(str)
        n = lastindex(str)::Int
        r = findfirst(splitter,str)::Union{Nothing,Int,UnitRange{Int}}
        if r !== nothing
            j, k = first(r), nextind(str,last(r))::Int
            while 0 < j <= n && length(strs) != limit-1
                if i < k
                    if keepempty || i < j
                        push!(strs, @inbounds SubString(str,i,prevind(str,j)::Int))
                    end
                    i = k
                end
                (k <= j) && (k = nextind(str,j)::Int)
                r = findnext(splitter,str,k)::Union{Nothing,Int,UnitRange{Int}}
                r === nothing && break
                j, k = first(r), nextind(str,last(r))::Int
            end
        end
        if keepempty || i <= ncodeunits(str)::Int
            push!(strs, @inbounds SubString(str,i))
        end
        return strs
    end
end
