# NOTE: this file keeps patches for julia itself to make JET.jl test keep passing

# https://github.com/JuliaLang/julia/pull/42195
@static if VERSION < v"1.8.0-DEV.510"
    @eval Base isequal(x, y) = (x == y)::Bool # all `missing` cases are handled in missing.jl
end

# TODO remove this patch once https://github.com/aviatesk/JET.jl/issues/184 is resolved
@static if VERSION ≥ v"1.8.0-DEV.421"
    @eval Base mapreduce_empty(f, op, T) = _empty_reduce_error()
end
