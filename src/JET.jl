module JET

function __init__()
    @warn """
JET cannot be used on Julia $(Base.VERSION).
If you're depending on JET, it may be best to avoid loading the package except on supported versions of Julia, for example:

    if Base.VERSION >= v"1.6"
        using JET
    end
"""
end

end
