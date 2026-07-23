module JETCthulhuExt

using JET: JET_AVAILABLE
@static if JET_AVAILABLE
    include("JETCthulhuExtBase.jl")
end

end
