module JETCthulhuExt

using JET: JET_LOADABLE
@static if JET_LOADABLE
    include("JETCthulhuExtBase.jl")
end

end
