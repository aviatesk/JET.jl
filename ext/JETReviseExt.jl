module JETReviseExt

using JET: JET_LOADABLE
@static if JET_LOADABLE
    include("JETReviseExtBase.jl")
end

end # module ReviseExt
