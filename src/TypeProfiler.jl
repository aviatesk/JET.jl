module TypeProfiler

# export profile_file, profile_text

using Core: SimpleVector, svec,
            CodeInfo, MethodInstance, LineInfoNode, GotoNode, PiNode, PhiNode
using Core.Compiler: SSAValue, tmerge

include("types.jl")
include("utils.jl")
include("interpret.jl")
include("profile.jl")
# include("manipulate.jl")
# include("builtins.jl")

end
