module JETCthulhuExt

using JET: JET, InferenceErrorReport, VirtualFrame, PrintConfig, print_signature
using Cthulhu: Cthulhu, Node, Data, callstring
using Core: MethodInstance

const _emptybackedges = MethodInstance[]

struct CallFrames
    frames::Vector{VirtualFrame}
end

function Cthulhu.treelist(r::InferenceErrorReport)
    io = IOBuffer()
    cf = CallFrames(r.vst)
    print_signature(IOContext(io, :color=>true), r.sig, PrintConfig())
    # printstyled(IOContext(io, :color=>true), r.sig.tt, color=:red)
    Cthulhu.treelist!(Node(Data{Union{MethodInstance,Type}}("runtime dispatch to " * String(take!(io)), r.sig.tt)), io, cf, "", Base.IdSet{Union{MethodInstance,Nothing}}([nothing]))
end

Cthulhu.instance(cf::CallFrames) = isempty(cf.frames) ? nothing : cf.frames[end].linfo
Cthulhu.backedges(cf::CallFrames) = isempty(cf.frames) ? _emptybackedges : [cf.frames[end].linfo]
Cthulhu.nextnode(cf::CallFrames, ::MethodInstance) = CallFrames(cf.frames[1:end-1])

end
