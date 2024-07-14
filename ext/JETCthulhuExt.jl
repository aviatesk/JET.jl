module JETCthulhuExt

using JET: JET, RuntimeDispatchReport, VirtualFrame
using Cthulhu: Cthulhu, Node, Data, callstring
using Core: MethodInstance

const _emptybackedges = MethodInstance[]

struct CallFrames
    frames::Vector{VirtualFrame}
end

function Cthulhu.treelist(r::RuntimeDispatchReport)
    io = IOBuffer()
    cf = CallFrames(r.vst)
    frame = r.vst[end]
    str = callstring(io, frame.linfo)
    Cthulhu.treelist!(Node(Data(str, frame.linfo)), io, cf, "", Base.IdSet{Union{MethodInstance,Nothing}}([nothing]))
end

Cthulhu.instance(cf::CallFrames) = isempty(cf.frames) ? nothing : cf.frames[end].linfo
Cthulhu.backedges(cf::CallFrames) = isempty(cf.frames) ? _emptybackedges : [cf.frames[end].linfo]
Cthulhu.nextnode(cf::CallFrames, ::MethodInstance) = CallFrames(cf.frames[1:end-1])

end
