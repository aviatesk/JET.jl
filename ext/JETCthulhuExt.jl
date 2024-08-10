module JETCthulhuExt

using JET: JET, InferenceErrorReport, VirtualFrame
using Cthulhu: Cthulhu, Node, Data, callstring
using Core: MethodInstance

const _emptybackedges = MethodInstance[]

struct CallFrames
    frames::Vector{VirtualFrame}
end

function Cthulhu.treelist(r::InferenceErrorReport)
    io = IOBuffer()
    cf = CallFrames(r.vst)
    printstyled(IOContext(io, :color=>true), r.sig.tt, color=:red)
    Cthulhu.treelist!(Node(Data{Union{MethodInstance,Type}}("runtime call to " * String(take!(io)), r.sig.tt)), io, cf, "", Base.IdSet{Union{MethodInstance,Nothing}}([nothing]))
end

Cthulhu.instance(cf::CallFrames) = isempty(cf.frames) ? nothing : cf.frames[end].linfo
Cthulhu.backedges(cf::CallFrames) = isempty(cf.frames) ? _emptybackedges : [cf.frames[end].linfo]
Cthulhu.nextnode(cf::CallFrames, ::MethodInstance) = CallFrames(cf.frames[1:end-1])

end
