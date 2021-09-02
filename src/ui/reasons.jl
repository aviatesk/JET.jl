"""
    JET.reasons(results)

Construct a unique set of "reasons" for errors. Each reason is a unique file/line location, signature, and explanation.

`results` is most easily constructed from SnoopCompile's `report.(itrigs)`, where `itrigs` is a list of inference triggers.
See the SnoopCompile documentation for details.
"""
reasons(results::AbstractVector{<:JETCallResult}) = unique(reduce(vcat, filter(!isempty, reasons.(results)); init=Reason[]))
reasons(result::JETCallResult) = Reason.(get_reports(result))

struct Reason
    report::InferenceErrorReport
end

# A limited form of comparison so as to aggregate equivalent errors that were
# arrived at by different callgraphs
function Base.:(==)(a::Reason, b::Reason)
    a.report.msg == b.report.msg || return false
    af, bf = a.report.vst[end], b.report.vst[end]
    af.file == bf.file && af.line == bf.line || return false
    a.report.sig == b.report.sig
end

const reason_seed = Int === Int64 ? 0xfbdf9de4cc04c870 : 0x5e6fcd4e
function Base.hash(r::Reason, h::UInt)
    h = hash(reason_seed, h)
    h = hash(r.report.msg, h)
    f = r.report.vst[end]
    h = hash(f.file, h)
    h = hash(f.line)
    h = hash(r.report.sig, h)
end

function Base.show(io::IO, r::Reason)
    print_error_report(io, r.report)
    f = r.report.vst[end]
    print(io, f.file, ':', f.line)
end
