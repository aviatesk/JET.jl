# report cache
# ------------

struct InferenceReportCache{T<:InferenceErrorReport}
    st::ViewedVirtualStackTrace
    msg::String
    sig::Vector{Any}
    lineage::Lineage
    args::NTuple{N, Any} where N # additional field that keeps information specific to `T`
end

const TPCACHE = IdDict{MethodInstance,Pair{Symbol,Vector{InferenceReportCache}}}()

is_lineage(linfo::MethodInstance, cached_report::InferenceReportCache) =
    is_lineage(linfo, cached_report.lineage)

get_id(interp::TPInterpreter) = interp.id

function restore_cached_report!(mi, cache::InferenceReportCache{T}, interp) where {T<:InferenceErrorReport}
    report = _restore_cached_report!(mi, T, cache, interp)
    push!(interp.reports, report)
end

function restore_cached_report!(mi, cache::InferenceReportCache{ExceptionReport}, interp)
    report = _restore_cached_report!(mi, ExceptionReport, cache, interp)
    push!(interp.exception_reports, length(interp.reports) => report)
end

function _restore_cached_report!(mi, T, cache, interp)
    sv = get_current_frame(interp)

    # reconstruct virtual stack trace and lineage for this cached report
    msg = cache.msg
    sig = cache.sig
    st = collect(cache.st)
    lineage = Lineage(sf.linfo for sf in st)
    cache_report! = gen_report_cacher(st, msg, sig, lineage, T, interp, #= dead arg =# sv, cache.args...)

    prewalk_inf_frame(sv) do frame::InferenceState
        linfo = frame.linfo
        push!(st, get_virtual_frame(frame))
        push!(lineage, linfo)
        haskey(TPCACHE, linfo) || cache_report!(linfo) # caller can be already cached
    end

    return T(st, cache.msg, cache.sig, lineage, cache.args)
end

# code cache interface
# --------------------

code_cache(interp::TPInterpreter) = TPCache(interp, code_cache(interp.native))

struct TPCache{NativeCache}
    interp::TPInterpreter
    native::NativeCache
    TPCache(interp::TPInterpreter, native::NativeCache) where {NativeCache} =
        new{NativeCache}(interp, native)
end
WorldView(tpc::TPCache, wr::WorldRange) = TPCache(tpc.interp, WorldView(tpc.native, wr))
WorldView(tpc::TPCache, args...) = WorldView(tpc, WorldRange(args...))

CC.haskey(tpc::TPCache, mi::MethodInstance) = CC.haskey(tpc.native, mi)

function CC.get(tpc::TPCache, mi::MethodInstance, default)
    ret = CC.get(tpc.native, mi, default)

    # cache hit, we need to append already-profiled error reports if exist
    if ret !== default
        if haskey(TPCACHE, mi)
            id, cached_reports = TPCACHE[mi]

            # don't append duplicated reports from the same inference process
            if id !== get_id(tpc.interp)
                for cache in cached_reports
                    restore_cached_report!(mi, cache, tpc.interp)
                end
            end
        end
    end

    return ret
end

CC.getindex(tpc::TPCache, mi::MethodInstance) = CC.getindex(tpc.native, mi)

CC.setindex!(tpc::TPCache, ci::CodeInstance, mi::MethodInstance) = CC.setindex!(tpc.native, ci, mi)
