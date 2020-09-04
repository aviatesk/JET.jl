# global cache
# ------------

struct InferenceReportCache{T<:InferenceErrorReport}
    st::ViewedVirtualStackTrace
    msg::String
    sig::Vector{Any}
end

const TPCACHE = Dict{UInt,Pair{Symbol,Vector{InferenceReportCache}}}()

get_id(interp::TPInterpreter) = interp.id

function restore_cached_report!(cache::InferenceReportCache{T}, interp) where {T<:InferenceErrorReport}
    report = restore_cached_report(T, cache, interp)
    push!(interp.reports, report)
end

function restore_cached_report!(cache::InferenceReportCache{ExceptionReport}, interp)
    report = restore_cached_report(ExceptionReport, cache, interp)
    push!(interp.exception_reports, length(interp.reports) => report)
end

function restore_cached_report(T, cache, interp)
    sv = get_current_frame(interp)
    cur_st = track_abstract_call_stack!(sv)
    st = vcat(cache.st, cur_st)
    return T(st, cache.msg, cache.sig)
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
        key = hash(mi)
        if haskey(TPCACHE, key)
            id, cached_reports = TPCACHE[key]

            # don't append duplicated reports from the same inference process
            if id !== get_id(tpc.interp)
                for cache in cached_reports
                    restore_cached_report!(cache, tpc.interp)
                end
            end
        end
    end

    return ret
end

CC.getindex(tpc::TPCache, mi::MethodInstance) = CC.getindex(tpc.native, mi)

CC.setindex!(tpc::TPCache, ci::CodeInstance, mi::MethodInstance) = CC.setindex!(tpc.native, ci, mi)
