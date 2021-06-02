function CC.OptimizationState(linfo::MethodInstance, params::OptimizationParams, analyzer::AbstractAnalyzer)
    return OptimizationState(linfo, params, get_native(analyzer))
end

function CC.optimize(analyzer::AbstractAnalyzer, opt::OptimizationState, params::OptimizationParams, result)
    # NOTE: don't recurse with `@invoke`
    # `AbstractAnalyzer` shouldn't recur into the optimization step via `@invoke` macro,
    # but rather it should just go through `NativeInterpreter`'s optimization pass
    # this is necessary because `CC.get(wvc::WorldView{JETGlobalCache}, mi::MethodInstance, default)`
    # can be called from optimization pass otherwise, and it may cause errors because our report
    # construction is only valid on inference frames before the optimizer runs on it
    return optimize(get_native(analyzer), opt, params, result)
end
