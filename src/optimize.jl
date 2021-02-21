function CC.OptimizationState(linfo::MethodInstance, params::OptimizationParams, interp::JETInterpreter)
    return OptimizationState(linfo, params, interp.native)
end

function CC.optimize(interp::JETInterpreter, opt::OptimizationState, params::OptimizationParams, result)
    # NOTE: don't recurse with `@invoke`
    # `JETInterpreter` shouldn't recur into the optimization step via `@invoke` macro,
    # but rather it should just go through `NativeInterpreter`'s optimization pass
    # this is necessary because `CC.get(wvc::WorldView{JETGlobalCache}, mi::MethodInstance, default)`
    # can be called from optimization pass otherwise, and it may cause errors because our report
    # construction is only valid on inference frames before the optimizer runs on it
    return optimize(interp.native, opt, params, result)
end
