function CC.OptimizationState(linfo::MethodInstance, params::OptimizationParams, interp::JETInterpreter)
    return OptimizationState(linfo, params, interp.native)
end

function CC.optimize(interp::JETInterpreter, opt::OptimizationState, params::OptimizationParams, result)
    # NOTE: to keep the previous implementations as a future reference
    # `JETInterpreter` shouldn't recur into the optimization step via `@invoke` macro,
    # but rather it should just go through `NativeInterpreter`'s optimization pass
    # this is necessary because `CC.get(wvc::WorldView{JETCache}, mi::MethodInstance, default)`
    # can be called from optimization pass otherwise, and it may cause errors because our report
    # construction is only valid before optimization pass
    return optimize(interp.native, opt, params, result)
end
