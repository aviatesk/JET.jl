# NOTE: optimization step for `JETInterpreter`
# currently `may_optimize(::JETInterpreter)` always returns `false`,
# as such we even don't need the definitions below, but I'd define them to minimize codegen time

struct JETOptimizationState end

function CC.OptimizationState(linfo::MethodInstance, params::OptimizationParams, interp::JETInterpreter)
    return JETOptimizationState()
end

function CC.optimize(interp::JETInterpreter, opt::JETOptimizationState, params::OptimizationParams, result)
    return nothing # do nothing

    # # NOTE: to keep the previous implementations as a future reference
    # # `JETInterpreter` shouldn't recur into the optimization step via `@invoke` macro,
    # # but rather it should just go through `NativeInterpreter`'s optimization pass
    # # this is necessary because `CC.get(wvc::WorldView{JETCache}, mi::MethodInstance, default)`
    # # can be called from optimization pass otherwise, and it may cause errors because our report
    # # construction is only valid before optimization pass
    # return optimize(interp.native, opt, params, result)
end
