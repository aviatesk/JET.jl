module JET

using Preferences: Preferences
using .Preferences: UUID

const JET_DEV_MODE = Preferences.load_preference(UUID("c3a54625-cd67-489e-a8e7-0a5a0ff4e31b"), "JET_DEV_MODE", false)

const USE_FIXED_WORLD = Preferences.load_preference(UUID("c3a54625-cd67-489e-a8e7-0a5a0ff4e31b"), "use_fixed_world", !JET_DEV_MODE)

const PKG_EVAL = Base.get_bool_env("JULIA_PKGEVAL", false)

const MINIMUM_JULIA_VERSION = v"1.12.0-beta1.11"
const FIRST_UNSUPPORTED_JULIA_VERSION = v"1.14.0-DEV"

_is_supported_julia(version::VersionNumber) =
    MINIMUM_JULIA_VERSION ≤ version < FIRST_UNSUPPORTED_JULIA_VERSION

# PkgEval must exercise full JET functionality on pre-release Julia versions so that
# compiler incompatibilities are detected instead of being hidden by empty stubs.
"""
    JET_AVAILABLE::Bool

Whether full JET functionality is available in the current process.

This is `true` on supported Julia versions, when `JET_DEV_MODE` is enabled, or under
PkgEval. When `false`, JET is loaded with empty stubs so test suites can skip JET-specific
checks while keeping their test environments usable.
"""
const JET_AVAILABLE = JET_DEV_MODE || PKG_EVAL || _is_supported_julia(VERSION)
export JET_AVAILABLE

# exports
# =======

const exports = Set{Symbol}((
    # jetanalyzer
    Symbol("@report_call"), :report_call, Symbol("@test_call"), :test_call,
    :report_file, :test_file, :report_package, :test_package, :report_text, :reportkey, :test_text,
    # optanalyzer
    Symbol("@report_opt"), :report_opt, Symbol("@test_opt"), :test_opt,
    # configurations
    :ReportMatcher, :LastFrameModule, :AnyFrameModule, :LastFrameModuleExact, :AnyFrameModuleExact,
    :LastFrameMethod, :AnyFrameMethod,
))

for exported_name in exports
    Core.eval(@__MODULE__, Expr(:export, exported_name))
end

# Keep JET installable on unsupported future Julia versions so that packages with JET
# as a test dependency can instantiate their test environments. Full functionality is
# only loaded on supported Julia versions unless explicitly enabled with `JET_DEV_MODE`.
@static if JET_AVAILABLE
    include("JETBase.jl")
else
    include("JETEmpty.jl")
end

end # module
