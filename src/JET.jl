module JET

using Preferences: Preferences
using .Preferences: UUID

"""
    const JET_DEV_MODE::Bool

Whether JET is loaded in development mode.

This is a [Preferences.jl](https://github.com/JuliaPackaging/Preferences.jl) setting that
is read when JET is loaded, so it needs to be configured before JET is precompiled, e.g.
with a `LocalPreferences.toml` file containing:
```toml
[JET]
JET_DEV_MODE = true
```

Enabling it has the following effects:
- Full JET functionality is loaded even on unsupported Julia versions, i.e.
  [`JET_AVAILABLE`](@ref) becomes `true`. Note that JET may still not work correctly on
  such versions.
- Internal assertions that check JET's report caching and validate report interface
  implementations are compiled in. They are omitted by default since they add overhead.
- The `use_fixed_world` preference defaults to `false`, so JET's pre-defined analyzers and
  interpreters run their analysis in the current world age instead of the world age fixed
  at load time. This makes redefinitions of JET's own code take effect, at the cost of
  losing robustness against invalidations caused by loading other packages.

This mode is intended for developing JET itself and for experimenting on unsupported Julia
versions, and is not recommended otherwise.
"""
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
    const JET_AVAILABLE::Bool

Whether full JET functionality is available in the current process.

This is `true` on supported Julia versions, when [`JET_DEV_MODE`](@ref) is enabled, or when
running under PkgEval, i.e. with the `JULIA_PKGEVAL` environment variable set. Otherwise JET
is loaded with empty stubs: loading it emits a warning, and calling any of its entry points
throws an error.

This lets a test suite stay instantiable on unsupported Julia versions while skipping its
JET-specific checks at runtime:
```julia
using JET

if JET.JET_AVAILABLE
    include("jet_tests.jl")
end
```
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
    # trimanalyzer
    Symbol("@report_trim"), :report_trim, Symbol("@test_trim"), :test_trim,
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
