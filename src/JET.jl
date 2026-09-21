module JET

using Preferences: Preferences

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
- The [`allow_unsupported_julia`](@ref ALLOW_UNSUPPORTED_JULIA) preference defaults to
  `true`, so full JET functionality is loaded even on unsupported Julia versions, i.e.
  [`JET_AVAILABLE`](@ref) becomes `true`. Unlike with `allow_unsupported_julia` alone, no
  warning is emitted on such versions, although JET may still not work correctly there.
- Internal assertions that check JET's report caching and validate report interface
  implementations are compiled in. They are omitted by default since they add overhead.
- The `use_fixed_world` preference defaults to `false`, so JET's pre-defined analyzers and
  interpreters run their analysis in the current world age instead of the world age fixed
  at load time. This makes redefinitions of JET's own code take effect, at the cost of
  losing robustness against invalidations caused by loading other packages.

This mode is intended for developing JET itself, and is not recommended otherwise. To try
JET on an unsupported Julia version, enable [`ALLOW_UNSUPPORTED_JULIA`](@ref) instead.
"""
const JET_DEV_MODE = Preferences.@load_preference("JET_DEV_MODE", false)

const USE_FIXED_WORLD = Preferences.@load_preference("use_fixed_world", !JET_DEV_MODE)

const PKG_EVAL = Base.get_bool_env("JULIA_PKGEVAL", false)

const MINIMUM_JULIA_VERSION = v"1.12.0-beta1.11"
const FIRST_UNSUPPORTED_JULIA_VERSION = v"1.15.0-DEV"

_is_supported_julia(version::VersionNumber) =
    MINIMUM_JULIA_VERSION ≤ version < FIRST_UNSUPPORTED_JULIA_VERSION

"""
    const ALLOW_UNSUPPORTED_JULIA::Bool

Whether full JET functionality is loaded even on unsupported Julia versions.

This is a [Preferences.jl](https://github.com/JuliaPackaging/Preferences.jl) setting that
is read when JET is loaded, so it needs to be configured before JET is precompiled, e.g.
with a `LocalPreferences.toml` file containing:
```toml
[JET]
allow_unsupported_julia = true
```

Unlike [`JET_DEV_MODE`](@ref), enabling it does not change any other behavior of JET.
It has no effect on supported Julia versions, and applies to any unsupported Julia version
rather than a specific one. Since JET may not work correctly on unsupported Julia versions,
loading JET on such a version with this setting enabled emits a warning, unless
`JET_DEV_MODE` is enabled.

It defaults to `true` when `JET_DEV_MODE` is enabled, and to `false` otherwise.
"""
const ALLOW_UNSUPPORTED_JULIA = Preferences.@load_preference("allow_unsupported_julia", JET_DEV_MODE)

# PkgEval must exercise full JET functionality on pre-release Julia versions so that
# compiler incompatibilities are detected instead of being hidden by empty stubs.
"""
    const JET_AVAILABLE::Bool

Whether full JET functionality is available in the current process.

This is `true` on supported Julia versions, when [`ALLOW_UNSUPPORTED_JULIA`](@ref) is
enabled (as it is by default with [`JET_DEV_MODE`](@ref)), or when running under PkgEval,
i.e. with the `JULIA_PKGEVAL` environment variable set. Otherwise JET is loaded with empty
stubs: loading it emits a warning, and calling any of its entry points throws an error.

This lets a test suite stay instantiable on unsupported Julia versions while skipping its
JET-specific checks at runtime:
```julia
using JET

if JET.JET_AVAILABLE
    include("jet_tests.jl")
end
```
"""
const JET_AVAILABLE = ALLOW_UNSUPPORTED_JULIA || PKG_EVAL || _is_supported_julia(VERSION)
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
delete!(exports, :report_package)
delete!(exports, :test_package)

for exported_name in exports
    Core.eval(@__MODULE__, Expr(:export, exported_name))
end

# Keep JET installable on unsupported future Julia versions so that packages with JET
# as a test dependency can instantiate their test environments. Full functionality is
# only loaded on supported Julia versions unless explicitly enabled with
# `allow_unsupported_julia` or `JET_DEV_MODE`.
@static if JET_AVAILABLE
    include("JETBase.jl")
else
    include("JETEmpty.jl")
end

end # module
