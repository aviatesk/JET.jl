module JET

using Preferences: Preferences

const JET_DEV_MODE = Preferences.@load_preference("JET_DEV_MODE", false)

const USE_FIXED_WORLD = Preferences.@load_preference("use_fixed_world", !JET_DEV_MODE)

const PKG_EVAL = Base.get_bool_env("JULIA_PKGEVAL", false)

const JET_LOADABLE = JET_DEV_MODE || PKG_EVAL || (get(VERSION.prerelease, 1, "") != "DEV")

# exports
# =======

const exports = Set{Symbol}((
    # jetanalyzer
    Symbol("@report_call"), :report_call, Symbol("@test_call"), :test_call,
    :report_file, :test_file, :report_package, :test_package, :report_text, :reportkey, :test_text,
    :watch_file,
    # optanalyzer
    Symbol("@report_opt"), :report_opt, Symbol("@test_opt"), :test_opt,
    # trimanalyzer
    Symbol("@report_trim"), :report_trim, Symbol("@test_trim"), :test_trim,
    # configurations
    :LastFrameModule, :AnyFrameModule
))

for exported_name in exports
    Core.eval(@__MODULE__, Expr(:export, exported_name))
end

using Compiler: Compiler as CC

# Pre-release Julia versions are not supported, and we don't expect JET to even
# precompile in pre-release versions. So, instead of having JET fail to precompile, we
# simply make JET an empty module so that failure is delayed until the first time JET is
# actually used. This means packages with a test dependency on JET don't fail to
# precompile when their tests are run, instead there will be test failures when JET is
# used (but potentially other tests can at least run).
@static if JET_LOADABLE
    @static if VERSION ≥ v"1.12.0-beta1.11"
        include("JETBase.jl")
    else
        function __init__()
            @warn """
            The latest version of JET is incompatible with Julia versions earlier than `v"1.12.0-beta1.11"`.
            To build a compatible Julia version, follow the instructions at
            https://github.com/aviatesk/JET.jl/blob/master/CHANGELOG.md#0103.
            """
        end
    end
else
    include("JETEmpty.jl")
end

end # module
