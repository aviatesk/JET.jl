module JET

using Preferences: Preferences

const JET_DEV_MODE = Preferences.@load_preference("JET_DEV_MODE", false)

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
    # configurations
    :LastFrameModule, :AnyFrameModule
))

for exported_name in exports
    Core.eval(@__MODULE__, Expr(:export, exported_name))
end

using Base: Compiler as CC

# Pre-release Julia versions are not supported, and we don't expect JET to even
# precompile in pre-release versions. So, instead of having JET fail to precompile, we
# simply make JET an empty module so that failure is delayed until the first time JET is
# actually used. This means packages with a test dependency on JET don't fail to
# precompile when their tests are run, instead there will be test failures when JET is
# used (but potentially other tests can at least run).
@static if JET_LOADABLE
    include("JETBase.jl")
else
    include("JETEmpty.jl")
end

if CC !== Base.Compiler
    # XXX this shouldn't be necessary
    push_inithook!() do
        Base.REFLECTION_COMPILER[] = CC
    end
end

end # module
