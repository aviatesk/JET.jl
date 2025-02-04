module JET

using Preferences: Preferences

const JET_DEV_MODE = Preferences.@load_preference("JET_DEV_MODE", false)

# Pre-release Julia versions are not supported, and we don't expect JET to even
# precompile in pre-release versions. So, instead of having JET fail to precompile, we
# simply make JET an empty module so that failure is delayed until the first time JET is
# actually used. This means packages with a test dependency on JET don't fail to
# precompile when their tests are run, instead there will be test failures when JET is
# used (but potentially other tests can at least run).
if JET_DEV_MODE || (get(VERSION.prerelease, 1, "") != "DEV")
    include("jet_base.jl")
end

end # module
