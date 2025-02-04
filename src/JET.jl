module JET

using Preferences: Preferences

const JET_DEV_MODE = Preferences.@load_preference("JET_DEV_MODE", false)

const JET_LOADABLE = JET_DEV_MODE || (get(VERSION.prerelease, 1, "") != "DEV")

# exports
# =======

export
    # jetanalyzer
    @report_call, report_call, @test_call, test_call,
    report_file, test_file, report_package, test_package, report_text, reportkey, test_text,
    watch_file,
    # optanalyzer
    @report_opt, report_opt, @test_opt, test_opt,
    # configurations
    LastFrameModule, AnyFrameModule

# Pre-release Julia versions are not supported, and we don't expect JET to even
# precompile in pre-release versions. So, instead of having JET fail to precompile, we
# simply make JET an empty module so that failure is delayed until the first time JET is
# actually used. This means packages with a test dependency on JET don't fail to
# precompile when their tests are run, instead there will be test failures when JET is
# used (but potentially other tests can at least run).
@static if JET_LOADABLE
    include("JETBase.jl")
else
    @warn """
    JET.jl does not guarantee compatibility with nightly versions of Julia and,
    it will not be loaded on nightly versions by default.
    If you want to load JET on a nightly version, set the `JET_DEV_MODE` Preferences.jl
    configuration to `true` and reload it.
    """
    include("JETEmpty.jl")
end

end # module
