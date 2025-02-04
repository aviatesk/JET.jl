# Empty stubs for the exported functions/macros of JET.jl, to provide a more informative
# error message when JET.jl is used with a pre-release version of Julia.

const err_msg = strip("""
    JET.jl does not guarantee compatibility with pre-release versions of Julia and
    is not be loaded on this versions by default.
        Julia VERSION = $VERSION
    We recommend using a stable version of Julia in order to use JET.jl.
    Or to try JET with this pre-release Julia version use Preferences.jl to enable
    `JET_DEV_MODE`, and then reload JET. For example create a file named
    `LocalPreferences.toml` which contains the line:
        JET_DEV_MODE = true
    Note that JET.jl may not function properly with a pre-release versions of Julia
    even with `JET_DEV_MODE` enabled.
    """)

for exported_func in (
    :report_call, :test_call,
    :report_file, :test_file, :report_package, :test_package, :report_text, :reportkey, :test_text,
    :watch_file,
    # optanalyzer
    :report_opt, :test_opt,
    # configurations
    :LastFrameModule, :AnyFrameModule
)
    @eval $exported_func(args...; kws...) = error($err_msg)
end
for exported_macro in (
    :report_call, :test_call,
    :report_opt, :test_opt
)
    @eval begin
        macro $exported_macro(args...)
            error($err_msg)
        end
    end
end
