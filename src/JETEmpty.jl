# Empty stubs for the exported functions/macros of JET.jl, to provide a more informative
# error message when JET.jl is used with a pre-release version of Julia.

const empty_loading_message = """
    JET.jl does not guarantee compatibility with nightly versions of Julia and,
    it will not be loaded on nightly versions by default.
    If you want to load JET on a nightly version, set the `JET_DEV_MODE` Preferences.jl
    configuration to `true` and reload it.
    """ |> strip
function __init__()
    @warn empty_loading_message
end

const empty_stub_message = strip("""
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

for exported_func in exports
    if startswith(String(exported_func), "@")
        exported_macro_name = Symbol(lstrip(String(exported_macro), '@'))
        @eval macro $exported_macro_name(exs...); :(error($(GlobalRef(@__MODULE__, :empty_stub_message)))); end
    else
        @eval $exported_func(args...; kws...) = error($(GlobalRef(@__MODULE__, :empty_stub_message)))
    end
end
