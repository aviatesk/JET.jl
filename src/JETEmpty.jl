# Empty stubs for the exported functions and macros of JET.jl.

const empty_loading_message = """
    Full JET functionality is not available on Julia $VERSION.
    JET was loaded with empty stubs so that this environment remains usable.
    To try JET on this Julia version, set the `JET_DEV_MODE` Preferences.jl
    configuration to `true` and reload it.
    """ |> strip
function __init__()
    @warn empty_loading_message
end

const empty_stub_message = strip("""
    Full JET functionality is not available on Julia $VERSION.
    To try JET on this Julia version, use Preferences.jl to enable `JET_DEV_MODE` and
    reload JET. For example, create a `LocalPreferences.toml` file containing:
        ```toml
        [JET]
        JET_DEV_MODE = true
        ```
    JET may not function correctly on unsupported Julia versions even with
    `JET_DEV_MODE` enabled.
    """)

for exported_func in exports
    if startswith(String(exported_func), "@")
        exported_macro_name = Symbol(lstrip(String(exported_func), '@'))
        @eval macro $exported_macro_name(exs...); :(error($(GlobalRef(@__MODULE__, :empty_stub_message)))); end
    else
        @eval $exported_func(args...; kws...) = error($(GlobalRef(@__MODULE__, :empty_stub_message)))
    end
end
