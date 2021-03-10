using Documenter, JET

makedocs(; modules = [JET],
           sitename="JET.jl",
           pages = [
             "README" => "index.md",
             "JET Configurations" => "config.md",
           ],
           format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
           )

deploydocs(; repo = "github.com/aviatesk/JET.jl.git",
             push_preview = true,
             )
