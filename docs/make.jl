using JET, Documenter, Literate, Markdown

const DOC_SRC_DIR          = normpath(@__DIR__, "src")
const INDEX_FILENAME       = normpath(DOC_SRC_DIR, "generated-index.md")
const PLUGIN_API_FILENAME  = normpath(DOC_SRC_DIR, "generated-plugin-api.md")
const PLUGIN_EXAMPLES_DIRS = (normpath(@__DIR__, "..", "examples"), normpath(DOC_SRC_DIR, "generated-plugin-examples"))

function generate_index!()
    isfile(INDEX_FILENAME) && rm(INDEX_FILENAME)
    open(INDEX_FILENAME, write=true) do io
        s = string(@doc JET)
        write(io, s)
    end

    return relpath(INDEX_FILENAME, DOC_SRC_DIR)
end

function generate_example_docs!(dir = PLUGIN_EXAMPLES_DIRS[1], outs = String[])
    # clean up first
    outdir = PLUGIN_EXAMPLES_DIRS[2]
    isdir(outdir) && rm(outdir; recursive = true)

    for (root, dirs, files) in walkdir(dir)
        for file in files
            endswith(file, ".jl") || continue
            push!(outs, Literate.markdown(normpath(root, file), outdir; documenter=true))
        end
        for dir in dirs
            gen_example_doc!(normpath(root, dir), outs)
        end
    end

    return relpath.(outs, DOC_SRC_DIR)
end

function codeblock(s, header = "@docs")
    return """
    ```$header
    $s
    ```
    """ |> Markdown.parse
end

function generate_api_doc(examples_pages)
    out = relpath(PLUGIN_API_FILENAME, DOC_SRC_DIR)

    isfile(PLUGIN_API_FILENAME) && rm(PLUGIN_API_FILENAME)
    open(PLUGIN_API_FILENAME, write=true) do io
        contents = codeblock("Pages = $(repr([out]))", "@contents")
        interface_docs = codeblock(join(JET.JETInterfaces.DOCUMENTED_NAMES, '\n'))
        examples_contents = codeblock("Pages = $(repr(examples_pages))", "@contents")

        s = md"""
        # JET.jl Pluggable Analysis Framework

        $contents

        !!! warning
            The APIs described in this page is _very_ experimental and subject to changes.
            And this documentation is also very WIP.

        ```@meta
        CurrentModule = JET
        ```

        ## Interfaces

        $interface_docs

        ## Examples

        $examples_contents

        """ |> string

        write(io, s)
    end
    @assert isfile(PLUGIN_API_FILENAME)

    return out
end

let
    examples = generate_example_docs!()
    makedocs(; modules = [JET],
               sitename="JET.jl",
               pages = [
                    "README" => generate_index!(),
                    "Usages" => "usages.md",
                    "Configurations" => "config.md",
                    "Internals" => "internals.md",
                    "Pluggable Analysis Framework" => Any[
                        "API"      => generate_api_doc(examples),
                        "Examples" => examples,
                    ]
               ],
               format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
               )
end

deploydocs(; repo = "github.com/aviatesk/JET.jl.git",
             push_preview = true,
             )
