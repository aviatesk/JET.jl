using JET, Documenter, Literate, Markdown

const DOC_SRC_DIR          = normpath(@__DIR__, "src")
const INDEX_FILENAME       = normpath(DOC_SRC_DIR, "index.md")
const PLUGIN_API_FILENAME  = normpath(DOC_SRC_DIR, "generated-plugin-api.md")
const PLUGIN_EXAMPLES_DIRS = (normpath(@__DIR__, "..", "examples"), normpath(DOC_SRC_DIR, "generated-plugin-examples"))

writeln(io, xs...) = write(io, xs..., '\n')

function generate_index!()
    isfile(INDEX_FILENAME) && rm(INDEX_FILENAME)
    open(INDEX_FILENAME, write=true) do io
        writeln(io, """
        ```@setup index
        using JET
        ```
        """)

        README = string(@doc JET)
        incode = false
        for line in split(README, '\n')
            if startswith(line, "```julia-repl") && !endswith(line, "noeval")
                incode = true
                writeln(io, "```@repl index")
                continue
            elseif endswith(line, "```")
                incode = false
                writeln(io, "```")
                continue
            elseif incode
                if startswith(line, "julia> ")
                    writeln(io, replace(line,
                        "julia> "=>"",
                        r"report_package\((.*)\)"=>s"report_package(\1; toplevel_logger=nothing)"))
                end
                continue
            end
            line_ref = replace(line,
                r"`(.+?)`" => function (word)
                    if any(names(JET)) do name
                            occursin(String(name), word)
                        end
                        return "[$word](@ref)"
                    end
                    return word
                end)
            writeln(io, line_ref)
        end

        # Switch back to the original environment. Otherwise the `@repl` blocks in
        # jetanalysis.md/optanalysis.md would end up using the temporary environment
        # we set up for the `report_package` demo.
        writeln(io, """
        ```@setup index
        using Pkg; Pkg.activate(normpath(@__DIR__, ".."))
        ```
        """)
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
            generate_example_docs!(normpath(root, dir), outs)
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
        interface_docs = let
            objs = getfield.(Ref(JET.JETInterface), JET.JETInterface.DOCUMENTED_NAMES)
            map(objs) do @nospecialize obj
                if obj === JET.AnalysisToken
                    return "JET.AnalysisToken(::JET.AbstractAnalyzer)"
                elseif obj === JET.InferenceErrorReport
                    return "JET.InferenceErrorReport()"
                else
                    return string(parentmodule(obj), '.', nameof(obj))
                end
            end |> Base.Fix2(join, '\n') |> codeblock
        end
        examples_contents = codeblock("Pages = $(repr(examples_pages))", "@contents")

        s = md"""
        # [`AbstractAnalyzer` Framework](@id AbstractAnalyzer-Framework)

        $contents

        JET offers an infrastructure to implement a "plugin" code analyzer.
        Actually, [JET's default error analyzer](@ref jetanalysis) is one specific instance
        of such a plugin analyzer built on top of the framework.

        In this documentation we will try to elaborate the framework APIs and showcase example analyzers.

        !!! warning
            The APIs described in this page is _very_ experimental and subject to changes.
            And this documentation is also very WIP.

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
    DocMeta.setdocmeta!(JET, :DocTestSetup, :(using JET); recursive=true)
    examples = generate_example_docs!()
    makedocs(; modules = [JET, Base.Compiler], # TODO use the Compiler stdlib wiht the full implementation?
               sitename = "JET.jl",
               pages = Any[
                    "README" => generate_index!(),
                    "Tutorial" => "tutorial.md",
                    "Analyses" => Any[
                        "Error Analysis" => "jetanalysis.md",
                        "Optimization Analysis" => "optanalysis.md"],
                    "Configurations" => "config.md",
                    "Internals" => "internals.md",
                    "`AbstractAnalyzer` Framework" => Any[
                        "API" => generate_api_doc(examples),
                        "Examples" => examples]
               ],
               format = Documenter.HTML(;
                   prettyurls = get(ENV, "CI", nothing) == "true",
                   ansicolor = true),
               warnonly = [:missing_docs, :cross_references])
end

deploydocs(; repo = "github.com/aviatesk/JET.jl.git",
             push_preview = true)
