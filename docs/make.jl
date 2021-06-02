using Documenter, JET

joinlines(lines) = join(lines, '\n')

function make_example_doc(filename)
    doc, code = get_doc_and_code(filename)
    return joinlines((
        doc,
        "```julia",
        code,
        "```",
    ))
end

function get_doc_and_code(filename)
    doclines, codelines = String[], String[]
    indoc = false
    for line in readlines(filename)
        if line == "\"\"\""
            indoc = !indoc
            continue
        end
        push!(indoc ? doclines : codelines, line)
    end
    return joinlines(doclines), joinlines(codelines)
end

function attach_doc!(dir)
    docs = String[]

    function gen_doc!(dir)
        for (root, dirs, files) in walkdir(dir)
            for file in files
                endswith(file, ".jl") || continue
                push!(docs, make_example_doc(normpath(root, file)))
            end
            for dir in dirs
                gen_doc!(normpath(root, dir))
            end
        end
    end
    gen_doc!(dir)

    doc = join(docs, "\n---\n")

    @eval JET module PluginExamples end
    @eval JET @doc $doc PluginExamples
end

attach_doc!(normpath(@__DIR__, "..", "examples"))

makedocs(; modules = [JET],
           sitename="JET.jl",
           pages = [
                "README" => "index.md",
                "Usages" => "usages.md",
                "Configurations" => "config.md",
                "Internals" => "internals.md",
                "Pluggable Analysis Framework" => "plugins.md",
           ],
           format = Documenter.HTML(prettyurls = get(ENV, "CI", nothing) == "true"),
           )

deploydocs(; repo = "github.com/aviatesk/JET.jl.git",
             push_preview = true,
             )
