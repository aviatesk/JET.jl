using Test, Aqua, JET

Aqua.test_all(JET; stale_deps=false)

using Pkg
function test_dependent(test_func, (pkgname, code))
    old = Pkg.project().path
    pkgcode = Base.remove_linenums!(code)
    mktempdir() do tempdir
        try
            pkgpath = normpath(tempdir, pkgname)
            Pkg.generate(pkgpath; io=devnull)
            Pkg.activate(pkgpath; io=devnull)
            Pkg.develop(; path=normpath(@__DIR__, ".."), io=devnull)

            proj = Pkg.project()
            @assert proj.name == pkgname && haskey(proj.dependencies, "JET")

            pkgcode = Expr(:module, true, Symbol(pkgname), pkgcode)
            pkgfile = normpath(pkgpath, "src", "$pkgname.jl")
            write(pkgfile, string(pkgcode))

            test_func(proj)
        finally
            Pkg.activate(old; io=devnull)
        end
    end
end

test_dependent("Issue499" => quote
    using JET
end) do proj
    Pkg.precompile(proj.name; io=devnull)
    @test true
end
