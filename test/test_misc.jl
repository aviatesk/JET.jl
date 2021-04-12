@testset "get_package_file" begin
    using Pkg, JET
    using JET: get_package_file

    target = pathof(JET)

    @test get_package_file("JET") == target
    @test_throws ErrorException get_package_file("unknown")

    @test get_package_file(JET) == target
    @test_throws ErrorException get_package_file(Module())

    # suppress logs from Pkg.jl if possible
    function pkg_activate(args...; io = IOBuffer(), kwargs...)
        @static if VERSION ≥ v"1.7-DEV"
            Pkg.activate(args...; io, kwargs...)
        else
            Pkg.activate(args...; kwargs...)
        end
    end
    io = IOBuffer()
    old = Pkg.project().path
    try
        pkg_activate(pkgdir(JET))
        @test get_package_file(nothing) == target

        pkg_activate(; temp = true)
        @test_throws ErrorException get_package_file(nothing)
    catch err
        rethrow(err)
    finally
        pkg_activate(old; io)
    end
end
