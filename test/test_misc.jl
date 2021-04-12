@testset "get_package_file" begin
    using Pkg, JET
    using JET: get_package_file

    target = pathof(JET)

    @test get_package_file("JET") == target
    @test_throws ErrorException get_package_file("unknown")

    @test get_package_file(JET) == target
    @test_throws ErrorException get_package_file(Module())

    io = IOBuffer()
    old = Pkg.project().path
    try
        Pkg.activate(pkgdir(JET); io)
        @test get_package_file(nothing) == target

        Pkg.activate(; temp = true, io)
        @test_throws ErrorException get_package_file(nothing)
    catch err
        rethrow(err)
    finally
        Pkg.activate(old; io)
    end
end
