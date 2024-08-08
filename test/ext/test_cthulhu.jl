using JET, Cthulhu

@testset begin
    getsomething(x::Array) = x[]
    computesomething(x) = getsomething(x) + 1

    rpt = @report_opt computesomething(Any[1])
    r = only(JET.get_reports(rpt))
    parent = Cthulhu.treelist(r)
    @test parent.data.nd isa Core.MethodInstance
end
