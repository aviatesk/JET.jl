using Test, JET

for line in filter(!isempty, split(JET.empty_loading_message, '\n'))
    @test occursin(line, errmsg)
end

@test_throws JET.empty_stub_message report_call(sin, (Int,))

@test_throws JET.empty_stub_message @report_call sin(42)
