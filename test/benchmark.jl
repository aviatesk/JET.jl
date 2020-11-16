using JET: @benchmark_call, print_reports

versioninfo(stdout)


interp, frame = @benchmark_call rand(Bool)
print_reports(stdout::IO, interp.reports; annotate_types = true)
