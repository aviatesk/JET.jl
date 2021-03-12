## "legacy" ... ?

This directory keeps legacy code, which is supposed to work with older Julia versions.
It might not be "older" in the usual sense though – currently the code here is kept for the compatibility with Julia v1.6, whose stable version is not even released yet (as of 2021/03/11).
This is because the `AbstractInterpreter` interface is really unstable at this point and right under the rapid development.

Note that the files are not given `.jl` file extensions.
This is a naive hack to prevent [CoverageTools.jl](https://github.com/JuliaCI/CoverageTools.jl) to take into account them when calculating a coverage.
