module Outer

baremodule Inner

import ..Outer: Outer
Core.include(Outer, "include1.jl")

end # module Inner

end # module Outer
