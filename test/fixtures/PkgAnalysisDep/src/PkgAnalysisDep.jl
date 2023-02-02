module PkgAnalysisDep

export func1

func1() = "exported!"
func2() = "not exported!"

module Inner

export func3

func3() = "inner exported!"

end

end # module PkgAnalysisDep
