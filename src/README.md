## Organization

JET's codebase is structured as following:
- [src/abstractinterpret](./abstractinterpret/): implements the core functionalities of `AbstractAnalyzer`, the general program analysis framework based on abstract interpretation
- [src/toplevel](./toplevel/): implements `ConcreteInterpreter`, which is combined with `AbstractAnalyzer` to analyze top-level scripts
- [src/ui](./ui/): implements a set of functionalities to show `AbstractAnalyzer`'s analysis result in various frontends, e.g. console output
- [src/analyzers](./analyzers/): implements the default set of useful analyzers built on top of the `AbstractAnalyzer` framework
- [src/JET.jl](./JET.jl): defines the package, implements various analysis entry points
