module self_check

using JET

let target_modules = (JET,)
    JETAnalyzerT   = typeof(JET.JETAnalyzer())
    OptAnalyzerT   = typeof(JET.OptAnalyzer())
    InferenceState = Core.Compiler.InferenceState
    annotate_types = true

    # error analysis
    # ==============

    # JETAnalyzer
    test_call(JET.analyze_frame!, (JETAnalyzerT, InferenceState);
                target_modules, annotate_types)
    # OptAnalyzer
    test_call(JET.analyze_frame!, (OptAnalyzerT, InferenceState);
                target_modules, annotate_types)
    # top-level
    test_call(JET.virtual_process, (String, String, Nothing, JETAnalyzerT, JET.ToplevelConfig{Vector{Expr}});
                target_modules, annotate_types)
    test_call(JET.virtual_process, (String, String, Base.PkgId, JETAnalyzerT, JET.ToplevelConfig{Vector{Expr}});
                target_modules, annotate_types)
    # entries
    test_call(JET.report_file, (String,);
                target_modules, annotate_types)
    test_call(JET.report_package, (Union{String,Module,Nothing},);
                target_modules, annotate_types)

    # optimization analysis
    # =====================

    # ignore some dynamically-designed functions
    # TODO implement `signature_filter` and limit the ignorance scope
    function function_filter(@nospecialize f)
        if (f === JET.widenconst ||
            f === JET.ignorelimited ||
            f === JET.print ||
            f === Base.CoreLogging.handle_message ||
            f === get ||
            f === sprint ||
            f === string ||
            f === zero ||
            f === JET.copy_report ||
            f === JET.handle_sig! ||
            f === JET._which ||
            f === JET.rewrap_unionall ||
            f === JET.nameof ||
            false)
            return false
        end
        return true
    end
    # JETAnalyzer
    test_opt(JET.analyze_frame!, (JETAnalyzerT, InferenceState);
                target_modules,
                function_filter, annotate_types)
    # OptAnalyzer
    test_opt(JET.analyze_frame!, (OptAnalyzerT, InferenceState);
                target_modules,
                function_filter, annotate_types)
end

end # module self_check
