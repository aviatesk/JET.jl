module self_check

using JET

let target_modules = (JET,)
    JETAnalyzerT   = typeof(JET.JETAnalyzer())
    OptAnalyzerT   = typeof(JET.OptAnalyzer())
    InferenceState = JET.CC.InferenceState

    # error analysis
    # ==============

    # JETAnalyzer
    test_call(JET.analyze_frame!, (JETAnalyzerT, InferenceState); target_modules)
    # OptAnalyzer
    test_call(JET.analyze_frame!, (OptAnalyzerT, InferenceState); target_modules)
    # top-level
    test_call(JET.virtual_process, (JET.JETConcreteInterpreter{JETAnalyzerT}, String, String, JET.ToplevelConfig); target_modules)
    # entries
    test_call(JET.report_file, (String,); target_modules)
    test_call(JET.report_package, (Union{String,Module,Nothing},); target_modules)

    # optimization analysis
    # =====================

    return # FIXME on v1.11 and higher

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
            f === Base._which ||
            f === Base.rewrap_unionall ||
            f === JET.nameof ||
            false)
            return false
        end
        return true
    end
    # JETAnalyzer
    test_opt(JET.analyze_frame!, (JETAnalyzerT, InferenceState); function_filter, target_modules)
    # OptAnalyzer
    test_opt(JET.analyze_frame!, (OptAnalyzerT, InferenceState); function_filter, target_modules)
end

end # module self_check
