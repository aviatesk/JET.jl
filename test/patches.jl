# NOTE: this file keeps patches for julia itself to make JET.jl test keep passing

@static if Sys.iswindows()
    @eval Base.Filesystem function splitdrive(path::String)
        m = match(r"^([^\\]+:|\\\\[^\\]+\\[^\\]+|\\\\\?\\UNC\\[^\\]+\\[^\\]+|\\\\\?\\[^\\]+:|)(.*)$"s, path)::AbstractMatch
        String(something(m.captures[1])), String(something(m.captures[2]))
    end
end
