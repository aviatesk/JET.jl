using Base: PkgId
using RelocatableExprs
using OrderedCollections: OrderedDict
using JuliaInterpreter: is_doc_expr

# NOTE A lot of code in exprinfo.jl, fileinfo is adapted from Revise -- we consider
#      factoring out the common code into a separate package, but for now we just copy it here.

# ExprInfo
# ========

struct ExprInfo
    sigs::Union{Nothing,Vector{Any}}
end

const ExprInfos = OrderedDict{RelocatableExpr,ExprInfo}

function Base.show(io::IO, exinfos::ExprInfos)
    compact = get(io, :compact, false)
    if compact
        n = 0
        for (rex, (;sigs)) in exinfos
            sigs === nothing && continue
            n += length(sigs)
        end
        print(io, "ExprInfos(<$(length(exinfos)) expressions>, <$n signatures>)")
    else
        print(io, "ExprInfos with the following expressions: ")
        for def in keys(exinfos)
            print(io, "\n  ")
            Base.show_unquoted(io, RelocatableExpr(unwrap(def)), 2)
        end
    end
end

function unwrap_where(ex::Expr)
    while isexpr(ex, :where)
        ex = ex.args[1]
    end
    return ex::Expr
end

function pushex!(exinfos::ExprInfos, ex::Expr)
    uex = unwrap(ex)
    if is_doc_expr(uex)
        body = uex.args[4]
        # Don't trigger for exprs where the documented expression is just a signature
        # (e.g. `"docstr" f(x::Int)`, `"docstr" f(x::T) where T` etc.)
        if isa(body, Expr) && unwrap_where(body).head !== :call
            exinfos[RelocatableExpr(body)] = ExprInfo(nothing)
        end
        if length(uex.args) < 5
            push!(uex.args, false)
        else
            uex.args[5] = false
        end
    end
    exinfos[RelocatableExpr(ex)] = ExprInfo(nothing)
    return exinfos
end

# ModuleInfo
# ==========

struct ModuleInfo
    exprinfos::ExprInfos
    range::UnitRange{Int} # TODO use byte range?
end

"""
    ModuleInfos

For a particular source file, the corresponding `ModuleInfos` is a mapping
`mod=>exprs=>sigs` of the expressions `exprs` found in `mod` and the signatures `sigs`
that arise from them. Specifically, if `mes` is a `ModuleInfos`, then `mes[mod][ex]`
is a list of signatures that result from evaluating `ex` in `mod`. It is possible that
this returns `nothing`, which can mean either that `ex` does not define any methods
or that the signatures have not yet been cached.
The first `mod` key is guaranteed to be the module into which this file was `include`d.
"""
const ModuleInfos = OrderedDict{Module,ExprInfos}

function Base.typeinfo_prefix(io::IO, modinfos::ModuleInfos)
    tn = typeof(modinfos).name
    return string(tn.module, '.', tn.name), true
end

"""
    modinfos = ModuleInfos(mod::Module)

Initialize an empty `ModuleInfos` for a file that is `include`d into `mod`.
"""
ModuleInfos(mod::Module) = ModuleInfos(mod=>ExprInfos())

Base.isempty(modinfos::ModuleInfos) = length(modinfos) == 1 && isempty(first(values(modinfos)))

# FileInfo
# ========

struct FileInfo
    modinfos::ModuleInfos
end

# PkgFiles
# ========

"""
    mutable struct PkgFiles
        id::PkgId
        basedir::String
        files::Vector{String}
    end

PkgFiles encodes information about the current location of a package.
Fields:
- `id`: the `PkgId` of the package
- `basedir`: the current base directory of the package
- `files`: a list of files (relative path to `basedir`) that define the package.

Note that `basedir` may be subsequently updated by Pkg operations such as `add` and `dev`.
"""
mutable struct PkgFiles
    id::PkgId
    basedir::String
    files::Vector{String}
end

PkgFiles(id::PkgId, path::AbstractString) = PkgFiles(id, path, String[])
PkgFiles(id::PkgId, ::Nothing) = PkgFiles(id, "")
PkgFiles(id::PkgId) = PkgFiles(id, normpath(basepath(id)))
PkgFiles(id::PkgId, files::AbstractVector{<:AbstractString}) =
    PkgFiles(id, normpath(basepath(id)), files)

# Abstraction interface
Base.PkgId(info::PkgFiles) = info.id
srcfiles(info::PkgFiles) = info.files
basedir(info::PkgFiles) = info.basedir

function Base.show(io::IO, info::PkgFiles)
    compact = get(io, :compact, false)
    if compact
        print(io, "PkgFiles(", info.id.name, ", ", info.basedir, ", ")
        show(io, info.files)
        print(io, ')')
    else
        println(io, "PkgFiles(", info.id, "):")
        println(io, "  basedir: \"", info.basedir, '"')
        print(io, "  files: ")
        show(io, info.files)
    end
end

function basepath(id::PkgId)
    id.name ∈ ("Main", "Base", "Core") && return ""
    loc = Base.locate_package(id)
    loc === nothing && return ""
    return dirname(dirname(loc))
end

# PkgData
# =======

mutable struct PkgData
    info::PkgFiles
    fileinfos::Vector{FileInfo}
    requirements::Vector{PkgId}
end

PkgData(id::PkgId, path) = PkgData(PkgFiles(id, path), FileInfo[], PkgId[])
PkgData(id::PkgId, ::Nothing) = PkgData(id, "")
function PkgData(id::PkgId)
    bp = basepath(id)
    if !isempty(bp)
        bp = normpath(bp)
    end
    PkgData(id, bp)
end

function Base.show(io::IO, pkgdata::PkgData)
    compact = get(io, :compact, false)
    print(io, "PkgData(")
    if compact
        print(io, '"', pkgdata.info.basedir, "\", ")
        nexs, nsigs, nparsed = 0, 0, 0
        for fi in pkgdata.fileinfos
            thisnexs, thisnsigs = 0, 0
            for (mod, exsigs) in fi.modexsigs
                for (rex, sigs) in exsigs
                    thisnexs += 1
                    sigs === nothing && continue
                    thisnsigs += length(sigs)
                end
            end
            nexs += thisnexs
            nsigs += thisnsigs
            if thisnexs > 0
                nparsed += 1
            end
        end
        print(io, nparsed, '/', length(pkgdata.fileinfos), " parsed files, ", nexs, " expressions, ", nsigs, " signatures)")
    else
        show(io, pkgdata.info.id)
        println(io, ", basedir \"", pkgdata.info.basedir, "\":")
        for (f, fi) in zip(pkgdata.info.files, pkgdata.fileinfos)
            print(io, "  \"", f, "\": ")
            show(IOContext(io, :compact=>true), fi)
            print(io, '\n')
        end
    end
end

# Abstraction interface for PkgData
Base.PkgId(pkgdata::PkgData) = PkgId(pkgdata.info)
basedir(pkgdata::PkgData) = basedir(pkgdata.info)
srcfiles(pkgdata::PkgData) = srcfiles(pkgdata.info)

relpath_safe(path::AbstractString, startpath::AbstractString) = isempty(startpath) ? path : relpath(path, startpath)

function Base.relpath(filename::AbstractString, pkgdata::PkgData)
    if isabspath(filename)
        # `Base.locate_package`, which is how `pkgdata` gets initialized, might strip pieces of the path.
        # For example, on Travis macOS the paths returned by `abspath`
        # can be preceded by "/private" which is not present in the value returned by `Base.locate_package`.
        idx = findfirst(basedir(pkgdata), filename)
        if idx !== nothing
            idx = first(idx)
            if idx > 1
                filename = filename[idx:end]
            end
            filename = relpath_safe(filename, basedir(pkgdata))
        end
    elseif startswith(filename, "compiler")
        # Core.Compiler's pkgid includes "compiler/" in the path
        filename = relpath(filename, "compiler")
    end
    return filename
end

function fileindex(info::PkgData, file::AbstractString)
    for (i, f) in enumerate(srcfiles(info))
        String(f) == String(file) && return i
    end
    return nothing
end

function hasfile(info::PkgData, file::AbstractString)
    if isabspath(file)
        file = relpath(file, info)
    end
    fileindex(info, file) !== nothing
end

function fileinfo(pkgdata::PkgData, file::AbstractString)
    i = fileindex(pkgdata, file)
    i === nothing && error("file ", file, " not found")
    return pkgdata.fileinfos[i]
end
fileinfo(pkgdata::PkgData, i::Int) = pkgdata.fileinfos[i]

function Base.push!(pkgdata::PkgData, pr::Pair{<:AbstractString,FileInfo})
    push!(srcfiles(pkgdata), pr.first)
    push!(pkgdata.fileinfos, pr.second)
    return pkgdata
end

function pkgfileless((pkgdata1,file1)::Tuple{PkgData,String}, (pkgdata2,file2)::Tuple{PkgData,String})
    # implements a partial order
    PkgId(pkgdata1) ∈ pkgdata2.requirements && return true
    PkgId(pkgdata1) == PkgId(pkgdata2) && return fileindex(pkgdata1, file1)::Int < fileindex(pkgdata2, file2)::Int
    return false
end
