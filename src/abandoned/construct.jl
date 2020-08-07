# construct frames
# ----------------

# from method instance
prepare_frame(
  mi::MethodInstance, slottypes::Vector, ::Nothing = nothing
)::Union{Frame,Type} = prepare_frame_from_mi(mi, slottypes)

function prepare_frame(
  mi::MethodInstance, slottypes::Vector, parentframe::Frame
)::Union{Frame,Type}
  # if frame for this method instance has already been or will be profiled, let's just trust
  # the inference result for its return type in order to allow TP to avoid entering into
  # recursive calls
  h = hash(mi)
  h in keys(parentframe.profiled) && return parentframe.profiled[h]

  return prepare_frame_from_mi(mi, slottypes, parentframe)
end

# from method call
function prepare_frame(
  m::Method,
  @nospecialize(tt),
  sparams::SimpleVector,
  parentframe::Union{Nothing,Frame} = nothing,
)::Union{Frame,Type}
  mi = specialize_method(m, tt, sparams)
  # XXX: tt can include UnionAll, so unwrap. But, is this really valid ?
  slottypes = rewrap_unionall.(unwrap_unionall(tt).parameters, Ref(tt))
  return prepare_frame(mi, slottypes, parentframe)
end

function prepare_frame_from_mi(
  mi::MethodInstance, slottypes::Vector,
  parentframe::Union{Nothing,Frame} = nothing;
  # caller::Union{Nothing,FrameChain} = nothing,
  generator::Bool = false, istoplevel::Bool = false,
)
  caller, reports, profiled = frame_chain_info(parentframe)

  src = typeinf_ext(mi, Base.get_world_counter())

  sparams = get_sparams(mi)
  ssavaluetypes = Vector{Type}(undef, length(src.ssavaluetypes))
  nstmts = length(ssavaluetypes)

  push!(profiled, hash(mi) => src.rettype) # cache current type inference result (i.e. return type) into the cache

  return Frame(
    reports, profiled, mi, src, slottypes, sparams, nstmts, generator, istoplevel, 1,
    ssavaluetypes, Union{}, caller, nothing
  )
end

# caller, frame cache, reports
frame_chain_info(::Nothing) = nothing, ErrorReport[], Dict{UInt64,Type}()

function frame_chain_info(parentframe::Frame)
  lin = lineinfonode(parentframe)
  caller = FrameChain(lin, parentframe)
  return caller, parentframe.reports, parentframe.profiled
end

# XXX: is this really valid ?
function get_sparams(mi::MethodInstance)
  m = mi.def::Method
  return rewrap_unionall.(mi.sparam_vals, m.sig)
end

# TODO: handle toplevel frame
# function prepare_frame_from_module(
#   scope::Module, parentframe::Union{Nothing,Frame};
#   generator::Bool = false, istoplevel::Bool = false,
# )
#   error("profiling on toplevel frame not implemented.")
# end
