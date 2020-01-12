# main creation method
function prepare_frame(
  mi::MethodInstance,
  slottypes::Vector,
  parentframe::Union{Nothing,Frame} = nothing,
)
  if parentframe !== nothing
    # a frame of for this method instance has already been or will be profiled,
    # so let's just trust the inference result for its return type and allow TP
    # to avoid entering into recursive calls.
    h = hash(mi)
    h in keys(parentframe.profiled) && return parentframe.profiled[h]
  end

  src = typeinf_ext(mi, Base.get_world_counter())
  # keep inference result
  parentframe !== nothing && push!(parentframe.profiled, h => src.rettype)

  # XXX: is this really valid ?
  m = mi.def::Method
  sparams = rewrap_unionall.(mi.sparam_vals, m.sig)
  caller = parentframe === nothing ? nothing : begin
    lin = lineinfonode(parentframe)
    FrameChain(lin, parentframe)
  end
  return Frame(mi, src, slottypes, sparams, caller)
end

function prepare_frame(
  m::Method,
  @nospecialize(tt),
  sparams::SimpleVector,
  parentframe::Union{Nothing,Frame} = nothing,
)
  mi = specialize_method(m, tt, sparams)
  # XXX: is this really valid ?
  slottypes = rewrap_unionall.(unwrap_unionall(tt).parameters, Ref(tt))
  return prepare_frame(mi, slottypes, parentframe)
end

# this method is basically only for testing
# TODO: keyword args
function prepare_frame(f, args...)
  slottyps = Type[typeof(f), typeof.(args)...]
  tt = to_tt(slottyps)
  mms = matching_methods(tt)
  @assert (n = length(mms)) === 1 "$(n === 0 ? "no" : "multiple") methods found: $tt"
  tt, sparams::SimpleVector, m::Method = mms[1]
  mi = specialize_method(m, tt, sparams)
  return prepare_frame(mi, slottyps)
end
