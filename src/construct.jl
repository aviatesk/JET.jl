function prepare_frame(
  mi::MethodInstance,
  slottypes::Vector,
  parentframe::Union{Nothing,Frame} = nothing,
)
  scope = mi.def::Method
  src = typeinf_ext(mi, Base.get_world_counter())
  # XXX: is this really valid ?
  sparams = rewrap_unionall.(mi.sparam_vals, scope.sig)
  caller = parentframe === nothing ? nothing : begin
    lin = lineinfonode(parentframe)
    FrameChain(lin, parentframe)
  end
  return Frame(scope, src, slottypes, sparams, caller)
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
