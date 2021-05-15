# computes strongly connected components of a control flow graph `cfg`
# NOTE adapted from https://github.com/JuliaGraphs/LightGraphs.jl/blob/6134e21484f2312b33ba753d47dc2fc4d6b35fac/src/connectivity.jl#L221-L311
# since to load an entire LightGraphs.jl is a bit cost-ineffective in terms of a trade-off of latency vs. maintainability
function strongly_connected_components(g::CFG)
    T = Int
    zero_t = zero(T)
    one_t = one(T)
    nvg = nv(g)
    count = one_t


    index = zeros(T, nvg)         # first time in which vertex is discovered
    stack = Vector{T}()           # stores vertices which have been discovered and not yet assigned to any component
    onstack = zeros(Bool, nvg)    # false if a vertex is waiting in the stack to receive a component assignment
    lowlink = zeros(T, nvg)       # lowest index vertex that it can reach through back edge (index array not vertex id number)
    parents = zeros(T, nvg)       # parent of every vertex in dfs
    components = Vector{Vector{T}}()    # maintains a list of scc (order is not guaranteed in API)


    dfs_stack = Vector{T}()

    @inbounds for s in vertices(g)
        if index[s] == zero_t
            index[s] = count
            lowlink[s] = count
            onstack[s] = true
            parents[s] = s
            push!(stack, s)
            count = count + one_t

            # start dfs from 's'
            push!(dfs_stack, s)

            while !isempty(dfs_stack)
                v = dfs_stack[end] #end is the most recently added item
                u = zero_t
                @inbounds for v_neighbor in outneighbors(g, v)
                    if index[v_neighbor] == zero_t
                        # unvisited neighbor found
                        u = v_neighbor
                        break
                        #GOTO A push u onto DFS stack and continue DFS
                    elseif onstack[v_neighbor]
                        # we have already seen n, but can update the lowlink of v
                        # which has the effect of possibly keeping v on the stack until n is ready to pop.
                        # update lowest index 'v' can reach through out neighbors
                        lowlink[v] = min(lowlink[v], index[v_neighbor])
                    end
                end
                if u == zero_t
                    # All out neighbors already visited or no out neighbors
                    # we have fully explored the DFS tree from v.
                    # time to start popping.
                    popped = pop!(dfs_stack)
                    lowlink[parents[popped]] = min(lowlink[parents[popped]], lowlink[popped])

                    if index[v] == lowlink[v]
                        # found a cycle in a completed dfs tree.
                        component = Vector{T}()

                        while !isempty(stack) #break when popped == v
                            # drain stack until we see v.
                            # everything on the stack until we see v is in the SCC rooted at v.
                            popped = pop!(stack)
                            push!(component, popped)
                            onstack[popped] = false
                            # popped has been assigned a component, so we will never see it again.
                            if popped == v
                                # we have drained the stack of an entire component.
                                break
                            end
                        end

                        reverse!(component)
                        push!(components, component)
                    end

                else #LABEL A
                    # add unvisited neighbor to dfs
                    index[u] = count
                    lowlink[u] = count
                    onstack[u] = true
                    parents[u] = v
                    count = count + one_t

                    push!(stack, u)
                    push!(dfs_stack, u)
                    # next iteration of while loop will expand the DFS tree from u.
                end
            end
        end
    end

    # # assert with the original implementation
    # components′ = LightGraphs.strongly_connected_components(cfg_to_sdg(g))
    # @assert Set(Set.(components)) == Set(Set.(components′))

    return components
end

# compatibility with LightGraphs.jl interfaces
@inline nv(cfg::CFG) = length(cfg.blocks)
@inline vertices(cfg::CFG) = 1:nv(cfg)
@inline outneighbors(cfg::CFG, v) = cfg.blocks[v].succs

# import LightGraphs: SimpleDiGraph, add_edge!, LightGraphs
# function cfg_to_sdg(cfg::CFG)
#     g = SimpleDiGraph(length(cfg.blocks))
#     for (v, block) in enumerate(cfg.blocks)
#         for succ in block.succs
#             add_edge!(g, v, succ)
#         end
#     end
#     return g
# end
