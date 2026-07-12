"Heap-allocated collections that get used by `flood_fill()`"
struct FloodFillAllocations{TNode, TNodeSet<:AbstractSet{TNode}, TResizableNodeList<:AbstractVector{TNode}}
    visited_nodes::TNodeSet
    interesting_nodes::TResizableNodeList
end
@inline FloodFillAllocations(TNode) = FloodFillAllocations(Set{TNode}(), preallocated_vector(TNode, 256))
flood_fill_node_type(::FloodFillAllocations{TNode}) where {TNode} = TNode


"
Splits a graph into distinct areas, using your lambdas to control the entire process.

This process follows a deterministic order -- with the same seeds and connection logic,
  you always get the same ordered output.
The search is depth-first through the outputs of `fn_get_connections`,
  always starting with the last connection first.

Graph nodes should be represented by some kind of hashable and equatable type
  (unique integer ID, pixel coordinate, mutable node reference, etc).
You also must provide a callback to get the connections from a given node.

The `seeds` input is an enumeration of nodes (really any data convertible to your node type)
  that are used to execute the flood-fill.
For example, to fill a single area connecting to one node, pass a 1-tuple of that node.

The output is evaluated using 2 or 3 lambdas provided by you --
  'start new area', 'add node to area', and optionally 'end area'.
Each new area is guaranteed to have at least one node in it.
"
function flood_fill(seeds,
                    fn_get_connections, # (TNode, TResizableNodeList) -> Nothing
                    fn_start_area, # () -> Nothing
                    fn_add_node_to_area, # (TNode) -> Nothing
                    fn_end_area = () -> nothing,
                    buffers::FloodFillAllocations{TNode} = FloodFillAllocations(eltype(seeds))
                   )::Nothing where {TNode}
    empty!(buffers.visited_nodes)
    empty!(buffers.interesting_nodes)

    # Go through every seed node, and if it hasn't already been put into a group,
    #   make it the start of a new one.
    for seed in seeds
        node = convert(TNode, seed)
        (node in buffers.visited_nodes) && continue

        fn_start_area()

        push!(buffers.interesting_nodes, node)
        while !isempty(buffers.interesting_nodes)
            neighbor_node::TNode = pop!(buffers.interesting_nodes)
            (neighbor_node in buffers.visited_nodes) && continue

            fn_add_node_to_area(neighbor_node)
            push!(buffers.visited_nodes, neighbor_node)

            fn_get_connections(neighbor_node, buffers.interesting_nodes)
        end

        fn_end_area()
    end

    return nothing
end

export flood_fill, FloodFillAllocations