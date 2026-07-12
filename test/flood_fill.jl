# Test flood-fill on a 2D grid of integer values.
# Nodes are connected if they are orthogonally-adjacent and have the same value.
const FF_MAP = [
    # Remember that Row is the first (X) axis!
    0 0 1 1 1 0 0 3
    0 0 0 0 1 1 2 3
    0 1 1 0 0 2 2 2
    2 2 2 2 2 2 2 2
]
const FFNodeID = v2i
function ff_connections(coord::FFNodeID, out_vector)
    try_conn(offset) = if all(coord + offset > 0) && all(coord + offset <= vsize(FF_MAP)) &&
                          (FF_MAP[coord + offset] == FF_MAP[coord])
        push!(out_vector, coord + convert(v2i, offset))
    end
    try_conn(v2i(-1, 0)) # 1) above
    try_conn(v2i(1, 0)) # 2) below
    try_conn(v2i(0, -1)) # 3) behind
    try_conn(v2i(0, 1)) # 4) ahead
    return nothing
end
# Provide every single index as a seed (note X is the inner axis here).
const FF_SEEDS = convert.(Ref(v2i), Vec.(Tuple.(eachindex(CartesianIndices(FF_MAP), FF_MAP))))
# For the test, log each event as it happens.
const FLOOD_FILL_EXPECTED_EVENTS = Union{v2i, Val{:start}, Val{:end}}[
    Val(:start), v2i(1, 1), v2i(1, 2),
                            v2i(2, 2), v2i(2, 3), v2i(2, 4),
                                                  v2i(3, 4), v2i(3, 5),
                 v2i(2, 1),
                 v2i(3, 1),
    Val(:end),

    Val(:start), v2i(4, 1), v2i(4, 2), v2i(4, 3), v2i(4, 4), v2i(4, 5), v2i(4, 6), v2i(4, 7), v2i(4, 8),
                                                                                              v2i(3, 8), v2i(3, 7), v2i(3, 6),
                                                                                                         v2i(2, 7),
    Val(:end),

    Val(:start), v2i(3, 2), v2i(3, 3), Val(:end),

    Val(:start), v2i(1, 3), v2i(1, 4), v2i(1, 5),
                                       v2i(2, 5), v2i(2, 6),
    Val(:end),

    Val(:start), v2i(1, 6), v2i(1, 7), Val(:end),
    Val(:start), v2i(1, 8), v2i(2, 8), Val(:end)
]
@bp_test_no_allocations_setup(
    begin
        ff_actual_events = preallocated_vector(
            Union{v2i, Val{:start}, Val{:end}},
            length(FLOOD_FILL_EXPECTED_EVENTS)
        )
        ff_allocs = FloodFillAllocations(v2i)
    end,
    begin
        empty!(ff_actual_events)
        flood_fill(FF_SEEDS, ff_connections,
                   () -> push!(ff_actual_events, Val(:start)),
                   i  -> push!(ff_actual_events, i),
                   () -> push!(ff_actual_events, Val(:end)),
                   ff_allocs)
        ff_actual_events
    end,
    FLOOD_FILL_EXPECTED_EVENTS
)