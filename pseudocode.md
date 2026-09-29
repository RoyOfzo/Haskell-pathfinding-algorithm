Input:
Graph with nodes and edges, where each edge has a weight. This weight is the cost of moving from one node to the other

Output: Shortest path from a starting node to a target node.

```

Function Dijkstra(graph, source){
    ForEach(Vertex v in graph) {
        Distance[v] = INFINITY
        Previous[v] = UNDEFINED
        add v to Q
        distance[source] = 0
    }

    while(Q is not empty) {
        u = vertex in Q with min distance[u]
        remove u from Q

        ForEach(Neighbor v of u still in Q) {
            alt = distance[u] + length(u, v)
            if(alt < distance[v]) {
                distance[v] = alt
                previous[v] = u
            }
        }
    }
    Return distance[], previous[]
}

```    
