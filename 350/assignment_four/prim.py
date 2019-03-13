from graph import Graph


def prim(graph: Graph):
    """
    This function implements Prim's minimum spanning tree algorithm.

    :param graph: An instance of `Graph` initialized with a graph.
    :return: A tuple of the form (starting node, list of steps taken in traversal)
    :rtype: tuple(str, list)
    """
    visited = set()

    # Store tree as a list to preserve order
    minimum_spanning_tree = []

    # Start with arbitrary vertex from vertices
    # see https://stackoverflow.com/questions/1619514/how-to-extract-the-member-from-single-member-set-in-python
    starting_node = next(iter(graph.get_vertices()))
    visited.add(starting_node)

    # Stop looping after visiting all the vertices
    while len(visited) is not len(graph.get_vertices()):
        min_edges = set()

        for vertex in visited:
            for new_vertex in graph.get_vertices() - visited:

                # New edge is tuple in the form of `(start vertex, end vertex, weight)
                new_edge = graph.get_edge(vertex, new_vertex)

                # Ensure an edge exists
                if new_edge[2] is not None:
                    min_edges.add(new_edge)

        # Find the edge with the least weight
        min_edge = min(min_edges, key=lambda e: e[2])

        # Add it to the MST
        minimum_spanning_tree.append(min_edge)

        # Update visited set with it 
        visited.add(min_edge[1])

    return starting_node, minimum_spanning_tree
