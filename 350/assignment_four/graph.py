"""
This file implements a graph data structure meant to be consumed by the `prim` and `kruskal` functions.
"""

import pprint


class Graph(object):
    """
    This class implements and manages a `graph` data structure meant to mimic an adjacency matrix.

    Because it was designed with the intent to be utilized by both Prim's and Kruskal's algorithms
    the graph, structurally, is significantly different than an adjacency matrix.

    If provided, it is expected to look like this:
    {
        "node1": {
                "node2": edge_weight1,
                "node3": edge_weight2,
                ...
                "nodeN": edge_weightN
            },
        "node2": {
                "node1": edge_weight1,
                ...
            }
    }

    Where node names are strings and edge weights are integers (could also be floats, too)

    While visually different, indexing is effectively the same as an adjacency matrix but instead of using
    matrix indexes (e.g. `graph[0][1] -> edge weight`) we use the python dictionary __getitem__ operation
    (e.g. `graph.get(0).get(1) -> edge weight).

    Effectively, the only difference (outside of temporal and spatial performance) is that this returns `None`
    in place of 0 for an absent edge.
    """
    def __init__(self, graph_input: dict={}):

        self.adj_matrix = None
        self._vertices = None
        self._edges = None
        self.graph = graph_input

        if not isinstance(graph_input, dict):
            raise TypeError(f"Graph must be of type `dict` not {type(graph_input)}")

    def print(self):
        """
        This function pretty prints the graph similar to the example given in the class docstring.
        :return: Nothing
        :rtype: None
        """
        pp = pprint.PrettyPrinter(indent=2, compact=True)
        pp.pprint(self.graph)

    def load_graph_from_file(self, filename: str):
        """

        :param filename:
        :return:
        """
        with open(filename) as graph_file:
            for line in graph_file:
                # Remove newline from end of line and split into vertices and weight between them
                city1, city2, edge_weight = line.strip("\n").split(" ")

                # Create a vertex connected to each other vertex with weights as values
                self.graph.setdefault(city1, {}).update({city2: int(edge_weight)})

    def get_vertices(self):
        """
        This function returns the set of all vertices (or nodes) in the graph.

        Note: Because it's a set, every element is guaranteed to be unique.
        :return: The set of all vertices in the graph.
        ":rtype: set
        """
        return set(self.graph.keys())

    @staticmethod
    def _remove_duplicate_edges(edge_list: list):
        """
        This function removes duplicate edges from a list (or set) of edges.
        This is because edges are stored as tuples, so the edges
        edge1: ('a', 'b', 1), edge2: ('b', 'a', 1) are technically the same edge but python sees
        them as unique due to a tuple elements natural ordering.

        Note: This function is used primarily by Kruskal's algorithm as it focuses on traversing edges
        as opposed to vertices (as Prim's does).

        :param edge_list: A list containing possible duplicate edges.
        :return: The list of unique edges from `edge_list`.
        ":rtype: list
        """
        unique_edges = []
        for v1, v2, weight in edge_list:
            # Check if edge is a duplicate and has not been added to the unique list yet
            if (v2, v1, weight) in edge_list and (v2, v1, weight) not in unique_edges:
                unique_edges.append((v1, v2, weight))

        return unique_edges

    def get_all_edges(self, unique: bool=True):
        """
        This function gets all possible edges between all of the vertices in the graph.

        :param unique: Returns unique edges in the graph if true; all edges otherwise.
        :return: A list of all edges in the graph
        ":rtype: list
        """
        edges = []

        # For all of vertices in the graph
        for vertex in self.get_vertices():
            # Add all edges connected to each vertex
            edges.extend(self.get_edges(vertex))

        if unique:
            return self._remove_duplicate_edges(edges)
        else:
            return edges

    def get_edges(self, v1: str):
        """
        This function returns all of the edges connected to a vertex in the graph.

        :param v1: A vertex to get all the edges for.
        :return: A list of the edges between `v1` and all its connections.
        :rtype: list
        """
        edges = self.graph.get(v1).keys()

        return [self.get_edge(v1, v2) for v2 in edges]

    def get_edge(self, v1: str, v2: str):
        """
        This function returns the edge weight between two nodes in the graph.

        :param v1: A starting node
        :param v2: A node connected to `v1`
        :return: A tuple of the form (vertex 1, vertex 2, weight b/t vertex 1 and 2) if the connection exists;
            None otherwise
        :rtype: tuple
        """
        return v1, v2, self.graph.get(v1).get(v2)

    # def get_min_edge(self, v1):
    #     starting_vertex = self.graph.get(v1)
    #     closest_vertex = min(starting_vertex, key=starting_vertex.get)
    #
    #     return v1, closest_vertex, starting_vertex.get(closest_vertex)
