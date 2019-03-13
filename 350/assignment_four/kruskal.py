from graph import Graph


class UnionFind(object):
    """
    This class implements and manages a union find data structure used primarily by the `kruskal` function.

    It has been implemented predominantly by use of the Union Find page on wikipedia:
    https://en.wikipedia.org/wiki/Disjoint-set_data_structure

    One major difference from a traditional implementation is that it does not use a set but rather a
    dictionary as its underlying data representation layer.

    It's `find` method uses path compression and its `union` method merges based on size (for no specific reason).
    """
    def __init__(self, vertices):
        self.uf_set = {}

        # Create a unique id for each "set" in the "set"; set each set's parent to itself
        for _id, vertex in enumerate(vertices):
            self.uf_set[vertex] = {
                    "id": _id,
                    "parent": vertex,
                    "size": 1
                }

    def _get_uf_item(self, x: str):
        """
        This is an internal function that returns the attributes of an item in the "set".

        :param x: The name of the item in the set.
        :return: The attributes of `x`
        ":rtype: dict
        """
        return self.uf_set[x]

    def union(self, x: str, y: str):
        """
        This function merges two disjoint sets together based on the size of each set.
        From the Wikipedia article:
            "Union by size always attaches the tree with fewer elements to the root of the tree having more elements."

        :param x: The name of the "set" to union with `y`
        :param y: The name of the "set" to union with `x`
        :return: Nothing
        ":rtype: None
        """
        # Get the root of each set
        x_root = self._get_uf_item(self.find(x))
        y_root = self._get_uf_item(self.find(y))

        # If they share the same; they've already been merged
        if x_root == y_root:
            return

        # Switch mergee with merger if mergee is larger
        if x_root["size"] < y_root["size"]:
            x_root, y_root = y_root, x_root

        # Merge and update size of new "set"
        y_root["parent"] = x_root["parent"]
        x_root["size"] += y_root["size"]

    def find(self, x: str):
        """
        This function finds the root of set (i.e. node) in the `uf_set`.

        :param x: The name of the "set" for which to find its parent
        :return: The name of the parent of `x`
        :rtype: str
        """

        # Get "set" of attributes for `x`
        uf_item = self._get_uf_item(x)

        # Recursively walk up the set tree until we find `x`
        if uf_item["parent"] is not x:
            uf_item["parent"] = self.find(uf_item["parent"])
        return uf_item["parent"]


def kruskal(graph: Graph):
    """
    This function implements Kruskal's minimum spanning tree algorithm.

    :param graph: An instance of `Graph` initialized with a graph.
    :return: A tuple of the form (starting node, list of steps taken in traversal)
    :rtype: tuple(str, list)
    """
    minimum_spanning_tree = []

    # Get all unique edges in the graph and sort them from least to greatest
    unique_edges = graph.get_all_edges()
    sorted_edge_queue = sorted(unique_edges, key=lambda e: e[2])

    # First node of first edge
    starting_node = sorted_edge_queue[0][0]

    # Initialize union find data structure with each vertex in the graph
    uf_ds = UnionFind(graph.get_vertices())

    # Iterate through all edges starting at the least weighted.
    for edge in sorted_edge_queue:
        v1, v2, weight = edge

        # Use union find data structure to determine if the v1 -> v2 edge is acyclic.
        if uf_ds.find(v1) is not uf_ds.find(v2):
            # If not cyclic, merge and continue
            uf_ds.union(v1, v2)

            # Add the edge to the MST
            minimum_spanning_tree.append(edge)

    return starting_node, minimum_spanning_tree
