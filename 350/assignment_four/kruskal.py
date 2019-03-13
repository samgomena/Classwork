
class UnionFind(object):
    def __init__(self, vertices):
        self.uf_set = {}

        for _id, vertex in enumerate(vertices):
            self.uf_set[vertex] = {
                    "id": _id,
                    "name": vertex,
                    "parent": vertex,
                    "_size": 1
                }

    def _get_uf_item(self, x):
        return self.uf_set[x]

    @staticmethod
    def make_set(x):
        return set(x)

    def union(self, x, y):
        x_root = self._get_uf_item(self.find(x))
        y_root = self._get_uf_item(self.find(y))

        if x_root == y_root:
            return

        if x_root["_size"] < y_root["_size"]:
            x_root, y_root = y_root, x_root

        y_root["parent"] = x_root["parent"]
        x_root["_size"] += y_root["_size"]

    def find(self, x):
        uf_item = self._get_uf_item(x)
        if uf_item["parent"] is not x:
            uf_item["parent"] = self.find(uf_item["parent"])
        return uf_item["parent"]


def kruskal(graph):
    minimum_spanning_tree = []

    unique_edges = graph.get_all_edges()
    sorted_edge_queue = sorted(unique_edges, key=lambda e: e[2])

    # First node of first edge
    starting_node = sorted_edge_queue[0][0]

    # print(sorted_edge_queue)
    # print(f"{unique_edges}\n{len(unique_edges)}")

    uf_ds = UnionFind(graph.get_vertices())

    for edge in sorted_edge_queue:
        v1, v2, weight = edge

        if uf_ds.find(v1) is not uf_ds.find(v2):
            uf_ds.union(v1, v2)
            # minimum_spanning_tree.add(edge)
            minimum_spanning_tree.append(edge)

    return starting_node, minimum_spanning_tree
