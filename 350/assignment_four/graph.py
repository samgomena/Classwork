import pprint


class Graph(object):

    def __init__(self, graph_input: dict={}):

        self.adj_matrix = None
        self._vertices = None
        self._edges = None
        self.graph = graph_input

        if not isinstance(graph_input, dict):
            raise TypeError(f"Graph must be of type `dict` not {type(graph_input)}")

    def print(self):
        pp = pprint.PrettyPrinter(indent=2, compact=True)
        pp.pprint(self.graph)

    def load_graph_from_file(self, filename: str):
        with open(filename) as graph_file:
            for line in graph_file:
                # Remove newline from end of line and split into vertices and weight between them
                city1, city2, edge_weight = line.strip("\n").split(" ")

                # Create a vertex connected to each other vertex with weights as values
                self.graph.setdefault(city1, {}).update({city2: int(edge_weight)})

        self._vertices = []  # sorted(self.graph.keys())

        for i, node in enumerate(sorted(self.graph.keys())):
            self._vertices.append((node, i))

        self.adj_matrix = [0] * (len(self._vertices) ** 2)

    def get_vertices(self):
        return set(self.graph.keys())

    def _get_connections(self):
        return self._vertices.items()

    def get_weight(self, v1, v2):
        return self.graph.get(v1).get(v2)

    def get_adjacent_vertices(self, v1):
        if type(v1) is str:
            return self.graph.get(v1).keys()
        else:
            vertices_list = set()
            for vertex in v1:
                vertices_list.update(set(self.get_adjacent_vertices(vertex)))
            return vertices_list

    def get_edges(self, v1):
        edges = self.graph.get(v1).items()
        return [(v1, v2, weight) for v2, weight in edges]

    def get_edge(self, v1, v2):
        return v1, v2, self.graph.get(v1).get(v2)

    def get_min_edge(self, v1):
        starting_vertex = self.graph.get(v1)
        closest_vertex = min(starting_vertex, key=starting_vertex.get)
        return v1, closest_vertex, starting_vertex.get(closest_vertex)
