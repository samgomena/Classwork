import unittest
from graph import Graph
from prim import prim
from kruskal import kruskal


class TestGraph(unittest.TestCase):

    def setUp(self):
        self.graph_file = "city_pairs.txt"
        self.graph = Graph()
        self.graph.load_graph_from_file(self.graph_file)

    def tearDown(self):
        del self.graph

    def test_vertices(self):
        self.assertEqual(len(self.graph.get_vertices()), 29)
        self.assertEqual(type(self.graph.get_vertices()), set)

    def test_prim(self):
        _, steps = prim(self.graph)
        total_distance = sum([edge[2] for edge in steps])

        if self.graph_file == "city_pairs.txt":
            self.assertEqual(total_distance, 1325)

    def test_kruskal(self):
        _, steps = kruskal(self.graph)
        total_distance = sum([edge[2] for edge in steps])

        if self.graph_file == "city_pairs.txt":
            self.assertEqual(total_distance, 1325)


if __name__ == '__main__':
    unittest.main()
