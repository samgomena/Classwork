import unittest
from assignment_four.graph import Graph


class TestGraph(unittest.TestCase):

    def setup(self):
        self.graph = Graph()
        self.graph.load_graph_from_file("../city_pairs.txt")

    def tear_down(self):
        del self.graph

    def test_vertices(self):
        self.setup()

        self.assertEqual(len(self.graph.get_vertices()), 29)
        self.assertEqual(type(self.graph.get_vertices()), list)

        self.tear_down()


if __name__ == '__main__':
    unittest.main()
