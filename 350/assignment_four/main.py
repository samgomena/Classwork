from argparse import ArgumentParser

from graph import Graph
from prim import prim

parser = ArgumentParser(description='Calculate minimum spanning trees with different algorithms')

parser.add_argument("-f", "--file",
                    type=str,
                    default="city_pairs.txt",
                    dest="graph_file",
                    action="store",
                    help="A file containing a textual representation of a graph")

parser.add_argument("-c", "--cumulative",
                    default=True,
                    dest="show_cumulative",
                    action="store_true",
                    help="Show cumulative distance travelled for each step")


def normalize_name(name):
    return name.replace('.', ' ').capitalize()


def main():
    args = parser.parse_args()

    graph = Graph()

    # graph.load_graph_from_file(args.graph_file)
    graph.load_graph_from_file("./test/test_pairs_2.txt")

    starting_node, prim_mst = prim(graph)
    total_weight = sum([edge[2] for edge in prim_mst])

    assert len(prim_mst) == len(set([edge[1] for edge in prim_mst])), f"Oops, looks like you visited a city twice"

    print(f"Starting graph traversal at {normalize_name(starting_node)}")
    print(prim_mst)

    if args.show_cumulative:
        total_distance = 0
        for edge_travelled in prim_mst:
            print(total_distance)
            total_distance += edge_travelled[2]

    print(f"Total distance traveled was {total_weight}")


if __name__ == '__main__':
    main()
