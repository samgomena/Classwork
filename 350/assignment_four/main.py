from argparse import ArgumentParser

from graph import Graph
from prim import prim
from kruskal import kruskal

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


def print_mst_steps(mst):
    starting_node, mst_steps = mst

    total_distance = sum([edge[2] for edge in mst_steps])
    cumulative_distance = 0

    print(f"Starting traversal at {normalize_name(starting_node)}", end="\n\n")

    for edge_travelled in mst_steps:
        print(f"Travelled {cumulative_distance} so far")
        cumulative_distance += edge_travelled[2]

    print(f"Total distance traveled was {total_distance}", end="\n\n")


def main():
    args = parser.parse_args()

    graph = Graph()

    # graph.load_graph_from_file(args.graph_file)
    graph.load_graph_from_file("./test/test_pairs_1.txt")

    prim_starting_node, prim_mst = prim(graph)
    assert len(prim_mst) == len(set([edge[1] for edge in prim_mst])), f"Oops, looks like you visited a city twice"

    kruskal_starting_node, kruskal_mst = kruskal(graph)

    if args.show_cumulative:
        print_mst_steps(prim(graph))
        # print_mst_steps((prim_starting_node, prim_mst))
        print_mst_steps((kruskal_starting_node, kruskal_mst))


if __name__ == '__main__':
    main()
