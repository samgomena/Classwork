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
    return name.replace('.', ' ').title()


def print_mst_steps(name, mst):
    """
    Helper function to print the steps taken by an mst algorithm to generate an MST.

    It is meant to consume the output of `prim` and `kruskal`

    :return: A tuple of the form (starting node, list of steps taken in traversal)
    :return: Nothing
    :rtype: None
    """
    starting_node, mst_steps = mst

    total_distance = sum([edge[2] for edge in mst_steps])
    cumulative_distance = 0

    print(f"{normalize_name(name)}'s algorithm started traversal at {normalize_name(starting_node)}", end="\n\n")

    for edge_travelled in mst_steps:
        print(f"Moved from {normalize_name(edge_travelled[0])} -> {normalize_name(edge_travelled[1])}")
        print(f"\t(travelled {cumulative_distance} so far)")
        cumulative_distance += edge_travelled[2]

    print()
    print(f"Total distance traveled using {normalize_name(name)}'s was {total_distance}", end="\n\n")


def main():
    args = parser.parse_args()

    graph = Graph()

    # Defaults to city_pairs.txt
    graph.load_graph_from_file(args.graph_file)

    prim_starting_node, prim_mst = prim(graph)
    kruskal_starting_node, kruskal_mst = kruskal(graph)

    if args.show_cumulative:
        print_mst_steps("prim", (prim_starting_node, prim_mst))
        print_mst_steps("kruskal", (kruskal_starting_node, kruskal_mst))


if __name__ == '__main__':
    main()
