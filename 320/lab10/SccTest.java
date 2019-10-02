import java.util.*;

public class SccTest {
  public static void main(String argv[]) throws Exception {

    // Construct an empty directed graph:
    Graph<String> g = GraphUtils.emptyDirectedGraph();

    // Add the vertices and edges for the sample Graph on Slide 67
    // of the Week 5 lecture.  (This will be a good example for
    // initial testing, but do not limit yourself to just this
    // one example!)
    g.addVertex("a");
    g.addVertex("b");
    g.addVertex("c");
    g.addVertex("d");
    g.addVertex("e");
    g.addVertex("f");
    g.addVertex("g");
    g.addEdge("a", "b");
    g.addEdge("b", "c");
    g.addEdge("c", "b");
    g.addEdge("b", "d");
    g.addEdge("d", "e");
    g.addEdge("e", "f");
    g.addEdge("f", "d");
    g.addEdge("f", "g");

    // Print out a description of the graph so that we can check
    // it is correct.  The code below also generates a .dot version
    // of the graph so that you can visualize the structure of the
    // graph.  If you want to do this, run the command:
    //   dot -Tpdf -o graph.pdf graph.dot
    // to generate a pdf version of the graph.
    System.out.print(GraphUtils.dumpGraph(g));
    GraphUtils.directedToDot(g, "graph_normal");

    Graph<String> flippedG = GraphUtils.flip(g);
    System.out.print(GraphUtils.dumpGraph(flippedG));
    GraphUtils.directedToDot(flippedG, "graph_flipped");
    // Run the stronly connected components algorithm on this
    // graph and print out the results:
    List<List<String>> sccs = GraphUtils.scc(g);
    System.out.println("-----");
    System.out.println("Number of strongly connected components = " + sccs.size());
    for (List<String> scc : sccs) {
      System.out.print("strongly connected component:");
      for (String v : scc) {
        System.out.print(" " + v);
      }
      System.out.println();
    }
    System.out.println("-----");
  }
}
