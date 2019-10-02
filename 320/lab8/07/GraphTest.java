public class GraphTest {
  public static void main(String argv[]) throws Exception {
    Graph<Integer> g = GraphUtils.emptyGraph();
    g.addVertex(10);
    g.addVertex(20);
    g.addVertex(30);
    g.addEdge(10,20);
    g.addEdge(10,30);
    g.addEdge(20,30);
    System.out.println("Stage 1:");
    System.out.print(GraphUtils.dumpGraph(g));
    System.out.println("Graph contains vertex 10: " + g.hasVertex(10));
    System.out.println("Graph contains vertex 50: " + g.hasVertex(50));
    System.out.println("Degree of vertex 10 = " + g.degree(10)); 
    System.out.println("Degree of vertex 30 = " + g.degree(30)); 
    g.addVertex(30);
    g.addVertex(40);
    g.addEdge(10,40);
    g.addEdge(10,30);
    g.addVertex(50);
    System.out.println("Stage 2:");
    System.out.print(GraphUtils.dumpGraph(g));
    System.out.println("Graph contains vertex 10: " + g.hasVertex(10));
    System.out.println("Graph contains vertex 50: " + g.hasVertex(50));
    System.out.println("Degree of vertex 10 = " + g.degree(10)); 
    System.out.println("Degree of vertex 30 = " + g.degree(30)); 
  }
}
