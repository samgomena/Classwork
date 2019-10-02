import java.io.*;
import java.util.*;

public abstract class GraphUtils {

  static <V> Graph<V> emptyGraph() {
      return new HiddenGraph<V>();
  }

  static <V> String dumpGraph(Graph<V> g) {
    StringBuilder s = new StringBuilder();
    s.append("Vertex count = " + g.vertexCount() + "\n");
    for (V v : g.vertices()) {
      s.append(v + ":");
      for (V w : g.neighbors(v)) {
        s.append(" " + w);
      }
      s.append("\n");
    }
    return s.toString();
  }

  static Graph<Integer> readIntegerGraph(String name) throws Exception {
    Scanner in = new Scanner(new FileReader(name + ".ig"));
    Graph<Integer> g = emptyGraph();
    in.nextLine(); // read and ignore V
    in.nextLine(); // read and ignore E
    while (in.hasNext()) {
      Integer v1 = in.nextInt();
      Integer v2 = in.nextInt();
      g.addVertex(v1); //might or might not already exist
      g.addVertex(v2);
      g.addEdge(v1,v2);
    }
    in.close();
    return g;
  }

  static Graph<String> readStringGraph(String name, String sep) throws Exception {
    Scanner in = new Scanner(new FileReader(name + ".sg"));
    Graph<String> g = emptyGraph();
    while (in.hasNext()) {
      String[] a = in.nextLine().split(sep);
      g.addVertex(a[0]); //might or might not already exist
      for (int i = 1; i < a.length; i++) {
        g.addVertex(a[i]);
        g.addEdge(a[0],a[i]);
      }
    }
    in.close();
    return g;
  }
}
