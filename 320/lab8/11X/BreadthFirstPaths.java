import java.util.*;

// Compute breadth-first paths on graphs.
// Closely inspired by https://algs4.cs.princeton.edu/41graph/BreadthFirstPaths.java
//  Copyright © 2000–2017, Robert Sedgewick and Kevin Wayne. 

public class BreadthFirstPaths<V> {
  private static final int INFINITY = Integer.MAX_VALUE;

  private Graph<V> g; // the underlying graph

  // After bfs, this maps each visited vertex v to the previous vertex in (some) shortest path
  // from source to v.  The source itself is mapped to null. Unvisited nodes are not mapped at all.
  private Map<V,V> previous; 

  BreadthFirstPaths(Graph<V> g, V s) {
    if (!g.hasVertex(s)) {
      throw new IllegalArgumentException("Vertex " + s  + " is not in graph");
    }
    this.g = g;
    previous = new HashMap<V,V>();
    bfs(g,s);
  }

  private void bfs(Graph<V> g, V s) {
    Queue<V> q = new LinkedList<V>();
    previous.put(s,null);
    q.add(s);
    while (!q.isEmpty()) {
      V v = q.remove();
      for (V w : g.neighbors(v)) {
        if (!previous.containsKey(w)) {
          previous.put(w,v);
          q.add(w);
        }
      }
    }
  }

  public boolean hasPathTo(V v) {
    if (!g.hasVertex(v)) {
      throw new IllegalArgumentException("Vertex " + v  + " is not in graph");
    }
    return previous.containsKey(v);
  }

  public Deque<V> pathTo (V v) {
    if (!hasPathTo(v)) {
      return null;
    }
    Deque<V> path = new LinkedList<V>();
    while (v != null) {
      path.addFirst(v);
      v = previous.get(v);
    }
    return path;
  }

  public int distTo(V v) {
    Deque<V> path = pathTo(v);
    if (path != null) {
      return path.size() - 1;
    } else {
      return INFINITY;
    }
  }
}
