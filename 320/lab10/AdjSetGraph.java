import java.util.*;

class  AdjSetGraph<V> implements Graph<V> {
  private Map<V,Set<V>> vertices;

  // Construct an empty graph.
  public AdjSetGraph() {
    vertices = new HashMap<V,Set<V>>();
  }

  // Add a vertex. No-op if vertex already exists.
  public void addVertex(V v) {
    if (!vertices.containsKey(v)) {
      vertices.put(v, new HashSet<V>());
    }
  }

  // Return all the vertices 
  public Iterable<V> vertices() {
    return vertices.keySet();
  }

  // Return the number of vertices.
  public int vertexCount() {
    return vertices.size();
  }

  // Answer whether a particular vertex is in the graph
  public boolean hasVertex(V v) {
    return vertices.containsKey(v);
  }

  // Several of the following methods require that we throw an
  // IllegalArgumentException if a given vertex does not exist
  // in the graph.  To avoid duplicating code that checks for
  // that condition, we package it up here in a reusable private
  // method:
  protected Set<V> adjacents(V v) {
    if (vertices.containsKey(v)) {
      return vertices.get(v);
    }
    throw new IllegalArgumentException("vertex " + v + " is not in graph");
  }

  // Add an edge between two vertices.
  // Raises IllegalArgumentException if either vertex is not in graph
  // No-op if edge already exists
  // public void addEdge(V v1, V v2) {
  //   Set<V> adjs1 = adjacents(v1);   // Check that v1 is in the graph
  //   Set<V> adjs2 = adjacents(v2);   // Check that v2 is in the graph
  //   adjs1.add(v2);                  // Add an edge from v1 to v2
  //   if (!v1.equals(v2)) {
  //     adjs2.add(v1);                // And add an edge fromv v2 to v1 (if they are distinct)
  //   }
  // }
  public void addEdge(V v1, V v2) {
    Set<V> adjs1 = adjacents(v1);   // Check that v1 is in the graph
    Set<V> adjs2 = adjacents(v2);   // Check that v2 is in the graph
    adjs1.add(v2);                  // Add an edge from v1 to v2
    // if (!v1.equals(v2)) {
    //   adjs2.add(v1);                // And add an edge fromv v2 to v1 (if they are distinct)
    // }
  }

  // Return the neighbors of a vertex
  // Raises IllegalArgumentException if vertex is not in graph
  public Iterable<V> neighbors(V v) {
    return adjacents(v);
  }

  // Return the degree (number of neighbors) of a vertex
  // Raises IllegalArgumentException if vertex is not in graph
  public int degree(V v) {
    return adjacents(v).size();
  }

  // useful for debugging, once methods are implemented
  public String toString() {
    return GraphUtils.dumpGraph(this);
  }
}
