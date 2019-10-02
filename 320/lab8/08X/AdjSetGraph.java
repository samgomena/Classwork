import java.util.*;

class  AdjSetGraph<V> implements Graph<V> {
  private Map<V,Set<V>> vertices;

    // complete the implementation by adding all the methods
    // defined in the Graph interface

    // (you may also wish to add private helper methods to perform
    //  tasks that are common to several public methods)

  // useful for debugging, once methods are implemented

  public AdjSetGraph() {
    verties = new HashMap<V, HashSet<V>>();
  }

  public void addVertex(V v) {
    if(!vertices.containsKey(v)) {
      vertices.put(v, new HashSet<V> ());
    }
  }

  public Iterable<V> vertices() {
    return vertices.keySet();
  }

  public int vertexCount() {
    return vertices.size();
  }

  public boolean hasVertex(V v) {
    return vertices.keySet().contains(v);
  }

  public void addEdge(V v1, V v2) {
    return;
  }

  public Iterable<V> neighbors(V v) {

    if(vertices.get(v) == null) {
      throw new IllegalArgumentException();
    }
    return vertices.get(v);
  }

  public int degree(V v) {
    if(vertices.get(v) == null) {
      throw new IllegalArgumentException();
    }
    return vertices.get(v).size();
  }

  public String toString() {
    return GraphUtils.dumpGraph(this);
  }
}
