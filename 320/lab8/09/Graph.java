// Abstract Data Type for undirected graphs with vertices of type V
// Self-loops are allowed, but parallel edges are not

interface Graph<V> {

  // Add a vertex. No-op if vertex already exists.
  void addVertex(V v);

  // Return all the vertices 
  Iterable<V> vertices();

  // Return the number of vertices.
  int vertexCount();

  // Answer whether a particular vertex is in the graph
  boolean hasVertex(V v);

  // Add an edge between two vertices.
  // Raises IllegalArgumentException if either vertex is not in graph
  // No-op if edge already exists
  void addEdge(V v1, V v2);

  // Return the neighbors of a vertex
  // Raises IllegalArgumentException if vertex is not in graph
  Iterable<V> neighbors(V v);

  // Return the degree (number of neighbors) of a vertex
  // Raises IllegalArgumentException if vertex is not in graph
  int degree(V v);
}
