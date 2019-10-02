import java.util.*;

public class AdjSetDirectedGraph<V> extends AdjSetGraph<V> implements Graph<V> {
    public void addEdge(V v1, V v2) {
        Set<V> adjs1 = adjacents(v1);
        if(!v1.equals(v2)) {
            adjs1.add(v2);
        }
    }
}