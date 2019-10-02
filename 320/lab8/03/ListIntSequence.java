public class ListIntSequence implements IntSequence {
  private static class Node {
    int item;
    Node next;
    Node prev;

    Node(Node prev, int element, Node next) {
      this.item = element;
      this.next = next;
      this.prev = prev;
    }
  }

  private Node first;
  private Node last;
  private int size;

  ListIntSequence() {
    first = null;
    last = null;
    size = 0;
  }

  public void append(int e) {
    final Node l = last;
    final Node n = new Node(l,e,null);
    last = n;
    if (l == null) {
      first = n;
    } else {
      l.next = n;
    }
    size++;
  }

  public void prepend(int e) {
    final Node f = first;
    final Node n = new Node(null,e,f);
    first = n;
    if (f == null) {
      last = n;
    } else {
      f.prev = n;
    }
    size++;
  }

  public int size() {
    return size;
  }

  public int get(int index) {
    if (index < 0 || index >= size) {
      throw new IllegalArgumentException("index " + index  + " out of range");
    } else if (index < (size >> 1)) {
      Node x = first;
      for (int i = 0; i < index; i++) {
        x = x.next;
      }
      return x.item;
    } else {
      Node x = last;
      for (int i = size - 1; i > index; i--) {
        x = x.prev;
      }
      return x.item;
    }
  }
}
