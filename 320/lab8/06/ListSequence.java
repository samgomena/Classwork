public class ListSequence<E> implements Sequence<E> {
  private static class Node<E> {
    E item;
    Node<E> next;
    Node<E> prev;

    Node(Node<E> prev, E element, Node<E> next) {
      this.item = element;
      this.next = next;
      this.prev = prev;
    }
  }

  private Node<E> first;
  private Node<E> last;
  private int size;

  ListSequence() {
    first = null;
    last = null;
    size = 0;
  }

  public void append(E e) {
    final Node<E> l = last;
    final Node<E> n = new Node<E>(l,e,null);
    last = n;
    if (l == null) {
      first = n;
    } else {
      l.next = n;
    }
    size++;
  }

  public void prepend(E e) {
    final Node<E> f = first;
    final Node<E> n = new Node<E>(null,e,f);
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

  public E get(int index) {
    if (index < 0 || index >= size) {
      throw new IllegalArgumentException("index " + index  + " out of range");
    if (index < (size >> 1)) {
      Node<E> x = first;
      for (int i = 0; i < index; i++) {
        x = x.next;
      }
      return x.item;
    } else {
      Node<E> x = last;
      for (int i = size - 1; i > index; i--) {
        x = x.prev;
      }
      return x.item;
    }
  }
}
