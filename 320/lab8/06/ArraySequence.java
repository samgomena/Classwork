public class ArraySequence<E> implements Sequence<E> {
  private E[] contents;
  private int size;

  final static private int INITIAL_CAPACITY = 10;

  @SuppressWarnings("unchecked")
  ArraySequence() {
    contents = (E[]) new Object[INITIAL_CAPACITY];
    size = 0;
  }

  // Double the array size whenever we run out of room.
  // (This can save exponential time at the cost of up to 2X wasted space.)
  private void ensureOneMore() {
    if (size >= contents.length) {
      int newCapacity = contents.length * 2;
      contents = java.util.Arrays.copyOf(contents,newCapacity);
    }
  }

  public void append(E e) {
    ensureOneMore();
    contents[size++] = e;
  }

  public void prepend(E e) {
    ensureOneMore();
    System.arraycopy(contents, 0, contents, 1, size);
    contents[0] = e;
    size++;
  }

  public int size() {
    return size;
  }

  public E get(int index) {
    if (index < 0 || index >= size) {
      throw new IllegalArgumentException("index " + index  + " out of range");
    }
    return contents[index];
  }
}
