public class ArrayIntSequence implements IntSequence {
  private int[] contents;
  private int size;

  final static private int INITIAL_CAPACITY = 10;

  ArrayIntSequence() {
    contents = new int[INITIAL_CAPACITY];
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

  public void append(int e) {
    ensureOneMore();
    contents[size++] = e;
  }

  public void prepend(int e) {
    ensureOneMore();
    System.arraycopy(contents, 0, contents, 1, size);
    contents[0] = e;
    size++;
  }

  public int size() {
    return size;
  }

  public int get(int index) {
    if (index < 0 || index >= size) {
      throw new IllegalArgumentException("index " + index  + " out of range");
    }
    return contents[index];
  }
}
