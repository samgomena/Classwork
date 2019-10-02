/**
 * An ordered collection of elements (very similar to the Java library's List interface).
 * There is no fixed bound on the number of elements. 
 * Elements can be added at the begining or the end of the sequence.
 * They can be retrieved by position; position numbers start at 0. 
 */

interface Sequence<E> {

  // Append an element to the end of the sequence.
  void append(E e);

  // Prepend an element to the beginning of the sequence, shifting the
  // indices of all existing elements up by one.
  void prepend(E e);

  // Return number of elements in sequence
  int size();

  // Return the element at the specified position in the sequence.
  // Positions are numbered from 0.
  // throws IllegalArgumentException if the index is out of bounds
  E get(int index);
}
