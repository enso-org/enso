package org.enso.base;

import java.util.ArrayList;
import java.util.List;

public class Array_Builder<T> {
  private final List<T> builder;

  private Array_Builder(int capacity) {
    this.builder = new ArrayList<>(capacity);
  }

  /**
   * This function forces the polyglot conversion of an Enso array into a `byte[]`. This allows for
   * asserting that it is a valid `byte[]`.
   *
   * @param input the converted array.
   * @return the `input` unchanged.
   */
  public static byte[] ensureByteArray(byte[] input) {
    return input;
  }

  /** Creates new builder */
  public static <V> Array_Builder<V> newBuilder(int capacity) {
    return new Array_Builder<>(capacity);
  }

  /** Is the builder empty? */
  public boolean isEmpty() {
    return builder.isEmpty();
  }

  /**
   * Adds an element to the builder
   *
   * @param e the element to add
   */
  public void add(T e) {
    builder.add(e);
  }

  /** Obtains an element from the builder */
  public T get(int index) {
    return builder.get(index);
  }

  /** A temporary workaround to be able to efficiently append an array to `ArrayList`. */
  public void appendTo(List<T> list) {
    builder.addAll(list);
  }

  /** Converts current content to a new array. */
  public Object[] toArray() {
    return builder.toArray();
  }
}
