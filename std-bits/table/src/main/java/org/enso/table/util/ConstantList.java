package org.enso.table.util;

import java.util.AbstractList;

public class ConstantList<E> extends AbstractList<E> {

  private final E element;
  private final int size;

  public static <T> ConstantList<T> make(T element, int size) {
    return new ConstantList<>(element, size);
  }

  protected ConstantList(E element, int size) {
    this.element = element;
    this.size = size;
  }

  @Override
  public E get(int index) {
    if (index < 0 || index >= size) {
      throw new IndexOutOfBoundsException(index);
    }

    return element;
  }

  @Override
  public int size() {
    return size;
  }
}
