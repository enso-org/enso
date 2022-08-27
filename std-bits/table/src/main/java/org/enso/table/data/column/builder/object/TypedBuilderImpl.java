package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;

import java.util.Arrays;

public abstract class TypedBuilderImpl<T> extends TypedBuilder {
  protected T[] data;
  protected int currentSize = 0;

  protected abstract T[] newArray(int size);

  public TypedBuilderImpl(int size) {
    this.data = newArray(size);
  }

  @Override
  public void writeTo(Object[] items) {
    if (currentSize >= 0) System.arraycopy(data, 0, items, 0, currentSize);
  }

  @Override
  public boolean canRetypeTo(long type) {
    return type == Storage.Type.OBJECT;
  }

  @Override
  public TypedBuilder retypeTo(long type) {
    if (type == Storage.Type.OBJECT) {
      Object[] widenedData = Arrays.copyOf(data, data.length, Object[].class);
      ObjectBuilder res = new ObjectBuilder(widenedData);
      res.setCurrentSize(currentSize);
      return res;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public void append(Object o) {
    if (currentSize + 1 > data.length) {
      grow();
    }

    appendNoGrow(o);
  }

  @Override
  public void appendNulls(int count) {
    currentSize += count;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  private void grow() {
    if (data.length > 1) {
      grow(data.length * 3 / 2);
    } else {
      grow(3);
    }
  }

  private void grow(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
