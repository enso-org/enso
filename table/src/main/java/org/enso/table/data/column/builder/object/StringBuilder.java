package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

/** A builder for string columns. */
public class StringBuilder extends TypedBuilder {
  private final Object[] data;
  private final int size;
  private int currentSize = 0;

  public StringBuilder(int size) {
    this.data = new Object[size];
    this.size = size;
  }

  @Override
  public void writeTo(Object[] items) {
    for (int i = 0; i < currentSize; i++) {
      items[i] = data[i];
    }
  }

  @Override
  public boolean canRetypeTo(long type) {
    return type == Storage.Type.OBJECT;
  }

  @Override
  public TypedBuilder retypeTo(long type) {
    if (type == Storage.Type.OBJECT) {
      ObjectBuilder res = new ObjectBuilder(data, size);
      res.setCurrentSize(currentSize);
      return res;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  @Override
  public int getType() {
    return Storage.Type.STRING;
  }

  @Override
  public void append(Object o) {
    data[currentSize++] = o;
  }

  @Override
  public void appendNulls(int count) {
    currentSize += count;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage seal() {
    return new StringStorage(data, size);
  }
}
