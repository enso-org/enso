package org.enso.table.data.column.builder.object;

import java.util.Arrays;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

/** A builder for string columns. */
public class StringBuilder extends TypedBuilder {
  private Object[] data;
  private int currentSize = 0;

  public StringBuilder(int size) {
    this.data = new Object[size];
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
      ObjectBuilder res = new ObjectBuilder(data);
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
  public void appendNoGrow(Object o) {
    data[currentSize++] = o;
  }

  @Override
  public void append(Object o) {
    if (currentSize + 1 > data.length) {
      grow();
    }
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
  public int getCurrentCapacity() {
    return 0;
  }

  @Override
  public Storage seal() {
    return new StringStorage(data, currentSize);
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
