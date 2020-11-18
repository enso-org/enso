package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.Storage;

/** A builder for boxed object columns. */
public class ObjectBuilder extends TypedBuilder {
  private final Object[] data;
  private final int size;
  private int currentSize = 0;

  public ObjectBuilder(int size) {
    this.size = size;
    this.data = new Object[size];
  }

  public ObjectBuilder(Object[] data, int size) {
    this.data = data;
    this.size = size;
  }

  @Override
  public void writeTo(Object[] items) {
    throw new IllegalStateException("Broken invariant: rewriting the most general type.");
  }

  @Override
  public boolean canRetypeTo(long type) {
    return false;
  }

  @Override
  public TypedBuilder retypeTo(long type) {
    throw new IllegalStateException("Broken invariant: rewriting the most general type.");
  }

  @Override
  public int getType() {
    return Storage.Type.OBJECT;
  }

  @Override
  public void append(Object o) {
    data[currentSize++] = o;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage seal() {
    return new ObjectStorage(data, size);
  }

  public Object[] getData() {
    return data;
  }

  public void setCurrentSize(int currentSize) {
    this.currentSize = currentSize;
  }
}
