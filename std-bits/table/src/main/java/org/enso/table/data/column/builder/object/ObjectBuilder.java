package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.Arrays;

/** A builder for boxed object columns. */
public class ObjectBuilder extends TypedBuilder {
  private Object[] data;
  private int currentSize = 0;

  public ObjectBuilder(int size) {
    this.data = new Object[size];
  }

  public ObjectBuilder(Object[] data) {
    this.data = data;
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
  public void appendNoGrow(Object o) {
    data[currentSize++] = o;
  }

  @Override
  public boolean accepts(Object o) {
    return true;
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
  public void appendBulkStorage(Storage<?> storage) {
    if (currentSize + storage.size() > data.length) {
      grow(currentSize + storage.size());
    }

    if (storage instanceof SpecializedStorage<?> specializedStorage) {
      System.arraycopy(specializedStorage.getData(), 0, data, currentSize, storage.size());
      currentSize += storage.size();
    } else {
      int n = storage.size();
      for (int i = 0; i < n; i++) {
        data[currentSize++] = storage.getItemBoxed(i);
      }
    }
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  @Override
  public Storage<Object> seal() {
    return new ObjectStorage(data, currentSize);
  }

  public Object[] getData() {
    return data;
  }

  public void setCurrentSize(int currentSize) {
    if (currentSize > data.length) grow(currentSize);
    this.currentSize = currentSize;
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
