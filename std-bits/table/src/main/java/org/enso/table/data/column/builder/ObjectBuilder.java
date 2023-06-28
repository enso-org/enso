package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;

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
  public boolean canRetypeTo(StorageType type) {
    return false;
  }

  @Override
  public TypedBuilder retypeTo(StorageType type) {
    throw new IllegalStateException("Broken invariant: rewriting the most general type.");
  }

  @Override
  public StorageType getType() {
    return AnyObjectType.INSTANCE;
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
    if (currentSize >= data.length) {
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
      resize(currentSize + storage.size());
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
    resize(currentSize);
    return new ObjectStorage(data, currentSize);
  }

  public Object[] getData() {
    return data;
  }

  public void setCurrentSize(int currentSize) {
    if (currentSize > data.length) resize(currentSize);
    this.currentSize = currentSize;
  }

  /**
   * Grows the underlying array.
   * <p>
   * The method grows the array by 50% by default to amortize the re-allocation time over appends.
   * It tries to keep the invariant that after calling `grow` the array has at least one free slot.
   */
  private void grow() {
    int desiredCapacity = 3;
    if (data.length > 1) {
      desiredCapacity = (data.length * 3 / 2);
    }

    // It is possible for the `currentSize` to grow arbitrarily larger than
    // the capacity, because when nulls are being added the array is not
    // resized, only the counter is incremented. Thus, we need to ensure
    // that we have allocated enough space for at least one element.
    if (currentSize >= desiredCapacity) {
      desiredCapacity = currentSize + 1;
    }

    resize(desiredCapacity);
  }

  private void resize(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
