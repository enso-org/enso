package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.SpecializedStorage;
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
  public void appendBulkStorage(Storage<?> storage) {
    if (storage.getType() == getType()) {
      if (storage instanceof SpecializedStorage<?>) {
        // This cast is safe, because storage.getType() == this.getType() iff storage.T == this.T
        @SuppressWarnings("unchecked")
        SpecializedStorage<T> specializedStorage = (SpecializedStorage<T>) storage;
        System.arraycopy(specializedStorage.getData(), 0, data, currentSize, storage.size());
        currentSize += storage.size();
      } else {
        throw new IllegalStateException(
            "Unexpected storage implementation for type "
                + storage.getType()
                + ": "
                + storage
                + ". This is a bug in the Table library.");
      }
    } else {
      throw new StorageTypeMismatch(getType(), storage.getType());
    }
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
