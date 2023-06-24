package org.enso.table.data.column.builder;

import java.util.Arrays;
import java.util.Objects;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.StorageType;

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
  public boolean canRetypeTo(StorageType type) {
    return Objects.equals(type, AnyObjectType.INSTANCE);
  }

  @Override
  public TypedBuilder retypeTo(StorageType type) {
    if (Objects.equals(type, AnyObjectType.INSTANCE)) {
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
    if (currentSize >= data.length) {
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
    if (storage.getType().equals(getType())) {
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

  /**
   * Grows the underlying array.
   *
   * <p>The method grows the array by 50% by default to amortize the re-allocation time over
   * appends. It tries to keep the invariant that after calling `grow` the array has at least one
   * free slot.
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

    grow(desiredCapacity);
  }

  private void grow(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
