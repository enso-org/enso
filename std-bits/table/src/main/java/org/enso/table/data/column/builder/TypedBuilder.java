package org.enso.table.data.column.builder;

import java.util.Arrays;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;

public abstract class TypedBuilder<T> implements BuilderWithRetyping, BuilderForType<T> {
  private final StorageType storageType;
  protected T[] data;
  protected int currentSize = 0;

  protected TypedBuilder(StorageType storageType, T[] data) {
    this.data = data;
    this.storageType = storageType;
  }

  @Override
  public StorageType getType() {
    return storageType;
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (currentSize >= 0) {
      System.arraycopy(data, 0, items, 0, currentSize);
    }
  }

  @Override
  public boolean canRetypeTo(StorageType type) {
    return false;
  }

  @Override
  public Builder retypeTo(StorageType type) {
    throw new UnsupportedOperationException();
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
      throw new StorageTypeMismatchException(getType(), storage.getType());
    }
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  /**
   * Checks if space to append single element, grows the underlying array if needed.
   *
   * <p>The method grows the array by 50% by default to amortize the re-allocation time over
   * appends. It tries to keep the invariant that after calling `grow` the array has at least one
   * free slot.
   */
  protected void ensureSpaceToAppend() {
    // Check current size. If there is space, we don't need to grow.
    if (currentSize < data.length) {
      return;
    }

    int desiredCapacity = Math.max(currentSize + 1, data.length > 1 ? data.length * 3 / 2 : 3);
    resize(desiredCapacity);
  }

  protected void resize(int desiredCapacity) {
    if (data.length == desiredCapacity) {
      return;
    }
    this.data = Arrays.copyOf(data, desiredCapacity);
  }

  protected abstract Storage<T> doSeal();

  @Override
  public Storage<T> seal() {
    // We set the array to the exact size, because we want to avoid index out of bounds errors.
    // Most of the time, the builder was initialized with the right size anyway - the only
    // exceptions are e.g. reading results from a database, where the count is unknown.
    // In the future we may rely on smarter storage for sparse columns.
    resize(currentSize);
    return doSeal();
  }
}
