package org.enso.table.data.column.builder;

import java.lang.reflect.Proxy;
import java.util.Arrays;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

abstract class TypedBuilder<T> implements BuilderWithRetyping, BuilderForType<T> {
  private final StorageType<T> storageType;
  protected T[] data;
  protected int currentSize = 0;

  protected TypedBuilder(StorageType<T> storageType, T[] data) {
    this.data = data;
    this.storageType = storageType;
  }

  @Override
  public StorageType<T> getType() {
    return storageType;
  }

  @Override
  public void copyDataTo(Object[] items) {
    if (currentSize >= 0) {
      System.arraycopy(data, 0, items, 0, currentSize);
    }
  }

  @Override
  public boolean canRetypeTo(StorageType<?> type) {
    return false;
  }

  @Override
  public Builder retypeTo(StorageType<?> type) {
    throw new UnsupportedOperationException();
  }

  @Override
  public TypedBuilder<T> appendNulls(int count) {
    currentSize += count;
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    long newSize = currentSize + storage.getSize();
    if (newSize > data.length) {
      int newSizeInt = Builder.checkSize(newSize);
      resize(newSizeInt);
    }
    var type = storage.getType();
    if (Proxy.isProxyClass(type.getClass())) {
      type = StorageType.fromTypeCharAndSize(type.typeChar(), type.size());
    }

    if (type.equals(getType())) {
      if (storage instanceof TypedStorage<?>) {
        // This cast is safe, because storage.getType() == this.getType() iff storage.T == this.T
        @SuppressWarnings("unchecked")
        TypedStorage<T> specializedStorage = (TypedStorage<T>) storage;
        System.arraycopy(
            specializedStorage.getData(), 0, data, currentSize, (int) storage.getSize());
        currentSize += storage.getSize();
      } else {
        // This is a fallback for non-specialized storages, which are not optimized for bulk
        // appends.
        for (long i = 0; i < storage.getSize(); i++) {
          append(storage.getItemBoxed(i));
        }
      }
    } else if (type instanceof NullType) {
      appendNulls(Math.toIntExact(storage.getSize()));
    } else {
      throw new StorageTypeMismatchException(getType(), type);
    }
  }

  @Override
  public long getCurrentSize() {
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

  protected abstract ColumnStorage<T> doSeal();

  @Override
  public ColumnStorage<T> seal() {
    // We set the array to the exact size, because we want to avoid index out of bounds errors.
    // Most of the time, the builder was initialized with the right size anyway - the only
    // exceptions are e.g. reading results from a database, where the count is unknown.
    // In the future we may rely on smarter storage for sparse columns.
    resize(currentSize);
    return doSeal();
  }
}
