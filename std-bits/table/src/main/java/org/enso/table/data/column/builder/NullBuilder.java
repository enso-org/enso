package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.NullStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

public final class NullBuilder implements Builder {
  private int length = 0;

  NullBuilder() {}

  @Override
  public void append(Object o) {
    if (o != null) {
      throw new IllegalArgumentException("NullBuilder can only append nulls, but got " + o);
    }

    length++;
  }

  @Override
  public void appendNulls(int count) {
    length += count;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    // For any storage that is not all-null, check if non-null values are present
    if (!(storage.getType() instanceof NullType)) {
      for (long i = 0; i < storage.getSize(); i++) {
        if (!storage.isNothing(i)) {
          throw new IllegalArgumentException(
              "NullBuilder can only append nulls, but got " + storage.getItemBoxed(i));
        }
      }
    }

    length += Math.toIntExact(storage.getSize());
  }

  @Override
  public int getCurrentSize() {
    return length;
  }

  @Override
  public Storage<?> seal() {
    return new NullStorage(length);
  }

  @Override
  public StorageType getType() {
    return NullType.INSTANCE;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < length; i++) {
      items[i] = null;
    }
  }
}
