package org.enso.table.data.column.builder;

import org.enso.table.data.column.operation.unary.CountNothing;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.NullStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;

final class NullBuilder implements Builder {
  private int length = 0;

  @Override
  public NullBuilder append(Object o) {
    if (o != null) {
      throw new IllegalArgumentException("NullBuilder can only append nulls, but got " + o);
    }

    length++;
    return this;
  }

  @Override
  public NullBuilder appendNulls(int count) {
    length += count;
    return this;
  }

  @Override
  public void appendBulkStorage(ColumnStorage<?> storage) {
    // For any storage that is not all-null, check if non-null values are present
    if (!CountNothing.allNothing(storage)) {
      throw new IllegalArgumentException("NullBuilder can only append nulls.");
    }
    length += Math.toIntExact(storage.getSize());
  }

  @Override
  public long getCurrentSize() {
    return length;
  }

  @Override
  public ColumnStorage<?> seal() {
    return new NullStorage(length);
  }

  @Override
  public StorageType<?> getType() {
    return NullType.INSTANCE;
  }

  @Override
  public void copyDataTo(Object[] items) {
    for (int i = 0; i < length; i++) {
      items[i] = null;
    }
  }
}
