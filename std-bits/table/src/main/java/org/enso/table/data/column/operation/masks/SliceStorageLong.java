package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

class SliceStorageLong extends SliceStorage<Long> implements ColumnLongStorage {
  private final ColumnLongStorage parent;

  public SliceStorageLong(ColumnLongStorage parent, long start, long end) {
    super(parent, start, end);
    this.parent = parent;
  }

  @Override
  ColumnLongStorage parent() {
    return parent;
  }

  @Override
  public IntegerType getType() {
    return parent.getType();
  }

  @Override
  public long getItemAsLong(long index) throws ValueIsNothingException {
    if (index < 0 || index >= end - start) {
      throw new IndexOutOfBoundsException(index);
    }
    return parent.getItemAsLong(index - start);
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(parent, start, getSize());
  }
}
