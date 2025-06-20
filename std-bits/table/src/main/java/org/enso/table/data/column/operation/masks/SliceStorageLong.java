package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.mask.OrderMask;

class SliceStorageLong extends SliceStorage<Long> implements ColumnLongStorage {
  private final ColumnLongStorage parent;

  public SliceStorageLong(ColumnLongStorage parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
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
    var mappedIndex = mapIndex(index);
    if (mappedIndex == OrderMask.NOT_FOUND_INDEX) {
      throw new ValueIsNothingException(index);
    }
    return parent.getItemAsLong(mappedIndex);
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(this);
  }
}
