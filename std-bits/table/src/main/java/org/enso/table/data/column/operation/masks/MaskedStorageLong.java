package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.iterators.LongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

final class MaskedStorageLong extends MaskedStorage<Long> implements ColumnLongStorage {
  private final ColumnLongStorage parent;

  public MaskedStorageLong(ColumnLongStorage parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
    this.parent = parent;
  }

  @Override
  public ColumnLongStorage parent() {
    return parent;
  }

  @Override
  public IntegerType getType() {
    return parent.getType();
  }

  @Override
  public long getItemAsLong(long index) throws ValueIsNothingException {
    var mappedIndex = mapIndex(index);
    if (mappedIndex == IndexMapper.NOT_FOUND_INDEX) {
      throw new ValueIsNothingException(index);
    }
    return parent.getItemAsLong(mappedIndex);
  }

  @Override
  public ColumnLongStorageIterator iteratorWithIndex() {
    return new LongStorageIterator(this);
  }
}
