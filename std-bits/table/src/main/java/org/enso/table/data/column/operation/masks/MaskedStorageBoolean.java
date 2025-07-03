package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.BooleanStorageIterator;
import org.enso.table.data.column.storage.iterators.ColumnBooleanStorageIterator;

final class MaskedStorageBoolean extends MaskedStorage<Boolean> implements ColumnBooleanStorage {
  private final ColumnBooleanStorage parent;

  MaskedStorageBoolean(ColumnBooleanStorage parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
    this.parent = parent;
  }

  @Override
  public ColumnBooleanStorage parent() {
    return parent;
  }

  @Override
  public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
    var mappedIndex = mapIndex(index);
    if (mappedIndex == IndexMapper.NOT_FOUND_INDEX) {
      throw new ValueIsNothingException(index);
    }
    return parent.getItemAsBoolean(mappedIndex);
  }

  @Override
  public ColumnBooleanStorageIterator iteratorWithIndex() {
    return new BooleanStorageIterator(this);
  }
}
