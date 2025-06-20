package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.BooleanStorageIterator;
import org.enso.table.data.column.storage.iterators.ColumnBooleanStorageIterator;

class SliceStorageBoolean extends SliceStorage<Boolean>
    implements ColumnBooleanStorage {
  private final ColumnBooleanStorage parent;

  public SliceStorageBoolean(ColumnBooleanStorage parent, long start, long end) {
    super(parent, start, end);
    this.parent = parent;
  }

  @Override
  ColumnBooleanStorage parent() {
    return parent;
  }

  @Override
  public boolean getItemAsBoolean(long index) throws ValueIsNothingException {
    if (index < 0 || index >= end - start) {
      throw new IndexOutOfBoundsException(index);
    }
    return parent.getItemAsBoolean(index - start);
  }

  @Override
  public ColumnBooleanStorageIterator iteratorWithIndex() {
    return new BooleanStorageIterator(parent, start, getSize());
  }
}
