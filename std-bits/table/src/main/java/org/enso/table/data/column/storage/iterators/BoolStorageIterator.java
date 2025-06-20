package org.enso.table.data.column.storage.iterators;

import org.enso.table.data.column.storage.ColumnBooleanStorage;

public final class BoolStorageIterator extends AbstractBaseIterator<Boolean> implements ColumnBooleanStorageIterator {
  private final ColumnBooleanStorage parent;

  public BoolStorageIterator(ColumnBooleanStorage parent, long startIndex, long length) {
    super(parent, startIndex, length);
    this.parent = parent;
  }

  @Override
  public boolean getItemAsBoolean() {
    return parent.getItemAsBoolean(getIndex());
  }
}
