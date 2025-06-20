package org.enso.table.data.column.storage.iterators;

import org.enso.table.data.column.storage.ColumnBooleanStorage;

public final class BooleanStorageIterator extends AbstractBaseIterator<Boolean>
    implements ColumnBooleanStorageIterator {
  private final ColumnBooleanStorage parent;

  public BooleanStorageIterator(ColumnBooleanStorage parent) {
    super(parent);
    this.parent = parent;
  }

  @Override
  public boolean getItemAsBoolean() {
    return parent.getItemAsBoolean(getIndex());
  }
}
