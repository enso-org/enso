package org.enso.table.data.column.storage.iterators;

import org.enso.table.data.column.storage.ColumnLongStorage;

/** Basic iterator for long storages. */
public final class LongStorageIterator extends AbstractBaseIterator<Long>
    implements ColumnLongStorageIterator {
  private final ColumnLongStorage parent;

  public LongStorageIterator(ColumnLongStorage parent) {
    super(parent);
    this.parent = parent;
  }

  @Override
  public long getItemAsLong() {
    return parent.getItemAsLong(getIndex());
  }
}
