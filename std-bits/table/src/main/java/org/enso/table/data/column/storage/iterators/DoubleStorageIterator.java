package org.enso.table.data.column.storage.iterators;

import org.enso.table.data.column.storage.ColumnDoubleStorage;

public final class DoubleStorageIterator extends AbstractBaseIterator<Double> implements ColumnDoubleStorageIterator {
  private final ColumnDoubleStorage parent;

  public DoubleStorageIterator(ColumnDoubleStorage parent, long startIndex, long length) {
    super(parent, startIndex, length);
    this.parent = parent;
  }

  @Override
  public double getItemAsDouble() {
    return parent.getItemAsDouble(getIndex());
  }
}