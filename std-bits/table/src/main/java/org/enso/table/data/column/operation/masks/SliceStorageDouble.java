package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;

class SliceStorageDouble extends SliceStorage<Double>
    implements ColumnDoubleStorage {
  private final ColumnDoubleStorage parent;

  public SliceStorageDouble(ColumnDoubleStorage parent, long start, long end) {
    super(parent, start, end);
    this.parent = parent;
  }

  @Override
  ColumnDoubleStorage parent() {
    return parent;
  }

  @Override
  public FloatType getType() {
    return parent.getType();
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    if (index < 0 || index >= end - start) {
      throw new IndexOutOfBoundsException(index);
    }
    return parent.getItemAsDouble(index - start);
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(parent, start, getSize());
  }
}
