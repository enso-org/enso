package org.enso.table.data.column.operation.masks;

import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ValueIsNothingException;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.mask.OrderMask;

class MaskedStorageDouble extends MaskedStorage<Double> implements ColumnDoubleStorage {
  private final ColumnDoubleStorage parent;

  public MaskedStorageDouble(ColumnDoubleStorage parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
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
    var mappedIndex = mapIndex(index);
    if (mappedIndex == OrderMask.NOT_FOUND_INDEX) {
      throw new ValueIsNothingException(index);
    }
    return parent.getItemAsDouble(mappedIndex);
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(this);
  }
}
