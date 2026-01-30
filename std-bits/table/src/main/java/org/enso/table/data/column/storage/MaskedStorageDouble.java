package org.enso.table.data.column.storage;

import org.enso.table.data.column.operation.masks.IndexMapper;
import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.iterators.DoubleStorageIterator;

public final class MaskedStorageDouble extends MaskedStorage<Double>
    implements ColumnDoubleStorage {
  private final ColumnDoubleStorage parent;

  public MaskedStorageDouble(ColumnDoubleStorage parent, IndexMapper indexMapper) {
    super(parent, indexMapper);
    this.parent = parent;
  }

  @Override
  public ColumnDoubleStorage parent() {
    return parent;
  }

  @Override
  public double getItemAsDouble(long index) throws ValueIsNothingException {
    var mappedIndex = mapIndex(index);
    if (mappedIndex == IndexMapper.NOT_FOUND_INDEX) {
      throw new ValueIsNothingException(index);
    }
    return parent.getItemAsDouble(mappedIndex);
  }

  @Override
  public ColumnDoubleStorageIterator iteratorWithIndex() {
    return new DoubleStorageIterator(this);
  }
}
