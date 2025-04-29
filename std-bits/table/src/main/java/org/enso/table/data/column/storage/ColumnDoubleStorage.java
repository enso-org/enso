package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.type.FloatType;

public interface ColumnDoubleStorage extends ColumnStorage<Double> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  double getItemAsDouble(long index) throws ValueIsNothingException;

  @Override
  ColumnDoubleStorageIterator iterator();

  @Override
  FloatType getType();
}
