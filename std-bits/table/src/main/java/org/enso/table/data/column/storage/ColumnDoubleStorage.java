package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.iterators.ColumnDoubleStorageIterator;
import org.enso.table.data.column.storage.type.FloatType;

public interface ColumnDoubleStorage extends ColumnStorage<Double> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  double getItemAsDouble(long index) throws ValueIsNothingException;

  /* Gets an iterator with index tracking and unboxed values. */
  ColumnDoubleStorageIterator iteratorWithIndex();

  @Override
  FloatType getType();
}
