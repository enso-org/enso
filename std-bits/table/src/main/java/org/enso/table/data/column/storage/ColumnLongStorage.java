package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;
import org.enso.table.data.column.storage.type.IntegerType;

public interface ColumnLongStorage extends ColumnStorage<Long> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  long getItemAsLong(long index) throws ValueIsNothingException;

  /* Gets an iterator with index tracking and unboxed values. */
  ColumnLongStorageIterator iteratorWithIndex();

  @Override
  IntegerType getType();
}
