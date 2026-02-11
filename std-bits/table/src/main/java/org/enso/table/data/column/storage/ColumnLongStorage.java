package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.iterators.ColumnLongStorageIterator;

public interface ColumnLongStorage extends ColumnStorage<Long> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  long getItemAsLong(long index) throws ValueIsNothingException;

  /* Gets an iterator with index tracking and unboxed values. */
  ColumnLongStorageIterator iteratorWithIndex();
}
