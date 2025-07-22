package org.enso.table.data.column.storage;

import org.enso.table.data.column.storage.iterators.ColumnBooleanStorageIterator;

public interface ColumnBooleanStorage extends ColumnStorage<Boolean> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  boolean getItemAsBoolean(long index) throws ValueIsNothingException;

  /* Gets an iterator with index tracking and unboxed values. */
  ColumnBooleanStorageIterator iteratorWithIndex();
}
