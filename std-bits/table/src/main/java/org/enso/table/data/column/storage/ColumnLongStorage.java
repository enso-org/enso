package org.enso.table.data.column.storage;

public interface ColumnLongStorage extends ColumnStorage {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  long get(long index) throws ValueIsNothingException;
}
