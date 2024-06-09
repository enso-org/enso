package org.enso.table.data.column.storage;

public interface ColumnDoubleStorage extends ColumnStorage {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  double get(long index) throws ValueIsNothingException;
}
