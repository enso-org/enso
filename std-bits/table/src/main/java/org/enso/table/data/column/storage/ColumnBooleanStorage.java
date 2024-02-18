package org.enso.table.data.column.storage;

public interface ColumnBooleanStorage extends ColumnStorage {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  boolean get(long index) throws ValueIsNothingException;
}
