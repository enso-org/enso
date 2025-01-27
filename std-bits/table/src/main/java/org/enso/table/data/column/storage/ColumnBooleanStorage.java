package org.enso.table.data.column.storage;

public interface ColumnBooleanStorage extends ColumnStorage<Boolean> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  boolean getItemAsBoolean(long index) throws ValueIsNothingException;
}
