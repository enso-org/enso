package org.enso.table.data.column.storage;

public interface ColumnLongStorage extends ColumnStorage<Long> {
  /** Gets the value at a given index. Throws ValueIsNothingException if the index is nothing. */
  long getPrimitive(long index) throws ValueIsNothingException;
}
