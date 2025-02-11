package org.enso.table.data.column.storage;

public interface ColumnLongStorageWithArray extends ColumnLongStorage {
  /** Gets the array of long values for faster iteration. */
  long[] getArray();
}
