package org.enso.table.data.column.storage;

public interface ColumnDoubleStorageWithArray extends ColumnDoubleStorage {
  /** Gets the array of long values for faster iteration. */
  double[] getArray();
}
