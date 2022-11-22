package org.enso.table.write;

public enum ExistingDataMode {
  /** Throw an ExistingDataException if the target already contains data. */
  ERROR,
  /** The current data is replaced by the new data. */
  REPLACE,
  /**
   * The new data is appended to the current data based on column position. If the new data has a
   * different number of columns than the existing data, a ColumnCountMismatch is exception is
   * thrown.
   */
  APPEND_BY_INDEX,
  /**
   * The new data is appended to the current data based on column name. If an existing column cannot
   * be found or if a new column does not exist in the existing table, a ColumnNameMismatchException
   * is thrown.
   */
  APPEND_BY_NAME
}
