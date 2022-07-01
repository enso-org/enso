package org.enso.table.write;

public enum ExistingDataMode {
  /**
   * Throw an ExistingDataException if the target already contains data.
   */
  ERROR,
  /**
   * The current data is replaced by the new data.
   */
  REPLACE,
  /**
   * The new data is appended to the current data based on column position.
   * If the new data has fewer columns than the existing data, extra columns are NULL.
   * If the new data has more columns than the existing data, extra columns are dropped.
   * If mismatched, a ColumnCountMismatch is attached to the result.
   */
  APPEND_BY_INDEX,
  /**
   * The new data is appended to the current data based on column name.
   * If an existing column cannot be found, the new rows will have NULL for that column.
   * If a new column does not exist in existing table, these columns are dropped.
   * If mismatched, a ColumnNameMismatch is attached to the result.
   */
  APPEND_BY_NAME
}
