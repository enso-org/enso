package org.enso.table.read;

/**
 * Specifies how to handle fixed-width file lines that do not haveenough characters to cover the
 * columns of the fixed-width layout.
 */
public enum InvalidFixedWidthRowsBehavior {
  /** Discards rows that are too short for the specified fixed-width layout. */
  DROP,

  /**
   * Keeps rows that are too short for the specified fixed-width layout, keeping partial columns, or
   * using empty strings for entirely missing columns.
   */
  KEEP,
}
