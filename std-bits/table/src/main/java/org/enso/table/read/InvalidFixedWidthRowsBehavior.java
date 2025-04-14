package org.enso.table.read;

/** Specifies how to handle rows with unexpected number of columns. */
public enum InvalidFixedWidthRowsBehavior {
  /** Discards rows that are too short for the specified fixed-width layout. */
  DROP,

  /**
   * Keeps rows that are too short for the specified fixed-width layout, keeping
   * partial rows, or using empty strings for entirely missing values.
   */
  KEEP,
}
