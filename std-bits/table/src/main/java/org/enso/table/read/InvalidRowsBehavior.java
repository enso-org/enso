package org.enso.table.read;

/** Specifies how to handle rows with unexpected number of columns. */
public enum InvalidRowsBehavior {
  /** Discards rows with unexpected number of columns. */
  DROP,

  /** Keeps rows with unexpected number of columns, but the additional columns are discarded. */
  KEEP,

  /** Keeps rows with unexpected number of columns, adding extra columns. */
  ADD_EXTRA_COLUMNS
}
