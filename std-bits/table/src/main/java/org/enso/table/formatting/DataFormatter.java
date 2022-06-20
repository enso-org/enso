package org.enso.table.formatting;

public interface DataFormatter {
  String format(Object value);

  /**
   * Checks if the given formatter can format the provided value.
   *
   * <p>The value should not be null.
   */
  boolean canFormat(Object value);

  String NULL_REPRESENTATION = null;
}
