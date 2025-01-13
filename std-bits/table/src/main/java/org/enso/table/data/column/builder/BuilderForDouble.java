package org.enso.table.data.column.builder;

/** A builder for double columns - avoids boxing. */
public interface BuilderForDouble extends Builder {
  /**
   * Append a new double to this builder.
   *
   * @param data the double to append
   */
  void appendDouble(double data);
}
