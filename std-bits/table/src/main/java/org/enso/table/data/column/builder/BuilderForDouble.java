package org.enso.table.data.column.builder;

/** A builder for double columns - avoids boxing. */
public interface BuilderForDouble extends Builder {
  /**
   * Append a new long to this builder. If the value cannot be represented exactly as a double, a
   * problem will be attached.
   *
   * @param data the long to append
   */
  void appendLong(long data);

  /**
   * Append a new double to this builder.
   *
   * @param data the double to append
   */
  void appendDouble(double data);
}
