package org.enso.table.data.column.builder;

/** A builder for double columns - avoids boxing. */
public interface BuilderForDouble extends BuilderForType<Double> {
  /**
   * Append a new long to this builder. If the value cannot be represented exactly as a double, a
   * problem will be attached.
   *
   * @param value the long to append
   */
  void appendLong(long value);

  /**
   * Append a new double to this builder.
   *
   * @param value the double to append
   */
  void appendDouble(double value);
}
