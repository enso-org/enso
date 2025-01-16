package org.enso.table.data.column.builder;

/** A builder for long columns - avoids boxing. */
public interface BuilderForLong extends Builder {
  /**
   * Append a new long to this builder.
   *
   * @param value the long to append
   */
  void appendLong(long value);
}
