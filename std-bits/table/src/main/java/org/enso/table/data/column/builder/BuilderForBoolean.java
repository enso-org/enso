package org.enso.table.data.column.builder;

/** A builder for boolean columns - avoids boxing. */
public interface BuilderForBoolean extends Builder {
  /**
   * Append a new boolean to this builder.
   *
   * @param data the boolean to append
   */
  void appendBoolean(boolean data);
}
