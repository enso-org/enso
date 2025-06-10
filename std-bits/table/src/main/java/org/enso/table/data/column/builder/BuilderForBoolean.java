package org.enso.table.data.column.builder;

/** A builder for boolean columns - avoids boxing. */
public interface BuilderForBoolean extends BuilderForType<Boolean> {
  /**
   * Append a new boolean to this builder.
   *
   * @param value the boolean to append
   */
  void appendBoolean(boolean value);
}
