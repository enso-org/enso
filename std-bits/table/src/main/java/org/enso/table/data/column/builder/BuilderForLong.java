package org.enso.table.data.column.builder;

/** A builder for long columns - avoids boxing. */
public interface BuilderForLong extends BuilderForType<Long> {
  /**
   * Append a new long to this builder.
   *
   * @param value the long to append
   */
  BuilderForLong appendLong(long value);

  /**
   * Checks whether the value at the given index is null.
   *
   * @param index the index to check
   * @return true if the value at the index is null, false otherwise
   */
  boolean isNothing(long index);

  /**
   * Gets the long value at the given index.
   *
   * @param index the index to get the value from
   * @return the long value at the index
   */
  long getLong(long index);

  /**
   * Gets the current capacity of the builder.
   *
   * @return the current capacity
   */
  long getCurrentCapacity();
}
