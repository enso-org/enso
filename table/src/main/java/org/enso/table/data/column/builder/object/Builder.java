package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;

/** A builder for creating columns dynamically. */
public abstract class Builder {
  /**
   * Append a new item to this builder.
   *
   * @param o the item to append
   */
  public abstract void append(Object o);

  /**
   * Appends a specified number of missing values into the builder.
   *
   * <p>This operation should be equivalent to calling {@link #append(Object)} with {@code null} as
   * an argument, {@code count} times, however it may be implemented more efficiently by the
   * builder.
   *
   * @param count the number of missing values to append.
   */
  public abstract void appendNulls(int count);

  /** @return the number of appended elements */
  public abstract int getCurrentSize();

  /** @return a storage containing all the items appended so far */
  public abstract Storage seal();
}
