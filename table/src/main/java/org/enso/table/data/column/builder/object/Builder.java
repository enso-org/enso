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

  /** @return the number of appended elements */
  public abstract int getCurrentSize();

  /** @return a storage containing all the items appended so far */
  public abstract Storage seal();
}
