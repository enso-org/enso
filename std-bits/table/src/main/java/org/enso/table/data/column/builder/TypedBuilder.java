package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.type.StorageType;

/** A builder for the given storage type and known result length. */
public abstract class TypedBuilder extends Builder {
  /**
   * Dump all the items into a given boxed buffer.
   *
   * @param items the buffer to dump elements into
   */
  public abstract void retypeToMixed(Object[] items);

  /**
   * Checks if the builder can be efficiently retyped to the given storage type.
   *
   * @param type the storage type
   * @return whether the column can be retyped
   */
  public abstract boolean canRetypeTo(StorageType type);

  /**
   * Retype this builder to the given type. Can only be called if {@link #canRetypeTo(StorageType)}
   * returns true for the type.
   *
   * @param type the target type
   * @return a retyped builder
   */
  public abstract TypedBuilder retypeTo(StorageType type);

  /** Specifies if the following object will be accepted by this builder's append* methods. */
  public abstract boolean accepts(Object o);
}
