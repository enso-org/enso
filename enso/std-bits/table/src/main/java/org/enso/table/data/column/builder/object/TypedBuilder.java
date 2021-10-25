package org.enso.table.data.column.builder.object;

/** A builder for the given storage type and known result size. */
public abstract class TypedBuilder extends Builder {
  /**
   * Dump all the items into a given boxed buffer.
   *
   * @param items the buffer to dump elements into
   */
  public abstract void writeTo(Object[] items);

  /**
   * Checks if the builder can be efficiently retyped to the given storage type.
   *
   * @param type the storage type enumeration
   * @return whether the column can be retyped
   */
  public abstract boolean canRetypeTo(long type);

  /**
   * Retype this builder to the given type. Can only be called if {@link #canRetypeTo(long)} returns
   * true for the type.
   *
   * @param type the target type
   * @return a retyped builder
   */
  public abstract TypedBuilder retypeTo(long type);

  /** @return the current storage type of this builder */
  public abstract int getType();
}
