package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.type.StorageType;

/** Used by the InferredBuilder to allow for retyping of the current builder. */
public interface BuilderWithRetyping extends Builder {
  /**
   * Specifies if the following object will be accepted by this builder's append* methods.
   *
   * <p>This is used to determine if a given value can be appended to the current builder, or if it
   * needs to be retyped to a more general one.
   *
   * <p>Note that the {@code appendBulkStorage} method may still accept more types than {@code
   * accept}. This is exploited by operations like Union where more flexibility in merging column
   * types is allowed than in building new columns from scratch.
   */
  boolean accepts(Object o);

  /**
   * Checks if the builder can be efficiently retyped to the given storage type.
   *
   * @param type the storage type
   * @return whether the column can be retyped
   */
  boolean canRetypeTo(StorageType type);

  /**
   * Retype this builder to the given type. Can only be called if {@link #canRetypeTo(StorageType)}
   * returns true for the type.
   *
   * @param type the target type
   * @return a retyped builder
   */
  Builder retypeTo(StorageType type);
}
