package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.Storage;

/** A builder for creating columns dynamically. */
public abstract class Builder {
  public static Builder getForType(int type, int size) {
    return switch (type) {
      case Storage.Type.OBJECT -> new ObjectBuilder(size);
      case Storage.Type.LONG -> NumericBuilder.createLongBuilder(size);
      case Storage.Type.DOUBLE -> NumericBuilder.createDoubleBuilder(size);
      case Storage.Type.STRING -> new StringBuilder(size);
      case Storage.Type.BOOL -> new BoolBuilder();
      case Storage.Type.DATE -> new DateBuilder(size);
      case Storage.Type.TIME_OF_DAY -> new TimeOfDayBuilder(size);
      case Storage.Type.DATE_TIME -> new DateTimeBuilder(size);
      default -> new InferredBuilder(size);
    };
  }

  /**
   * Append a new item to this builder, assuming that it has enough allocated space.
   *
   * <p>This function should only be used when it is guaranteed that the builder has enough
   * capacity, for example if it was initialized with an initial capacity known up-front.
   *
   * @param o the item to append
   */
  public abstract void appendNoGrow(Object o);

  /**
   * Append a new item to this builder, increasing the capacity if necessary.
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

  /**
   * Appends the whole contents of some other storage.
   *
   * <p>This may be used to efficiently copy a whole storage into the builder. Used for example when
   * concatenating columns.
   *
   * <p>If the provided storage type is not compatible with the type of this builder, a {@code
   * StorageTypeMismatch} exception may be thrown.
   */
  public abstract void appendBulkStorage(Storage<?> storage);

  /**
   * @return the number of appended elements
   */
  public abstract int getCurrentSize();

  /**
   * @return a storage containing all the items appended so far
   */
  public abstract Storage<?> seal();
}
