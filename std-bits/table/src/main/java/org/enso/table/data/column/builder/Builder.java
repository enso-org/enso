package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.problems.WithAggregatedProblems;

/** A builder for creating columns dynamically. */
public abstract class Builder {
  /** Constructs a builder accepting values of a specific type.
   * <p>
   * If {@code type} is {@code null}, it will return an {@link InferredBuilder} that will infer the type from the data.
   */
  public static Builder getForType(StorageType type, int size) {
    Builder builder = switch (type) {
      case AnyObjectType x -> new MixedBuilder(size);
      case BooleanType x -> new BoolBuilder(size);
      case DateType x -> new DateBuilder(size);
      case DateTimeType x -> new DateTimeBuilder(size);
      case TimeOfDayType x -> new TimeOfDayBuilder(size);
      case FloatType floatType -> switch (floatType.bits()) {
        case BITS_64 -> NumericBuilder.createDoubleBuilder(size);
        default -> throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
      };
      case IntegerType integerType -> NumericBuilder.createLongBuilder(size, integerType);
      case TextType textType -> new StringBuilder(size, textType);
      case BigIntegerType x -> new BigIntegerBuilder(size);
      case null -> new InferredBuilder(size);
    };
    assert builder.getType().equals(type);
    return builder;
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

  /** @return the current storage type of this builder */
  public abstract StorageType getType();

  /** @return any problems that occurred when building the Storage. */
  public AggregatedProblems getProblems() {
    return AggregatedProblems.of();
  }

  public WithAggregatedProblems<Storage<?>> sealWithProblems() {
    return new WithAggregatedProblems<>(seal(), getProblems());
  }

  /** Adds nulls to the builder to ensure that it reaches the size specified. */
  public void fillUpToSize(int size) {
    int currentSize = getCurrentSize();
    if (currentSize > size) {
      throw new IllegalArgumentException("fillUpToSize("+size+") called on a builder that already has "+currentSize+" elements.");
    }

    if (currentSize < size) {
      int nullsToAppend = size - currentSize;
      appendNulls(nullsToAppend);
    }
  }
}
