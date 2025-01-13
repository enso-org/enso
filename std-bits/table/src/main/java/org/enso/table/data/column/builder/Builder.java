package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.problems.ProblemAggregator;

import java.util.BitSet;

/** Interface defining a builder for creating columns dynamically. */
public interface Builder {
  /**
   * Constructs a builder accepting values of a specific type.
   *
   * <p>If {@code type} is {@code null}, it will return an {@link InferredBuilder} that will infer
   * the type from the data.
   */
  static Builder getForType(
      StorageType type, int size, ProblemAggregator problemAggregator) {
    Builder builder =
        switch (type) {
          case AnyObjectType x -> new MixedBuilder(size);
          case BooleanType x -> getForBoolean(size);
          case DateType x -> new DateBuilder(size);
          case DateTimeType x -> new DateTimeBuilder(size);
          case TimeOfDayType x -> new TimeOfDayBuilder(size);
          case FloatType floatType -> getForDouble(size, floatType, problemAggregator);
          case IntegerType integerType -> getForLong(size, integerType, problemAggregator);
          case TextType textType -> new StringBuilder(size, textType);
          case BigDecimalType x -> new BigDecimalBuilder(size);
          case BigIntegerType x -> new BigIntegerBuilder(size, problemAggregator);
          case null -> new InferredBuilder(size, problemAggregator);
        };
    assert java.util.Objects.equals(builder.getType(), type);
    return builder;
  }

  /**
   * Constructs a builder for storing booleans.
   *
   * @param size the initial size of the builder.
   */
  static BuilderForBoolean getForBoolean(int size) {
    return new BoolBuilder(size);
  }

  /**
   * Constructs a builder for storing integers.
   *
   * @param size the initial size of the builder.
   * @param integerType the type of integer to store.
   *                    This should be one of the {@link IntegerType} constants.
   * @param problemAggregator the problem aggregator to use for this builder.
   */
  static BuilderForLong getForLong(int size, IntegerType integerType, ProblemAggregator problemAggregator) {
    return LongBuilder.make(size, integerType, problemAggregator);
  }

  /**
   * Constructs a builder for storing floating-point numbers.
   *
   * @param size the initial size of the builder.
   * @param floatType the type of float to store.
   *                  This should be one of the {@link FloatType} constants.
   * @param problemAggregator the problem aggregator to use for this builder.
   */
  static BuilderForDouble getForDouble(int size, FloatType floatType, ProblemAggregator problemAggregator) {
    if (floatType.bits() != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }

    return new DoubleBuilder(new BitSet(), new long[size], 0, problemAggregator);
  }

  /**
   * Append a new item to this builder, assuming that it has enough allocated space.
   *
   * <p>This function should only be used when it is guaranteed that the builder has enough
   * capacity, for example if it was initialized with an initial capacity known up-front.
   *
   * @param o the item to append
   */
  void appendNoGrow(Object o);

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
   * Append a new item to this builder, increasing the capacity if necessary.
   *
   * @param o the item to append
   */
  void append(Object o);

  /**
   * Appends a specified number of missing values into the builder.
   *
   * <p>This operation should be equivalent to calling {@link #append(Object)} with {@code null} as
   * an argument, {@code count} times, however it may be implemented more efficiently by the
   * builder.
   *
   * @param count the number of missing values to append.
   */
  void appendNulls(int count);

  /**
   * Appends the whole contents of some other storage.
   *
   * <p>This may be used to efficiently copy a whole storage into the builder. Used for example when
   * concatenating columns.
   *
   * <p>If the provided storage type is not compatible with the type of this builder, a {@code
   * StorageTypeMismatch} exception may be thrown.
   */
  void appendBulkStorage(Storage<?> storage);

  /**
   * @return the number of appended elements
   */
  int getCurrentSize();

  /**
   * @return a storage containing all the items appended so far
   */
  Storage<?> seal();

  /**
   * @return the current storage type of this builder
   */
  StorageType getType();

  /**
   * Fills the given buffer with the data from this builder.
   *
   * <p>This is used when the builder is being converted to Mixed.
   *
   * @param items the buffer to dump elements into
   */
  void copyDataTo(Object[] items);

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
