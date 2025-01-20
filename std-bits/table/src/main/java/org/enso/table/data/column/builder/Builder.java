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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;

/** Interface defining a builder for creating columns dynamically. */
public interface Builder {
  /**
   * Constructs a builder accepting values of a specific type.
   *
   * <p>If {@code type} is {@code null}, it will return an {@link InferredBuilder} that will infer
   * the type from the data.
   */
  static Builder getForType(StorageType type, int size, ProblemAggregator problemAggregator) {
    Builder builder =
        switch (type) {
          case AnyObjectType _ -> new MixedBuilder(size);
          case BooleanType _ -> getForBoolean(size);
          case DateType _ -> getForDate(size);
          case DateTimeType _ -> getForDateTime(size);
          case TimeOfDayType _ -> getForTime(size);
          case FloatType floatType -> getForDouble(floatType, size, problemAggregator);
          case IntegerType integerType -> getForLong(integerType, size, problemAggregator);
          case TextType textType -> getForText(size, textType);
          case BigDecimalType _ -> getForBigDecimal(size);
          case BigIntegerType _ -> getForBigInteger(size, problemAggregator);
          case null -> getInferredBuilder(size, problemAggregator);
        };
    assert java.util.Objects.equals(builder.getType(), type);
    return builder;
  }

  /**
   * Constructs a builder for deducing the type of the column based on
   *
   * @param size the initial size of the builder.
   */
  static Builder getInferredBuilder(int size, ProblemAggregator problemAggregator) {
    return new InferredBuilder(size, problemAggregator, false);
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
   * @param integerType the type of integer to store. This should be one of the {@link IntegerType}
   *     constants.
   * @param problemAggregator the problem aggregator to use for this builder.
   */
  static BuilderForLong getForLong(
      IntegerType integerType, int size, ProblemAggregator problemAggregator) {
    return LongBuilder.make(size, integerType, problemAggregator);
  }

  /**
   * Constructs a builder for storing floating-point numbers.
   *
   * @param size the initial size of the builder.
   * @param floatType the type of float to store. This should be one of the {@link FloatType}
   *     constants.
   * @param problemAggregator the problem aggregator to use for this builder.
   */
  static BuilderForDouble getForDouble(
      FloatType floatType, int size, ProblemAggregator problemAggregator) {
    if (floatType.bits() != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }

    return new DoubleBuilder(size, problemAggregator);
  }

  /**
   * Constructs a builder for storing objects.
   * No operations will be supported on this builder.
   *
   * @param size the initial size of the builder.
   */
  static Builder getObjectBuilder(int size) {
    return new ObjectBuilder(size);
  }

  static BuilderForType<BigDecimal> getForBigDecimal(int size) {
    return new BigDecimalBuilder(size);
  }

  static BuilderForType<BigInteger> getForBigInteger(int size, ProblemAggregator problemAggregator) {
    return new BigIntegerBuilder(size, problemAggregator);
  }

  static BuilderForType<LocalDate> getForDate(int size) {
    return new DateBuilder(size, false);
  }

  static BuilderForType<ZonedDateTime> getForDateTime(int size) {
    return new DateTimeBuilder(size, false);
  }

  static BuilderForType<String> getForText(int size, TextType textType) {
    return new StringBuilder(size, textType);
  }

  static BuilderForType<LocalTime> getForTime(int size) {
    return new TimeOfDayBuilder(size);
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
}
