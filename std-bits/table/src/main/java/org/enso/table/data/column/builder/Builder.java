package org.enso.table.data.column.builder;

import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.BitSet;
import org.enso.table.data.column.operation.masks.IndexMapper;
import org.enso.table.data.column.operation.masks.MaskOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageProxy;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.Bits;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

/** Interface defining a builder for creating columns dynamically. */
public interface Builder {
  /**
   * The maximum size of a builder. Currently, just the maximum value of an integer, but should be
   * tested and limited. For array based builders, must be less than the maximum array size.
   */
  int MAX_SIZE = Integer.MAX_VALUE;

  /** Checks that the size is within the maximum allowed. */
  static int checkSize(long size) {
    if (size > MAX_SIZE) {
      throw new IllegalArgumentException("Columns cannot exceed " + MAX_SIZE + " rows.");
    }

    return (int) size;
  }

  static ColumnStorage<?> fromRepeatedItem(Object item, long size) {
    if (size < 0) {
      throw new IllegalArgumentException("Repeat count must be non-negative.");
    }

    // Create a single storage item based on the type of the item.
    return switch (item) {
      case null -> new NullBuilder().appendNulls(checkSize(size)).seal();
      case Boolean booleanValue -> {
        var s = checkSize(size);
        var validity = new BitSet();
        validity.set(0, s, true);
        yield new BoolStorage(new BitSet(), validity, s, booleanValue);
      }
      default -> {
        var storageType = StorageType.forBoxedItem(item, PreciseTypeOptions.DEFAULT);
        var builder = Builder.getForType(storageType, size, BlackholeProblemAggregator.INSTANCE);
        builder.append(item);
        yield size == 1
            ? builder.seal()
            : MaskOperation.getSlicedStorage(builder.seal(), new IndexMapper.Constant(size));
      }
    };
  }

  static <T> ColumnStorage<T> makeEmpty(StorageType<T> storageType, long size) {
    if (size < 0) {
      throw new IllegalArgumentException("Repeat count must be non-negative.");
    }

    if (storageType instanceof NullType) {
      return storageType.asTypedStorage(new NullBuilder().appendNulls(checkSize(size)).seal());
    }

    var builder = Builder.getForType(storageType, size, BlackholeProblemAggregator.INSTANCE);
    builder.appendNulls(1);
    var unTyped =
        size == 1
            ? builder.seal()
            : MaskOperation.getSlicedStorage(builder.seal(), new IndexMapper.Constant(size));
    return storageType.asTypedStorage(unTyped);
  }

  /**
   * Converts a proxy storage to local storage.
   *
   * @param <T> type of storage
   * @param storage the storage instance, possibly a {@link Proxy#isProxyClass proxy}
   * @return either {@code storage} itself, or optimized storage of the same {@link
   *     ColumnStorage#typeChar()} and {@link ColumnStorage#typeSize()} over the same {@link
   *     ColumnStorage#addressOfData() data}
   */
  static <T> ColumnStorage<T> makeLocal(ColumnStorage<T> storage) {
    if (!Proxy.isProxyClass(storage.getClass())) {
      return storage;
    }

    var storageType = StorageType.ofStorage(storage);
    var size = Math.toIntExact(storage.getSize());

    if (size == 0) {
      return Builder.makeEmpty(storageType, 0);
    }

    if (storageType instanceof NullType) {
      var b = new NullBuilder();
      b.appendNulls(size);
      return storageType.asTypedStorage(b.seal());
    }

    var data = storage.addressOfData();
    if (data != 0) {
      var validity = storage.addressOfValidity();

      var localStorage =
          switch (storageType) {
            case BooleanType _ -> BoolBuilder.fromAddress(size, data, validity).seal(storage);
            case IntegerType type ->
                LongBuilder.fromAddress(size, data, validity, type).seal(storage);
            case FloatType _ -> DoubleBuilder.fromAddress(size, data, validity).seal(storage);
            case TextType type ->
                StringBuilder.fromAddress(size, data, validity, type).seal(storage);
            case DateType _ -> DateBuilder.fromAddress(size, data, validity).seal(storage);
            case DateTimeType _ -> DateTimeBuilder.fromAddress(size, data, validity).seal(storage);
            case TimeOfDayType _ ->
                TimeOfDayBuilder.fromAddress(size, data, validity).seal(storage);
            default -> {
              // Currently: BigInteger, BigDecimal, AnyObject
              BuilderUtil.LOGGER.warn(
                  "Unable to make local buffer based storage for {} size {}", storageType, size);
              yield null;
            }
          };

      if (localStorage != null) {
        assert assertSameStorages(storage, localStorage);
        return storageType.asTypedStorage(localStorage);
      }
    }

    // Handle BigInteger specially
    if (storageType instanceof BigIntegerType) {
      var b = Builder.getForBigInteger(size, null);
      b.appendBulkStorage(storage);
      var localStorage = b.seal();
      return storageType.asTypedStorage(localStorage);
    }

    // Fallback and use a ColumnStorageProxy
    var proxiedStorage = ColumnStorageProxy.create(storageType, size, storage);
    return storageType.asTypedStorage(proxiedStorage);
  }

  private static boolean assertSameStorages(ColumnStorage<?> s1, ColumnStorage<?> s2) {
    var sb = new java.lang.StringBuilder();
    if (s1.getSize() != s2.getSize()) {
      sb.append("Unexpected size %d != %d\n".formatted(s1.getSize(), s2.getSize()));
    }
    if (s1.typeChar() != s2.typeChar()) {
      sb.append("Unexpected type %s != %s\n".formatted(s1.typeChar(), s2.typeChar()));
    }
    if (s1.typeSize() != s2.typeSize()) {
      sb.append("Unexpected type %d != %d\n".formatted(s1.typeSize(), s2.typeSize()));
    }
    assert sb.isEmpty() : sb;
    return sb.isEmpty();
  }

  /**
   * Constructs a builder accepting values of a specific type.
   *
   * <p>If {@code type} is {@code null}, it will return an {@link InferredBuilder} that will infer
   * the type from the data.
   */
  static Builder getForType(
      StorageType<?> storageType, long size, ProblemAggregator problemAggregator) {
    return switch (storageType) {
      case AnyObjectType _ -> getForAnyObject(size);
      case BooleanType _ -> getForBoolean(size);
      case DateType _ -> getForDate(size);
      case DateTimeType _ -> getForDateTime(size);
      case TimeOfDayType _ -> getForTime(size);
      case FloatType floatType -> getForDouble(floatType, size, problemAggregator);
      case IntegerType integerType -> getForLong(integerType, size, problemAggregator);
      case TextType textType -> getForText(textType, size);
      case BigDecimalType _ -> getForBigDecimal(size);
      case BigIntegerType _ -> getForBigInteger(size, problemAggregator);
      case NullType _ -> new NullBuilder();
      case null -> getInferredBuilder(size, problemAggregator);
    };
  }

  /**
   * Constructs a builder for deducing the type of the column based on
   *
   * @param size the initial size of the builder.
   */
  static Builder getInferredBuilder(long size, ProblemAggregator problemAggregator) {
    int checkedSize = checkSize(size);
    return new InferredBuilder(checkedSize, problemAggregator, false);
  }

  /**
   * Constructs a builder for storing booleans.
   *
   * @param size the initial size of the builder.
   */
  static BuilderForBoolean getForBoolean(long size) {
    int checkedSize = checkSize(size);
    return new BoolBuilder(checkedSize);
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
      IntegerType integerType, long size, ProblemAggregator problemAggregator) {
    int checkedSize = checkSize(size);
    return LongBuilder.make(checkedSize, integerType, problemAggregator);
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
      FloatType floatType, long size, ProblemAggregator problemAggregator) {
    if (floatType.bits() != Bits.BITS_64) {
      throw new IllegalArgumentException("Only 64-bit floats are currently supported.");
    }

    int checkedSize = checkSize(size);
    return new DoubleBuilder(checkedSize, problemAggregator);
  }

  /**
   * Constructs a builder for storing objects. No operations will be supported on this builder.
   *
   * @param size the initial size of the builder.
   */
  static Builder getObjectBuilder(long size) {
    int checkedSize = checkSize(size);
    return new ObjectBuilder(checkedSize);
  }

  static BuilderForType<Object> getForAnyObject(long size) {
    int checkedSize = checkSize(size);
    return new MixedBuilder(checkedSize);
  }

  static BuilderForType<BigDecimal> getForBigDecimal(long size) {
    int checkedSize = checkSize(size);
    return new BigDecimalBuilder(checkedSize);
  }

  static BuilderForType<BigInteger> getForBigInteger(
      long size, ProblemAggregator problemAggregator) {
    int checkedSize = checkSize(size);
    return new BigIntegerBuilder(checkedSize, problemAggregator);
  }

  static BuilderForType<LocalDate> getForDate(long size) {
    int checkedSize = checkSize(size);
    return new DateBuilder(checkedSize, false);
  }

  static BuilderForType<ZonedDateTime> getForDateTime(long size) {
    int checkedSize = checkSize(size);
    return new DateTimeBuilder(checkedSize, false);
  }

  static BuilderForType<String> getForText(TextType textType, long size) {
    int checkedSize = checkSize(size);
    return new StringBuilder(checkedSize, textType);
  }

  static BuilderForType<LocalTime> getForTime(long size) {
    int checkedSize = checkSize(size);
    return new TimeOfDayBuilder(checkedSize);
  }

  /**
   * Append a new item to this builder, increasing the capacity if necessary.
   *
   * @param o the item to append
   */
  Builder append(Object o);

  /**
   * Appends a specified number of missing values into the builder.
   *
   * <p>This operation should be equivalent to calling {@link #append(Object)} with {@code null} as
   * an argument, {@code count} times, however it may be implemented more efficiently by the
   * builder.
   *
   * @param count the number of missing values to append.
   */
  Builder appendNulls(int count);

  /**
   * Appends the whole contents of some other column.
   *
   * <p>This may be used to efficiently copy a whole column into the builder. Used for example when
   * concatenating columns.
   *
   * <p>If the provided storage type is not compatible with the type of this builder, a {@code
   * StorageTypeMismatch} exception may be thrown.
   */
  default void appendBulkStorage(Column column) {
    appendBulkStorage(column.getStorage());
  }

  /**
   * Appends the whole contents of some other storage.
   *
   * <p>This may be used to efficiently copy a whole storage into the builder. Used for example when
   * concatenating columns.
   *
   * <p>If the provided storage type is not compatible with the type of this builder, a {@code
   * StorageTypeMismatch} exception may be thrown.
   */
  void appendBulkStorage(ColumnStorage<?> storage);

  /**
   * @return the number of appended elements
   */
  long getCurrentSize();

  /**
   * @return a storage containing all the items appended so far
   */
  ColumnStorage<?> seal();

  /**
   * Fills the given buffer with the data from this builder.
   *
   * <p>This is used when the builder is being converted to Mixed.
   *
   * @param items the buffer to dump elements into
   */
  void copyDataTo(Object[] items);
}
