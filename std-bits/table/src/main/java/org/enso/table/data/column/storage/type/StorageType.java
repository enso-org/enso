package org.enso.table.data.column.storage.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.PreciseTypeOptions;
import org.enso.table.problems.ProblemAggregator;

/**
 * Represents an underlying internal storage type that can be mapped to the Value Type that is
 * exposed to users.
 */
public sealed interface StorageType<T>
    permits AnyObjectType,
        BigDecimalType,
        BigIntegerType,
        BooleanType,
        DateTimeType,
        DateType,
        FloatType,
        IntegerType,
        NullType,
        TextType,
        TimeOfDayType {
  /**
   * @param item the item whose type is to be determined.
   * @param options specifies details on how the precise type should be determined
   * @return the StorageType that represents a given boxed item.
   */
  static StorageType<?> forBoxedItem(Object item, PreciseTypeOptions options) {
    if (NumericConverter.isCoercibleToLong(item)) {
      return findSmallestIntegerType(item, options);
    }

    if (NumericConverter.isFloatLike(item)) {
      double value = NumericConverter.coerceToDouble(item);
      return findSmallestTypeForFloat(value, options);
    }

    if (item instanceof String itemString) {
      return findSmallestTypeForText(itemString, options);
    }

    return switch (item) {
      case BigDecimal i -> BigDecimalType.INSTANCE;
      case BigInteger i -> BigIntegerType.INSTANCE;
      case Boolean b -> BooleanType.INSTANCE;
      case LocalDate d -> DateType.INSTANCE;
      case LocalTime t -> TimeOfDayType.INSTANCE;
      case LocalDateTime d -> DateTimeType.INSTANCE;
      case ZonedDateTime d -> DateTimeType.INSTANCE;
      default -> AnyObjectType.INSTANCE;
    };
  }

  private static IntegerType findSmallestIntegerType(Object item, PreciseTypeOptions options) {
    if (options.shrinkIntegers()) {
      long value = NumericConverter.coerceToLong(item);
      return IntegerType.smallestFitting(value, false);
    }

    return IntegerType.INT_64;
  }

  private static StorageType<? extends Number> findSmallestTypeForFloat(
      double item, PreciseTypeOptions options) {
    if (options.wholeFloatsBecomeIntegers() && item % 1.0 == 0.0 && IntegerType.INT_64.fits(item)) {
      if (options.shrinkIntegers()) {
        return IntegerType.smallestFitting((long) item, false);
      }

      return IntegerType.INT_64;
    }

    return FloatType.FLOAT_64;
  }

  private static TextType findSmallestTypeForText(String item, PreciseTypeOptions options) {
    if (options.shrinkText()) {
      return TextType.preciseTypeForValue(item);
    } else {
      return TextType.VARIABLE_LENGTH;
    }
  }

  /**
   * @return true if the storage type is numeric.
   */
  boolean isNumeric();

  /**
   * @return true if the storage type has a date part.
   */
  boolean hasDate();

  /**
   * @return true if the storage type has a time part.
   */
  boolean hasTime();

  /**
   * @return true if the storage type is of the same type as the other.
   */
  boolean isOfType(StorageType<?> other);

  /** Convert the value to the type if possible or return null if not. */
  T valueAsType(Object value);

  /**
   * Creates a builder for the StorageType.
   *
   * @return a builder for the given type.
   */
  BuilderForType<T> makeBuilder(long initialCapacity, ProblemAggregator problemAggregator);

  /**
   * Types the Storage as a specific generic type. Allows for using the storage as a specific type
   * in the code.
   *
   * @param storage the storage to type. Must be of the same type as the StorageType. If it is not,
   *     an IllegalArgumentException will be thrown.
   * @return the storage as a typed storage.
   */
  ColumnStorage<T> asTypedStorage(ColumnStorage<?> storage);
}
