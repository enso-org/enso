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
public interface StorageType<T> {
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
  default boolean isNumeric() {
    return false;
  }

  /**
   * @return true if the storage type has a date part.
   */
  default boolean hasDate() {
    return false;
  }

  /**
   * @return true if the storage type has a time part.
   */
  default boolean hasTime() {
    return false;
  }

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

  static StorageType<?> fromTypeCharAndSize(char typeChar, long size) {
    return switch (typeChar) {
      case 'A' -> AnyObjectType.INSTANCE;
      case 'B' -> BooleanType.INSTANCE;
      case 'D' -> BigDecimalType.INSTANCE;
      case 'E' -> BigIntegerType.INSTANCE;
      case 'F' -> {
        if (size != 64) {
          throw new IllegalArgumentException("Unknown float size: " + size);
        }
        yield FloatType.FLOAT_64;
      }
      case 'I' ->
          switch ((int) size) {
            case 8 -> IntegerType.INT_8;
            case 16 -> IntegerType.INT_16;
            case 32 -> IntegerType.INT_32;
            case 64 -> IntegerType.INT_64;
            default -> throw new IllegalArgumentException("Unknown integer size: " + size);
          };
      case 'N' -> NullType.INSTANCE;
      case 'S' -> size == -1 ? TextType.VARIABLE_LENGTH : TextType.variableLengthWithLimit(size);
      case 'T' -> TextType.fixedLength(size);
      case 'W' -> TimeOfDayType.INSTANCE;
      case 'X' -> DateType.INSTANCE;
      case 'Y' -> DateTimeType.INSTANCE_NO_TZ;
      case 'Z' -> DateTimeType.INSTANCE;
      default -> throw new IllegalArgumentException("Unknown type char: " + typeChar);
    };
  }

  /**
   * @return a character representing the type, used for serialization.
   */
  char typeChar();

  /**
   * @return the maximum length of the type if applicable, or -1 if not applicable (e.g. for
   *     variable-length)
   */
  default long size() {
    return -1;
  }
}
