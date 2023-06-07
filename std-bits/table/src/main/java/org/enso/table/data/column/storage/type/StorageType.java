package org.enso.table.data.column.storage.type;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZonedDateTime;

/**
 * Represents an underlying internal storage type that can be mapped to the Value Type that is exposed to users.
 */
public sealed interface StorageType permits AnyObjectType, BooleanType, DateType, DateTimeType, FloatType, IntegerType, TextType, TimeOfDayType {
  /**
   * @return the a common StorageType the two types.
   */
  static StorageType findGeneralType(StorageType firstType, StorageType secondType) {
    if (firstType == null || firstType.equals(secondType)) {
      return secondType;
    }

    return switch (firstType) {
      case IntegerType i -> switch (secondType) {
        case IntegerType i2 -> i.bits().toInteger() >= i2.bits().toInteger() ? i : i2;
        case FloatType f -> FloatType.FLOAT_64;
        default -> AnyObjectType.INSTANCE;
      };
      case FloatType f -> switch (secondType) {
        case IntegerType i -> FloatType.FLOAT_64;
        case FloatType f2 -> f.bits().toInteger() >= f2.bits().toInteger() ? f : f2;
        default -> AnyObjectType.INSTANCE;
      };
      default -> AnyObjectType.INSTANCE;
    };
  }

  /**
   * @return the StorageType that represents a given boxed item.
   */
  static StorageType forBoxedItem(Object item) {
    return switch (item) {
      case Boolean b -> BooleanType.INSTANCE;
      case Byte b -> IntegerType.INT_64;
      case Short s -> IntegerType.INT_64;
      case Integer i -> IntegerType.INT_64;
      case Long l -> IntegerType.INT_64;
      case Float f -> FloatType.FLOAT_64;
      case Double d -> FloatType.FLOAT_64;
      case LocalDate d -> DateType.INSTANCE;
      case LocalTime t -> TimeOfDayType.INSTANCE;
      case LocalDateTime d -> DateTimeType.INSTANCE;
      case ZonedDateTime d -> DateTimeType.INSTANCE;
      case String s -> TextType.VARIABLE_LENGTH;
      default -> null;
    };
  }
}
