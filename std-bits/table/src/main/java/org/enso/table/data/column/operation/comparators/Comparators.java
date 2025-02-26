package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;

/**
 * Entry point for calling Comparators. Calls the appropriate comparator based on the type of the
 * left columns.
 */
public interface Comparators extends BinaryOperation<Boolean> {
  static boolean isSupported(Column left) {
    var storage = BinaryOperation.getInferredStorage(left);
    var storageType = storage.getType();

    return storageType instanceof DateType
        || storageType instanceof TimeOfDayType
        || storageType instanceof DateTimeType
        || storageType instanceof TextType
        || storageType instanceof NullType;
  }

  static BinaryOperation eq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.EQ;
      case DateTimeType dt -> DateTimeComparators.EQ;
      case TimeOfDayType tm -> TimeOfDayComparators.EQ;
      case TextType tt -> StringComparators.EQ;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  static BinaryOperation notEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.NEQ;
      case DateTimeType dt -> DateTimeComparators.NEQ;
      case TimeOfDayType tm -> TimeOfDayComparators.NEQ;
      case TextType tt -> StringComparators.NEQ;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  static BinaryOperation lessThan(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LT;
      case DateTimeType dt -> DateTimeComparators.LT;
      case TimeOfDayType tm -> TimeOfDayComparators.LT;
      case TextType tt -> StringComparators.LT;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  static BinaryOperation lessThanEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LTE;
      case DateTimeType dt -> DateTimeComparators.LTE;
      case TimeOfDayType tm -> TimeOfDayComparators.LTE;
      case TextType tt -> StringComparators.LTE;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  static BinaryOperation greaterThan(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GT;
      case DateTimeType dt -> DateTimeComparators.GT;
      case TimeOfDayType tm -> TimeOfDayComparators.GT;
      case TextType tt -> StringComparators.GT;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  static BinaryOperation greaterThanEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GTE;
      case DateTimeType dt -> DateTimeComparators.GTE;
      case TimeOfDayType tm -> TimeOfDayComparators.GTE;
      case TextType tt -> StringComparators.GTE;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }
}
