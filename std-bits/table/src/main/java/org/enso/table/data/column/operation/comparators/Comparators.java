package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.operation.BinaryOperation;
import org.enso.table.data.column.storage.type.BooleanType;
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
public final class Comparators {
  public static boolean isSupported(Column left) {
    var storage = BinaryOperation.getInferredStorage(left);
    var storageType = storage.getType();

    return storageType instanceof DateType
        || storageType instanceof TimeOfDayType
        || storageType instanceof DateTimeType
        || storageType instanceof TextType
        || storageType instanceof NullType
        || storageType instanceof BooleanType;
  }

  public static BinaryOperation<Boolean> eq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.EQ;
      case DateTimeType dt -> DateTimeComparators.EQ;
      case TimeOfDayType tm -> TimeOfDayComparators.EQ;
      case TextType tt -> StringComparators.EQ;
      case BooleanType bt -> BooleanComparators.EQ;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperation<Boolean> notEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.NEQ;
      case DateTimeType dt -> DateTimeComparators.NEQ;
      case TimeOfDayType tm -> TimeOfDayComparators.NEQ;
      case TextType tt -> StringComparators.NEQ;
      case BooleanType bt -> BooleanComparators.NEQ;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperation<Boolean> lessThan(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LT;
      case DateTimeType dt -> DateTimeComparators.LT;
      case TimeOfDayType tm -> TimeOfDayComparators.LT;
      case TextType tt -> StringComparators.LT;
      case BooleanType bt -> BooleanComparators.LT;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperation<Boolean> lessThanEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LTE;
      case DateTimeType dt -> DateTimeComparators.LTE;
      case TimeOfDayType tm -> TimeOfDayComparators.LTE;
      case TextType tt -> StringComparators.LTE;
      case BooleanType bt -> BooleanComparators.LTE;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperation<Boolean> greaterThan(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GT;
      case DateTimeType dt -> DateTimeComparators.GT;
      case TimeOfDayType tm -> TimeOfDayComparators.GT;
      case TextType tt -> StringComparators.GT;
      case BooleanType bt -> BooleanComparators.GT;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperation greaterThanEq(Column left) {
    var leftStorage = BinaryOperation.getInferredStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GTE;
      case DateTimeType dt -> DateTimeComparators.GTE;
      case TimeOfDayType tm -> TimeOfDayComparators.GTE;
      case TextType tt -> StringComparators.GTE;
      case BooleanType bt -> BooleanComparators.GTE;
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }
}
