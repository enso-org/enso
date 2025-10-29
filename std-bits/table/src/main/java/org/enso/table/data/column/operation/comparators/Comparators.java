package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.NumericType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;

/**
 * Entry point for calling Comparators. Calls the appropriate comparator based on the type of the
 * left columns.
 */
public final class Comparators {
  public static boolean isSupported(Column left) {
    var storage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var storageType = storage.getType();

    return storageType instanceof DateType
        || storageType instanceof TimeOfDayType
        || storageType instanceof DateTimeType
        || storageType instanceof TextType
        || storageType instanceof NullType
        || storageType instanceof BooleanType
        || storageType instanceof NumericType;
  }

  public static BinaryOperationTyped<Boolean> eq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.EQ;
      case DateTimeType dt -> DateTimeComparators.EQ;
      case TimeOfDayType tm -> TimeOfDayComparators.EQ;
      case TextType tt -> StringComparators.EQ;
      case BooleanType bt -> BooleanComparators.EQ;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.EQUAL_OPERATION, false);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> notEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.NEQ;
      case DateTimeType dt -> DateTimeComparators.NEQ;
      case TimeOfDayType tm -> TimeOfDayComparators.NEQ;
      case TextType tt -> StringComparators.NEQ;
      case BooleanType bt -> BooleanComparators.NEQ;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.NOT_EQUAL_OPERATION, true);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> lessThan(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LT;
      case DateTimeType dt -> DateTimeComparators.LT;
      case TimeOfDayType tm -> TimeOfDayComparators.LT;
      case TextType tt -> StringComparators.LT;
      case BooleanType bt -> BooleanComparators.LT;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.LESS_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> lessThanEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.LTE;
      case DateTimeType dt -> DateTimeComparators.LTE;
      case TimeOfDayType tm -> TimeOfDayComparators.LTE;
      case TextType tt -> StringComparators.LTE;
      case BooleanType bt -> BooleanComparators.LTE;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.LESS_OR_EQUAL_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> greaterThan(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GT;
      case DateTimeType dt -> DateTimeComparators.GT;
      case TimeOfDayType tm -> TimeOfDayComparators.GT;
      case TextType tt -> StringComparators.GT;
      case BooleanType bt -> BooleanComparators.GT;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.GREATER_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> greaterThanEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    return switch (leftStorage.getType()) {
      case NullType nt -> NullComparators.INSTANCE;
      case DateType dt -> DateComparators.GTE;
      case DateTimeType dt -> DateTimeComparators.GTE;
      case TimeOfDayType tm -> TimeOfDayComparators.GTE;
      case TextType tt -> StringComparators.GTE;
      case BooleanType bt -> BooleanComparators.GTE;
      case NumericType nt ->
          NumericComparators.create(
              leftStorage.getType(), right, NumericComparators.GREATER_OR_EQUAL_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }
}
