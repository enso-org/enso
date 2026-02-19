package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.operation.BinaryOperationTyped;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.NumericType;
import org.enso.table.data.column.storage.type.StorageType;
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
    var storageType = StorageType.ofStorage(storage);

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
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.EQ;
      case DateTimeType _ -> DateTimeComparators.EQ;
      case TimeOfDayType _ -> TimeOfDayComparators.EQ;
      case TextType _ -> StringComparators.EQ;
      case BooleanType _ -> BooleanComparators.EQ;
      case NumericType _ ->
          NumericComparators.create(
              leftStorageType, right, NumericComparators.EQUAL_OPERATION, false);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> notEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.NEQ;
      case DateTimeType _ -> DateTimeComparators.NEQ;
      case TimeOfDayType _ -> TimeOfDayComparators.NEQ;
      case TextType _ -> StringComparators.NEQ;
      case BooleanType _ -> BooleanComparators.NEQ;
      case NumericType _ ->
          NumericComparators.create(
              leftStorageType, right, NumericComparators.NOT_EQUAL_OPERATION, true);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> lessThan(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.LT;
      case DateTimeType _ -> DateTimeComparators.LT;
      case TimeOfDayType _ -> TimeOfDayComparators.LT;
      case TextType _ -> StringComparators.LT;
      case BooleanType _ -> BooleanComparators.LT;
      case NumericType _ ->
          NumericComparators.create(leftStorageType, right, NumericComparators.LESS_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> lessThanEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.LTE;
      case DateTimeType _ -> DateTimeComparators.LTE;
      case TimeOfDayType _ -> TimeOfDayComparators.LTE;
      case TextType _ -> StringComparators.LTE;
      case BooleanType _ -> BooleanComparators.LTE;
      case NumericType _ ->
          NumericComparators.create(
              leftStorageType, right, NumericComparators.LESS_OR_EQUAL_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> greaterThan(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.GT;
      case DateTimeType _ -> DateTimeComparators.GT;
      case TimeOfDayType _ -> TimeOfDayComparators.GT;
      case TextType _ -> StringComparators.GT;
      case BooleanType _ -> BooleanComparators.GT;
      case NumericType _ ->
          NumericComparators.create(leftStorageType, right, NumericComparators.GREATER_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }

  public static BinaryOperationTyped<Boolean> greaterThanEq(Column left, Object right) {
    var leftStorage = ColumnStorageWithInferredStorage.resolveStorage(left);
    var leftStorageType = StorageType.ofStorage(leftStorage);
    return switch (leftStorageType) {
      case NullType _ -> NullComparators.INSTANCE;
      case DateType _ -> DateComparators.GTE;
      case DateTimeType _ -> DateTimeComparators.GTE;
      case TimeOfDayType _ -> TimeOfDayComparators.GTE;
      case TextType _ -> StringComparators.GTE;
      case BooleanType _ -> BooleanComparators.GTE;
      case NumericType _ ->
          NumericComparators.create(
              leftStorageType, right, NumericComparators.GREATER_OR_EQUAL_OPERATION);
      default -> throw new IllegalArgumentException("Unsupported StorageType");
    };
  }
}
