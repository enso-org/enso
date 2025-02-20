package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithInferredStorage;
import org.enso.table.data.column.storage.Storage;
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
public interface Comparators {
  /* Get the storage of the column resolving through inferred storages. */
  static ColumnStorage<?> getStorage(Column input) {
    var storage = input.getStorage();
    if (storage instanceof ColumnStorageWithInferredStorage withInferredStorage) {
      var inferredStorage = withInferredStorage.getInferredStorage();
      if (inferredStorage != null) {
        return inferredStorage;
      }
    }
    return storage;
  }

  static boolean isSupported(Column left) {
    var storage = getStorage(left);
    var storageType = storage.getType();

    return storageType instanceof DateType
        || storageType instanceof TimeOfDayType
        || storageType instanceof DateTimeType
        || storageType instanceof TextType
        || storageType instanceof NullType;
  }

  static Column eq(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.EQ;
          case DateTimeType dt -> DateTimeComparators.EQ;
          case TimeOfDayType tm -> TimeOfDayComparators.EQ;
          case TextType tt -> StringComparators.EQ;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column notEq(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.NEQ;
          case DateTimeType dt -> DateTimeComparators.NEQ;
          case TimeOfDayType tm -> TimeOfDayComparators.NEQ;
          case TextType tt -> StringComparators.NEQ;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column lessThan(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.LT;
          case DateTimeType dt -> DateTimeComparators.LT;
          case TimeOfDayType tm -> TimeOfDayComparators.LT;
          case TextType tt -> StringComparators.LT;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column lessThanEq(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.LTE;
          case DateTimeType dt -> DateTimeComparators.LTE;
          case TimeOfDayType tm -> TimeOfDayComparators.LTE;
          case TextType tt -> StringComparators.LTE;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column greaterThan(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.GT;
          case DateTimeType dt -> DateTimeComparators.GT;
          case TimeOfDayType tm -> TimeOfDayComparators.GT;
          case TextType tt -> StringComparators.GT;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column greaterThanEq(Column left, Object rightValue, String newName) {
    var leftStorage = getStorage(left);
    var comparator =
        switch (leftStorage.getType()) {
          case NullType nt -> NullComparators.INSTANCE;
          case DateType dt -> DateComparators.GTE;
          case DateTimeType dt -> DateTimeComparators.GTE;
          case TimeOfDayType tm -> TimeOfDayComparators.GTE;
          case TextType tt -> StringComparators.GTE;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(leftStorage, rightValue, newName, comparator);
  }

  static Column performComparison(
      ColumnStorage<?> leftStorage, Object rightValue, String newName, Comparators comparator) {
    ColumnStorage<Boolean> output;
    if (rightValue instanceof Column right) {
      var rightStorage = getStorage(right);
      if (!comparator.canApply(leftStorage, rightStorage)) {
        throw new IllegalArgumentException("Cannot apply zip");
      }
      output = comparator.apply(leftStorage, rightStorage);
    } else {
      if (!comparator.canApplyMap(leftStorage, rightValue)) {
        throw new IllegalArgumentException("Cannot apply map");
      }
      output = comparator.applyMap(leftStorage, rightValue);
    }
    return new Column(newName, (Storage<Boolean>) output);
  }

  default boolean canApply(Column left, Object right) {
    return right instanceof Column rightColumn
        ? canApply(getStorage(left), getStorage(rightColumn))
        : canApplyMap(getStorage(left), right);
  }

  /** Can the map be applied to the pair of ColumnStorage and constant? */
  boolean canApplyMap(ColumnStorage<?> left, Object rightValue);

  /** Apply the map to the pair of ColumnStorage and constant. */
  ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue);

  /** Can the map be applied to the pair of ColumnStorage? */
  boolean canApply(ColumnStorage<?> left, ColumnStorage<?> right);

  /** Apply the map to the pair of ColumnStorage. */
  ColumnStorage<Boolean> apply(ColumnStorage<?> left, ColumnStorage<?> right);
}
