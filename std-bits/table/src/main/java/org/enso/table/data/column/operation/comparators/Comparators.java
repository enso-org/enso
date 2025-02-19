package org.enso.table.data.column.operation.comparators;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;

/**
 * Entry point for calling Comparators. Calls the appropriate comparator based on the type of the
 * left columns.
 */
public interface Comparators {
  static Column Eq(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.EQ;
          case TimeOfDayType tm -> TimeOfDayComparators.EQ;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  static Column NotEq(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.NEQ;
          case TimeOfDayType tm -> TimeOfDayComparators.NEQ;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  static Column LT(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.LT;
          case TimeOfDayType tm -> TimeOfDayComparators.LT;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  static Column LTE(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.LTE;
          case TimeOfDayType tm -> TimeOfDayComparators.LTE;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  static Column GT(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.GT;
          case TimeOfDayType tm -> TimeOfDayComparators.GT;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  static Column GTE(Column left, Object rightValue, String newName) {
    var leftStorage = left.getStorage();
    var comparator =
        switch (leftStorage.getType()) {
          case DateType dt -> DateComparators.GTE;
          case TimeOfDayType tm -> TimeOfDayComparators.GTE;
          default -> throw new IllegalArgumentException("Unsupported StorageType");
        };

    return performComparison(rightValue, newName, comparator, leftStorage);
  }

  private static Column performComparison(
      Object rightValue, String newName, Comparators comparator, Storage<?> leftStorage) {
    ColumnStorage<Boolean> output;
    if (rightValue instanceof Column right) {
      var rightStorage = right.getStorage();
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

  /** Can the map be applied to pair of ColumnStorage and constant? */
  boolean canApplyMap(ColumnStorage<?> left, Object rightValue);

  /** Apply the map to the pair of ColumnStorage and constant. */
  ColumnStorage<Boolean> applyMap(ColumnStorage<?> left, Object rightValue);

  /** Can the map be applied to pair of ColumnStorage? */
  boolean canApply(ColumnStorage<?> left, ColumnStorage<?> right);

  /** Apply the map to the pair of ColumnStorage. */
  ColumnStorage<Boolean> apply(ColumnStorage<?> left, ColumnStorage<?> right);
}
