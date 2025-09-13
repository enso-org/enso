package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.Column;

/** An operation for counting the number of Nothing values in a Column. */
public class CountNothing {
  /** Counts the number of Nothing values in the given column. */
  public static long apply(Column column) {
    return apply(column.getStorage());
  }

  private static class Accumulator {
    private long count = 0;

    public boolean process(Object value) {
      if (value == null) {
        count += 1;
      }
      return false;
    }

    public long getCount() {
      return count;
    }
  }

  /** Counts the number of Nothing values in the given storage. */
  public static long apply(ColumnStorage<?> storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return withNothingMap.getIsNothingMap().cardinality();
    }

    var accumulator = new Accumulator();
    StorageIterators.forEachOverStorage(
        storage, false, "CountNothing", (index, value) -> accumulator.process(value));
    return accumulator.getCount();
  }

  /** Returns true if any value in the storage is Nothing. */
  public static boolean anyNothing(ColumnStorage<?> storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return !withNothingMap.getIsNothingMap().isEmpty();
    }

    return StorageIterators.forEachOverStorage(
        storage, false, "anyNothing", (index, value) -> value == null);
  }

  /** Returns true if all values in the storage are Nothing. */
  public static boolean allNothing(ColumnStorage<?> storage) {
    if (storage.getType() instanceof NullType) {
      // If the storage is of NullType, it means all values are Nothing.
      return true;
    }

    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      var cardinality = withNothingMap.getIsNothingMap().cardinality();
      return cardinality == storage.getSize();
    }

    boolean hasSomething =
        StorageIterators.forEachOverStorage(
            storage, false, "allNothing", (index, value) -> value != null);
    return !hasSomething;
  }
}
