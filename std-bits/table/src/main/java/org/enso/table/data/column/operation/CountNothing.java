package org.enso.table.data.column.operation;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.table.Column;
import org.graalvm.polyglot.Context;

/** An operation for counting the number of Nothing values in a Column. */
public class CountNothing {
  /** Counts the number of Nothing values in the given column. */
  public static long apply(Column column) {
    ColumnStorage storage = column.getStorage();
    return applyToStorage(storage);
  }

  /** Counts the number of Nothing values in the given storage. */
  public static long applyToStorage(ColumnStorage storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return withNothingMap.getIsNothingMap().cardinality();
    }

    Context context = Context.getCurrent();
    long count = 0;
    for (long i = 0; i < storage.getSize(); i++) {
      if (storage.isNothing(i)) {
        count += 1;
      }
      context.safepoint();
    }
    return count;
  }

  /** Returns true if any value in the storage is Nothing. */
  public static boolean anyNothing(ColumnStorage storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return !withNothingMap.getIsNothingMap().isEmpty();
    }

    Context context = Context.getCurrent();
    for (long i = 0; i < storage.getSize(); i++) {
      if (storage.isNothing(i)) {
        return true;
      }
      context.safepoint();
    }
    return false;
  }

  /** Returns true if all values in the storage are Nothing. */
  public static boolean allNothing(ColumnStorage storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return withNothingMap.getIsNothingMap().nextClearBit(0) >= storage.getSize();
    }

    Context context = Context.getCurrent();
    for (long i = 0; i < storage.getSize(); i++) {
      if (!storage.isNothing(i)) {
        return false;
      }
      context.safepoint();
    }
    return true;
  }
}
