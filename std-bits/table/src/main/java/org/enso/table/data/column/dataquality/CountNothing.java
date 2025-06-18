package org.enso.table.data.column.dataquality;

import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.ColumnStorageWithNothingMap;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.table.Column;
import org.enso.table.util.LeastRecentlyUsedCache;

import java.util.Map;

/** An operation for counting the number of Nothing values in a Column. */
public class CountNothing {
  private static Map<ColumnStorage<?>,  Long> _cachedCounts = Map.of();

  private static Map<ColumnStorage<?>,  Long> cachedCounts() {
    if (_cachedCounts == null) {
      _cachedCounts = new LeastRecentlyUsedCache<>(1000);
    }
    return _cachedCounts;
  }

  /** Counts the number of Nothing values in the given column. */
  public static long apply(Column column) {
    var storage = column.getStorage();
    return cachedCounts().computeIfAbsent(storage, CountNothing::applyToStorage);
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
  static long applyToStorage(ColumnStorage<?> storage) {
    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return withNothingMap.getIsNothingMap().cardinality();
    }

    var accumulator = new Accumulator();
    StorageIterators.forEachOverStorage(
        storage,
        true,
        (value, index) -> accumulator.process(value));
    return accumulator.getCount();
  }

  /** Returns true if any value in the storage is Nothing. */
  public static boolean anyNothing(ColumnStorage<?> storage) {
    Long cached = cachedCounts().getOrDefault(storage, null);
    if (cached != null) {
      return cached > 0;
    }

    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      return !withNothingMap.getIsNothingMap().isEmpty();
    }

    boolean hasNothing = StorageIterators.forEachOverStorage(
        storage,
        true,
        (index, value) -> value == null);
    if (hasNothing) {
      return true;
    }

    _cachedCounts.put(storage, 0L);
    return false;
  }

  /** Returns true if all values in the storage are Nothing. */
  public static boolean allNothing(ColumnStorage<?> storage) {
    Long cached = cachedCounts().getOrDefault(storage, null);
    if (cached != null) {
      return cached == storage.getSize();
    }

    if (storage.getType() instanceof NullType) {
      // If the storage is of NullType, it means all values are Nothing.
      cachedCounts().put(storage, storage.getSize());
      return true;
    }

    if (storage instanceof ColumnStorageWithNothingMap withNothingMap) {
      var cardinality = withNothingMap.getIsNothingMap().cardinality();
      cachedCounts().put(storage, (long)cardinality);
      return cardinality == storage.getSize();
    }

    boolean hasSomething = StorageIterators.forEachOverStorage(
        storage,
        true,
        (index, value) -> value != null);
    if (hasSomething) {
      return false;
    }

    cachedCounts().put(storage, (long) storage.getSize());
    return true;
  }
}
