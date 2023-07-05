package org.enso.table.data.index;

import java.util.Comparator;

import org.enso.base.ObjectComparator;
import org.enso.table.data.column.storage.Storage;

/**
 * A multi-value key for ordered operations like sorting.
 *
 * <p>It is meant to be used by sorted collections relying on {@code compareTo}, like {@code
 * TreeMap}. It uses an {@code objectComparator} that should expose the Enso comparison logic to the
 * Java-verse.
 *
 * <p>It currently does not support hashing, as we do not have a hashing implementation consistent
 * with Enso's comparison semantics.
 */
public class OrderedMultiValueKey extends MultiValueKeyBase
    implements Comparable<OrderedMultiValueKey> {
  private final Comparator<Object> objectComparator;

  private final int[] directions;

  public OrderedMultiValueKey(
          Storage<?>[] storages, int rowIndex, int[] directions) {
    this(storages, rowIndex, directions, ObjectComparator.DEFAULT);
  }

  public OrderedMultiValueKey(
      Storage<?>[] storages, int rowIndex, int[] directions, Comparator<Object> objectComparator) {
    super(storages, rowIndex);
    this.objectComparator = objectComparator;
    this.directions = directions;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof MultiValueKeyBase that)) return false;
    if (storages.length != that.storages.length) return false;
    for (int i = 0; i < storages.length; i++) {
      if (objectComparator.compare(get(i), that.get(i)) != 0) {
        return false;
      }
    }

    return true;
  }

  @Override
  public int compareTo(OrderedMultiValueKey that) {
    if (objectComparator == null || that == null) {
      throw new NullPointerException();
    }

    if (that.storages.length != storages.length) {
      throw new ClassCastException("Incomparable keys.");
    }

    for (int i = 0; i < storages.length; i++) {
      int comparison = objectComparator.compare(get(i), that.get(i));
      if (comparison != 0) {
        return comparison * directions[i];
      }
    }

    return 0;
  }

  @Override
  public int hashCode() {
    throw new IllegalStateException(
        "Currently no hash_code implementation consistent with the ObjectComparator is exposed, so"
            + " OrderedMultiValueKey is not hashable.");
  }
}
