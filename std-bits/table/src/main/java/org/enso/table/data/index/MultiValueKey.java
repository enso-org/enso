package org.enso.table.data.index;

import org.enso.table.data.column.storage.Storage;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Objects;

public class MultiValueKey implements Comparable<MultiValueKey> {
  private final Storage[] storage;
  private final int[] directions;
  private final int rowIndex;
  private final Comparator<Object> objectComparator;
  private final int hashCodeValue;
  private final boolean allNull;
  private final boolean floatValue;

  public MultiValueKey(Storage[] storage, int rowIndex, Comparator<Object> objectComparator) {
    this(storage, rowIndex, null, objectComparator);
  }

  public MultiValueKey(
      Storage[] storage, int rowIndex, int[] directions, Comparator<Object> objectComparator) {
    this.storage = storage;
    this.rowIndex = rowIndex;

    if (directions == null) {
      directions = new int[storage.length];
      Arrays.fill(directions, 1);
    }
    this.directions = directions;

    this.objectComparator = objectComparator;

    boolean allNull = true;
    boolean floatValue = false;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 1;
    for (int i = 0; i < storage.length; i++) {
      h = 31 * h;

      Object value = this.get(i);
      if (value != null) {
        Object folded = foldObject(value);
        floatValue = floatValue || (folded instanceof Double);
        h += folded.hashCode();
        allNull = false;
      }
    }

    this.hashCodeValue = h;
    this.allNull = allNull;
    this.floatValue = floatValue;
  }

  public Object get(int column) {
    return storage[column].getItemBoxed(rowIndex);
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof MultiValueKey that)) return false;
    if (storage.length != that.storage.length) return false;
    if (hashCodeValue != that.hashCodeValue) return false;
    for (int i = 0; i < storage.length; i++) {
      if (objectComparator.compare(get(i), that.get(i)) != 0) {
        return false;
      }
    }

    return true;
  }

  public boolean areAllNull() {
    return allNull;
  }

  public boolean hasFloatValues() {
    return floatValue;
  }

  protected static Object foldObject(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer) {
      return ((Integer) value).longValue();
    } else if (value instanceof Byte) {
      return ((Byte) value).longValue();
    } else if (value instanceof Float && ((Float) value) % 1 == 0) {
      return ((Float) value).longValue();
    } else if (value instanceof Double && ((Double) value) % 1 == 0) {
      return ((Double) value).longValue();
    } else if (value instanceof Float) {
      return ((Float) value).doubleValue();
    } else if (value instanceof Double) {
      return value;
    }

    return value;
  }

  @Override
  public int compareTo(MultiValueKey that) {
    if (objectComparator == null || that == null) {
      throw new NullPointerException();
    }

    if (that.storage.length != storage.length) {
      throw new ClassCastException("Incomparable keys.");
    }

    for (int i = 0; i < storage.length; i++) {
      int comparison = objectComparator.compare(get(i), that.get(i));
      if (comparison != 0) {
        return comparison * directions[i];
      }
    }

    return 0;
  }
}
