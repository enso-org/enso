package org.enso.table.data.index;

import java.util.Arrays;
import java.util.Comparator;

public class MultiValueKey implements Comparable<MultiValueKey> {
  private final Object[] values;
  private final Comparator<Object> objectComparator;
  private final int hashCodeValue;
  private final boolean allNull;
  private final boolean floatValue;

  public MultiValueKey(Object[] values) {
    this(values, null);
  }

  public MultiValueKey(Object[] values, Comparator<Object> objectComparator) {
    this.values = values;
    this.objectComparator = objectComparator;

    boolean allNull = true;
    boolean floatValue = false;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 0;
    for (Object value : this.values) {
      if (value != null) {
        Object folded = foldObject(value);
        floatValue = floatValue || (folded instanceof Double);
        h ^= folded.hashCode();
        allNull = false;
      }
    }
    h += ~(h << 9);
    h ^= h >>> 14;
    h += h << 4;

    this.hashCodeValue = h ^ (h >>> 10);
    this.allNull = allNull;
    this.floatValue = floatValue;
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MultiValueKey that = (MultiValueKey) o;
    return hashCodeValue == that.hashCodeValue && Arrays.equals(values, that.values);
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

    if (that.values.length != values.length) {
      throw new ClassCastException("Incomparable keys.");
    }

    for (int i = 0; i < values.length; i++) {
      int comparison = objectComparator.compare(values[i], that.values[i]);
      if (comparison != 0) {
        return comparison;
      }
    }

    return 0;
  }
}
