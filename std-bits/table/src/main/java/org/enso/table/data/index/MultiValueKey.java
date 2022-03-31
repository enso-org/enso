package org.enso.table.data.index;

import java.util.Arrays;

public class MultiValueKey {
  private final Object[] values;
  private final int hashCodeValue;
  private final boolean allNull;
  private final boolean floatValue;

  public MultiValueKey(Object[] values) {
    this.values = values;

    boolean allNull = true;
    boolean floatValue = false;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 0;
    for (Object value: this.values) {
      if (value != null) {
        Object folded = FoldObject(value);
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

  public boolean hasFloatValues() { return floatValue; }

  protected static Object FoldObject(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer) {
      return ((Integer)value).longValue();
    } else if (value instanceof Byte) {
      return ((Byte)value).longValue();
    } else if (value instanceof Float && ((Float)value) % 1 == 0) {
      return ((Float)value).longValue();
    } else if (value instanceof Double && ((Double)value) % 1 == 0) {
      return ((Double)value).longValue();
    } else if (value instanceof Float) {
      return ((Float)value).doubleValue();
    } else if (value instanceof Double) {
      return value;
    }

    return value;
  }
}
