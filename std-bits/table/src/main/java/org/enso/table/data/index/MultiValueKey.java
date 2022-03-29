package org.enso.table.data.index;

import java.util.Arrays;

public class MultiValueKey {
  private final Object[] values;
  private final int hashCodeValue;
  private final boolean allNull;

  public MultiValueKey(Object[] values) {
    this.values = values;

    boolean allNull = true;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 0;
    for (Object value: this.values) {
      if (value != null) {
        h ^= value.hashCode();
        allNull = false;
      }
    }
    h += ~(h << 9);
    h ^= h >>> 14;
    h += h << 4;

    this.hashCodeValue = h ^ (h >>> 10);
    this.allNull = allNull;
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
}
